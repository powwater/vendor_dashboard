inventory_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = icon_text("boxes", "Inventory"),
        status = "primary",
        solidHeader = TRUE,
        shiny::actionButton(
          ns("add_offering"),
          "Add New Offering",
          class = "btn-success",
          style = "color: #fff; margin-botton: -30px;",
          icon = shiny::icon("plus")
        ),
        br(),
        br(),
        DT::DTOutput(ns('inventory_table')) %>%
          shinycssloaders::withSpinner()
      )
    ),
    shiny::tags$script(src = "inventory_module.js"),
    shiny::tags$script(paste0("inventory_module_js('", ns(''), "')"))
  )
}

inventory_module <- function(input, output, session, vendor_info, is_mobile) {
  ns <- session$ns

  # trigger to reload data from the "vendors" table
  session$userData$inventory_trigger <- reactiveVal(0)

  inventory <- reactive({

    # id <- notify("Loading Inventory from Database...")
    # on.exit(shinyFeedback::hideToast(), add = TRUE)

    session$userData$inventory_trigger()

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- conn %>%
        dplyr::tbl("vendor_inventory") %>%
        dplyr::filter(vendor_uid == vend) %>%
        dplyr::collect()

    }, error = function(err) {
      msg <- 'Error collecting inventory from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })


  inventory_prep <- reactiveVal(NULL)

  observeEvent(inventory(), {

    hold_inventory <- inventory() %>%
      arrange(capacity, offer_type, bottle_type)

    ids <- hold_inventory$uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Inventory Buttons">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id="', id_,'" style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id="', id_,'" style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })

    out <- hold_inventory %>%
      select(capacity, offer_type, bottle_type, price_per_unit, quantity) %>%
      tibble::add_column(" " = actions, .before = 1)

    if (is.null(inventory_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      inventory_prep(out)
    } else {
      # table has already rendered, so use DT proxy to update the data in the
      # table without re-rendering the entire table
      replaceData(inventory_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }
  })

  output$inventory_table <- DT::renderDT({
    req(inventory_prep())

    out <- inventory_prep()

    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1)


    if (isTRUE(is_mobile())) {
      tbl_class <- "table table-striped table-bordered dt-center dt-responsive dt-compact dt-hover nowrap table"
      tbl_exts <- c("Buttons", "Responsive")
      tbl_filt <- "none"
      tbl_scroll <- FALSE
    } else {
      tbl_class <- "table table-striped table-bordered dt-center dt-compact dt-hover nowrap table"
      tbl_exts <- c("Buttons")
      tbl_filt <- "top"
      tbl_scroll <- TRUE
    }

    DT::datatable(
      out,
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      style = "bootstrap",
      class = tbl_class,
      escape = esc_cols,
      extensions = tbl_exts,
      filter = "none",
      options = list(
        scrollX = tbl_scroll,
        dom = "<'row'<'col-sm-3'l><'col-sm-6 text-center'B><'col-sm-3'f>>
               <'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i><'col-sm-7'p>>",
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "45px"),
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "vendor-inventory-table", esc_cols),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }")
      )
    ) %>%
      formatCurrency("price_per_unit") %>%
      formatString("capacity", suffix = " Liters")
  })

  inventory_table_proxy <- DT::dataTableProxy("inventory_table")

  offerings <- reactive({
    inventory() %>%
      select(capacity, offer_type, bottle_type)
  })

  shiny::callModule(
    inventory_edit_module,
    "add_offering",
    vendor_inventory_to_edit = function() NULL,
    trigger = shiny::reactive({input$add_offering}),
    vendor_info = vendor_info,
    offerings = offerings
  )

  # vendor to edit (triggered by DT edit button)
  vendor_inventory_to_edit <- eventReactive(input$vendor_inventory_id_to_edit, {
    inventory() %>%
      filter(uid == input$vendor_inventory_id_to_edit)
  })

  shiny::callModule(
    inventory_edit_module,
    "edit_offering",
    vendor_inventory_to_edit = vendor_inventory_to_edit,
    trigger = reactive({input$vendor_inventory_id_to_edit}),
    vendor_info = vendor_info,
    offerings = offerings
  )

  vendor_inventory_to_delete <- eventReactive(input$vendor_inventory_id_to_delete, {
    inventory() %>%
      filter(uid == input$vendor_inventory_id_to_delete) %>%
      as.list()
  })

  shiny::callModule(
    inventory_delete_module,
    "delete_offering",
    vendor_inventory_to_delete = vendor_inventory_to_delete,
    trigger = shiny::reactive({input$vendor_inventory_id_to_delete})
  )

}

# Copy in UI
#inventory_module_ui("inventory_module_ui")

# Copy in server
#callModule(inventory_module_, "inventory_module_ui")


# edit_module -------------------------------------------------------------

inventory_edit_module <- function(input, output, session,
                                  vendor_inventory_to_edit,
                                  trigger = vendor_inventory_to_edit,
                                  vendor_info,
                                  offerings) {

  ns <- session$ns

  shiny::observeEvent(trigger(), {

    hold <- vendor_inventory_to_edit()

    if (!is.null(hold)) {
      if (hold$offer_type == "Refill") {
        capacity_choices <- c(
          "10L" = 10,
          "20L" = 20.0
        )

        bottle_type_choices <- ""

        offer_type_choices <- choices$inventory_offer_type

      } else {
        if (hold$capacity == 18.9) {
          capacity_choices <- choices$inventory_capacity
          bottle_type_choices <- "Dispenser"
          offer_type_choices <- c("New", "Swap")

        } else if (hold$capacity == 20) {
          capacity_choices <- choices$inventory_capacity
          bottle_type_choices <- c("Jerrycan", "PET")
          offer_type_choices <- choices$inventory_offer_type
        } else {
          capacity_choices <- choices$inventory_capacity
          bottle_type_choices <- choices$inventory_bottle_type
          offer_type_choices <- choices$inventory_offer_type
        }
      }
    }


    shiny::showModal(
      shiny::modalDialog(
        title = "Add/Edit Vendor Inventory and Offerings:",
        size = 's',
        footer = list(
          shiny::modalButton('Cancel'),
          shiny::actionButton(
            ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        ),
        fluidRow(
          column(
            width = 12,
            shinyWidgets::pickerInput(
              ns("capacity"),
              icon_text("box", "Capacity in Liters:"),
              choices = if (is.null(hold)) choices$inventory_capacity else capacity_choices,
              selected = if (is.null(hold)) choices$inventory_capacity[1] else hold$capacity
            ),
            shinyWidgets::pickerInput(
              ns("offer_type"),
              icon_text("archive", "Order Type Offered:"),
              choices = if (is.null(hold)) choices$inventory_offer_type else offer_type_choices,
              selected = if (is.null(hold)) choices$inventory_offer_type[1] else hold$offer_type
            ),
            shinyWidgets::pickerInput(
              ns("bottle_type"),
              icon_text("wine-bottle", "Bottle Type Offered:"),
              choices = if (is.null(hold)) choices$inventory_bottle_type else bottle_type_choices,
              selected = if (is.null(hold)) choices$inventory_bottle_type[2] else hold$bottle_type
            ),
            shiny::numericInput(
              ns("price_per_unit"),
              icon_text("money", "Price per Unit (KES):"),
              value = if (is.null(hold)) "" else hold$price_per_unit,
              min = 1,
              step = 25
            ),
            shiny::numericInput(
              ns("quantity"),
              icon_text("sort-amount-up-alt", "Quantity:"),
              value = if (is.null(hold)) "" else hold$quantity,
              min = 0,
              step = 1
            )
          ),
          div(
            id = ns("danger"),
            class = "dangerdiv",
            span(icon("exclamation-circle"), p("Offering already available."))
          ) %>% shinyjs::hidden()
        )
      )
    )

    ## Inventory Validation ##
    observeEvent(input$price_per_unit, {

      if (is.na(input$price_per_unit) || input$price_per_unit <= 0) {
        shinyFeedback::showFeedbackDanger(
          "price_per_unit",
          text =  "Price cannot be blank.",
          icon = shiny::icon("ban", lib = "font-awesome")
        )
        shinyjs::disable("submit")
      }  else {
        shinyFeedback::hideFeedback("price_per_unit")
      }
    })

    observeEvent(input$quantity, {

      if (is.na(input$quantity)) {
        shinyFeedback::showFeedbackDanger(
          "quantity",
          text = "Quantity cannot be blank.",
          icon = shiny::icon("ban", lib = "font-awesome")
        )
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("quantity")
      }
    })

    observeEvent(c(
      input$offer_type,
      input$capacity,
      input$bottle_type,
      input$price_per_unit,
      input$quantity
    ), {

      hold_offer_type <- input$offer_type
      hold_capacity <- input$capacity
      hold_bottle_type <- input$bottle_type

      ### Update Picker Choices ###
      if (input$capacity == 20.0) {
        if (input$offer_type == "Refill") {
          shinyWidgets::updatePickerInput(
            session,
            'capacity',
            choices = c(
              "10L" = 10,
              "20L" = 20.0
            ),
            selected = 20.0
          )

          shinyWidgets::updatePickerInput(
            session,
            'bottle_type',
            choices = ""
          )

          shinyjs::hide("bottle_type")
        } else {
          shinyWidgets::updatePickerInput(
            session,
            'capacity',
            choices = choices$inventory_capacity,
            selected = 20.0
          )

          shinyWidgets::updatePickerInput(
            session,
            'bottle_type',
            choices = c("Jerrycan", "PET"),
            selected = if (!(hold_bottle_type %in% c("Dispenser", ""))) hold_bottle_type else "PET"
          )

          shinyjs::show("bottle_type")

          shinyWidgets::updatePickerInput(
            session,
            "offer_type",
            choices = choices$inventory_offer_type,
            selected = hold_offer_type
          )
        }

      } else if (input$capacity == 18.9) {
        shinyWidgets::updatePickerInput(
          session,
          'bottle_type',
          choices = "Dispenser"
        )

        shinyWidgets::updatePickerInput(
          session,
          "offer_type",
          choices = c("New", "Swap"),
          selected = if (hold_offer_type != "Refill") hold_offer_type else "New"
        )
      } else {
        if (input$offer_type == "Refill") {
          shinyWidgets::updatePickerInput(
            session,
            'capacity',
            choices = c(
              "10L" = 10,
              "20L" = 20.0
            ),
            selected = 10
          )

          shinyWidgets::updatePickerInput(
            session,
            'bottle_type',
            choices = ""
          )

          shinyjs::hide("bottle_type")
        } else {

          shinyWidgets::updatePickerInput(
            session,
            'capacity',
            choices = choices$inventory_capacity,
            selected = 10
          )

          shinyWidgets::updatePickerInput(
            session,
            'bottle_type',
            choices = choices$inventory_bottle_type,
            selected = if (hold_bottle_type != "") hold_bottle_type else choices$inventory_bottle_type[2]
          )

          shinyjs::show("bottle_type")

          shinyWidgets::updatePickerInput(
            session,
            "offer_type",
            choices = choices$inventory_offer_type,
            selected = hold_offer_type
          )
        }

      }

      ### Check for Existing Inventory ###

      # Add: Can't already exist
      if (is.null(hold)) {
        hold_offerings <- offerings()

      # Edit: Remove existing entry
      } else {

        #' NO `Bottle Type` for `Refill` offering
        if (input$offer_type == "Refill") {
          hold_offerings <- offerings() %>%
            select(-bottle_type) %>%
            filter(
              .data$offer_type != hold$offer_type |
                .data$capacity != hold$capacity
            )
        } else {
          hold_offerings <- offerings() %>%
            filter(
              .data$offer_type != hold$offer_type |
                .data$capacity != hold$capacity |
                .data$bottle_type != hold$bottle_type
            )
        }
      }

      existing_offering <- hold_offerings %>%
        filter(
          .data$offer_type == input$offer_type,
          .data$capacity == input$capacity
        )

      #' NO `Bottle Type` for `Refill` offering
      if (input$offer_type != "Refill" && nrow(existing_offering) > 0) {
        existing_offering <- existing_offering %>%
          filter(.data$bottle_type == input$bottle_type)
      }

      if (nrow(existing_offering) > 0) {
        shinyjs::show("danger")
        shinyjs::disable("submit")
      } else {
        shinyjs::hide("danger")

        #' Validate `price` & `quantity`
        if (!is.na(input$price_per_unit) && !is.na(input$quantity)) {
          shinyjs::enable("submit")
        } else {
          shinyjs::disable("submit")
        }
      }

    }, ignoreInit = TRUE)
  })

  edit_inventory_dat <- reactive({
    hold <- vendor_inventory_to_edit()

    out <- list(
      uid = if (is.null(hold)) NA else hold$uid,
      data = list(
        vendor_uid = vendor_info()$vendor_uid,
        capacity = input$capacity,
        offer_type = input$offer_type,
        bottle_type = if (input$offer_type == "Refill") '' else input$bottle_type,
        price_per_unit = input$price_per_unit,
        quantity = input$quantity
      )
    )

    if (is.null(hold)) {
      out$data$created_at = time_now_utc()
      out$data$created_by = session$userData$user()$user_uid
    } else {
      out$data$created_at <- as.character(hold$created_at)
      out$data$created_by <- hold$created_by
    }

    out$data$modified_at = time_now_utc()
    out$data$modified_by = session$userData$user()$user_uid

    out
  })

  validate_edit <- eventReactive(input$submit, {
    dat <- edit_inventory_dat()
    # TODO: add validation to inputs
    dat
  })

  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()

    tryCatch({

      if (is.na(dat$uid)) {
        dbExecute(
          conn,
          'INSERT INTO vendor_inventory (
            vendor_uid,
            capacity,
            offer_type,
            bottle_type,
            price_per_unit,
            quantity,
            created_at,
            created_by,
            modified_at,
            modified_by)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)',
          params = c(
            unname(dat$data)
          )
        )
      } else {
        dbExecute(
          conn,
          "UPDATE vendor_inventory SET
            vendor_uid=$1,
            capacity=$2,
            offer_type=$3,
            bottle_type=$4,
            price_per_unit=$5,
            quantity = $6,
            created_at=$7,
            created_by=$8,
            modified_at=$9,
            modified_by=$10 WHERE uid=$11",
          params = c(
            unname(dat$data),
            list(dat$uid)
          )
        )
      }
      session$userData$inventory_trigger(session$userData$inventory_trigger() + 1)
      vendor_name <- vendor_info()$vendor_name
      msg <- paste0("Vendor ", vendor_name, " inventory successfully edited!")
      shinyFeedback::showToast("success", msg)
    }, error = function(err) {
      msg <- 'Error updating vendor_offerings database'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })
  })
}

inventory_delete_module <- function(input, output, session,
                                    vendor_inventory_to_delete,
                                    trigger) {

  ns <- session$ns

  observeEvent(trigger(), {

    showModal(
      modalDialog(
        div(
          style = "padding: 30px;",
          class = "text-center",
          h2(
            style = "line-height: 1.75;",
            'Are you sure you want to delete this offering?'
          )
        ),
        br(),
        title = "Delete Vendor Offering",
        size = "m",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("delete_button"),
            "Delete Offering",
            class = "btn-danger",
            style = "color: #FFF;"
          )
        )
      )
    )
  })

  observeEvent(input$delete_button, {
    req(vendor_inventory_to_delete())
    hold_vendor_inventory <- vendor_inventory_to_delete()
    removeModal()


    tryCatch({

      uid <- hold_vendor_inventory$uid

      DBI::dbExecute(
        conn,
        "DELETE FROM vendor_inventory WHERE uid=$1",
        params = c(uid)
      )

      session$userData$inventory_trigger(
        session$userData$inventory_trigger() + 1
      )

      shinyFeedback::showToast("success", "Vendor Inventory/Offering Successfully Deleted")

    }, error = function(err) {

      msg <- "Error Deleting Vendor Offering"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)
    })
  })
}
