inventory_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = icon_text("boxes", "Inventory and Sales Mix"),
        footer = "Powwater | Tychobra 2021",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("vendor_info_ui"), inline = TRUE),
        hr(),
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
          shinycustomloader::withLoader()
      )
    ),
    htmltools::tags$script(src = "inventory_module.js"),
    htmltools::tags$script(paste0("inventory_module_js('", ns(''), "')"))
  )
}

inventory_module <- function(input, output, session, vendor_info) {
  ns <- session$ns

  # trigger to reload data from the "vendors" table
  session$userData$inventory_trigger <- reactiveVal(0)

  inventory <- reactive({

    id <- notify("Loading Inventory from Database...")
    on.exit(shinyFeedback::hideToast(), add = TRUE)

    session$userData$inventory_trigger()

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- conn %>%
        dplyr::tbl("vendor_inventory") %>%
        dplyr::filter(vendor_uid == vend) %>%
        dplyr::collect()

    }, error = function(err) {
      msg <- 'Error collecting data from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  output$vendor_info_ui <- renderUI({
    req(inventory(), vendor_info())

    sales_mix <- inventory() %>%
      mutate(sales_mix = paste0(offer_type, " - ", capacity, " Liters: ", price_per_unit, " KES")) %>%
      pull(sales_mix) %>%
      unique() %>%
      paste(collapse = "; ")

    n_offerings <- nrow(inventory())

    txt <- paste0("Currently offering ", n_offerings, " various options: ", sales_mix)

    div(
      h3(paste0(vendor_info()$vendor_name, " - Offerings:")),
      h5(paste0("Sales Mix: ", txt))
    )
  })

  inventory_prep <- reactiveVal(NULL)

  observeEvent(inventory(), {
    ids <- inventory()$uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Inventory Buttons">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id="', id_,'" style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id="', id_,'" style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })

    out <- inventory() %>%
      select(-c(uid:vendor_uid, created_at:modified_by)) %>%
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

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1)
    id <- session$ns("inventory_table")

    dt_js <- paste0(
      "function(settings, json) {
        var filters = $('#", id, " td .form-group');
        // columns that should have visible filters
        var cols = [", paste(2:n_col, collapse = ", "), "];
        // hide certain column filters
        for (var i = 1; i <= filters.length; i++) {
          if (!cols.includes(i))
            filters.eq(i - 1).css('visibility', 'hidden');
          filters.eq(i - 1).css('position', 'static');
        }
      }")

    DT::datatable(
      out,
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      style = "bootstrap",
      class = 'table table-striped table-bordered dt-center compact hover',
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "none",
      options = list(
        scrollX = TRUE,
        dom = '<Bf>tip',
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "45px"),
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "vendor-inventory-table", esc_cols),
        initComplete = DT::JS(dt_js),
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
      select(capacity, offer_type) %>%
      mutate(offering = paste0(offer_type, ": ", capacity, " Liters")) %>%
      pull(offering)
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
            shinyWidgets::pickerInput(ns("capacity"),
                                      icon_text("box", "Capacity in Liters:"),
                                      choices = choices$inventory_capacity,
                                      selected = if (is.null(hold)) choices$inventory_capacity[2] else hold$capacity),
            shinyWidgets::pickerInput(ns("offer_type"),
                                      icon_text("archive", "Order Type Offered:"),
                                      choices = choices$inventory_offer_type,
                                      selected = if (is.null(hold)) choices$inventory_offer_type[1] else hold$offer_type),
            shiny::numericInput(ns("price_per_unit"),
                                icon_text("money", "Price per Unit (KES):"),
                                value = if (is.null(hold)) "" else hold$price_per_unit,
                                min = 1,
                                step = 25),
            shiny::numericInput(ns("quantity"),
                                icon_text("sort-amount-up-alt", "Quantity:"),
                                value = if (is.null(hold)) "" else hold$quantity,
                                min = 0,
                                step = 1)
          ),
          div(
            id = ns("danger"),
            class = "dangerdiv",
            span(icon("exclamation-circle"), p("Offering already available."))
          ) %>% shinyjs::hidden()
        )
      )
    )

    observeEvent(input$capacity, {
      if (input$capacity == "") {
        shinyFeedback::showFeedbackDanger(
          "capacity",
          text = "Capacity cannot be blank."
        )
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("capacity")
        shinyjs::enable("submit")
      }
    })

    observeEvent(input$offer_type, {

      if (input$offer_type == "") {
        shinyFeedback::showFeedbackDanger(
          "offer_type",
          text = "Offer Type cannot be blank."
        )
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("offer_type")
        shinyjs::enable("submit")
      }
    })

    observeEvent(input$price_per_unit, {
      if (is.na(input$price_per_unit) || input$price_per_unit <= 0) {
        shinyFeedback::showFeedbackDanger(
          "price_per_unit",
          text =  "Price cannot be blank."
        )
        shinyjs::disable("submit")
      }  else {
        shinyFeedback::hideFeedback("price_per_unit")
        shinyjs::enable("submit")
      }
    })

    observeEvent(input$quantity, {
      if (is.na(input$quantity)) {
        shinyFeedback::showFeedbackDanger(
          "quantity",
          text = "Quantity cannot be blank."
        )
        shinyjs::disable("submit")
      } else {
        shinyFeedback::hideFeedback("quantity")
        shinyjs::enable("submit")
      }
    })

    observe({
      req(is.null(hold), input$capacity, input$offer_type)
      # browser()
      offer <- paste0(input$offer_type, ": ", input$capacity, " Liters") %>%
        stringr::str_to_title()
      if (offer %in% offerings()) {
        shinyjs::show("danger")
        shinyjs::disable("submit")
      } else {
        shinyjs::hide("danger")
        shinyjs::enable("submit")
      }
    })

  })

  edit_inventory_dat <- reactive({

    hold <- vendor_inventory_to_edit()

    out <- list(
      uid = if (is.null(hold)) NA else hold$uid,
      data = list(
        vendor_uid = vendor_info()$vendor_uid,
        capacity = input$capacity,
        offer_type = stringr::str_to_title(input$offer_type),
        price_per_unit = input$price_per_unit,
        quantity = input$quantity
      )
    )

    if (is.null(hold)) {
      out$data$created_at = tychobratools::time_now_utc()
      out$data$created_by = session$userData$user()$user_uid
    } else {
      out$data$created_at <- as.character(hold$created_at)
      out$data$created_by <- hold$created_by
    }

    out$data$modified_at = tychobratools::time_now_utc()
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
            price_per_unit,
            quantity,
            created_at,
            created_by,
            modified_at,
            modified_by)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)',
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
            price_per_unit=$4,
            quantity = $5,
            created_at=$6,
            created_by=$7,
            modified_at=$8,
            modified_by=$9 WHERE uid=$10",
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

      session$userData$vendor_inventory_trigger(
        session$userData$vendor_inventory_trigger() + 1
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
