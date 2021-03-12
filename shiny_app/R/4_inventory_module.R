inventory_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = 'Inventory and Sales Mix',
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
    htmltools::tags$script(paste0("inventory_module_js('", ns(''), "')")),
  )
}

inventory_module <- function(input, output, session, vendor_info) {
  ns <- session$ns

  # trigger to reload data from the "vendors" table
  session$userData$inventory_trigger <- reactiveVal(0)

  inventory <- reactive({

    id <- notify("Loading Inventory from Database...")
    on.exit(removeNotification(id), add = TRUE)

    session$userData$inventory_trigger()

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- conn %>%
        dplyr::tbl("vendor_offerings") %>%
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
      class = 'dt-center stripe cell-border display',
      # Escape the HTML
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "top",
      options = list(
        autowidth = TRUE,
        # scrollX = TRUE,
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

  shiny::callModule(
    inventory_edit_module,
    "add_offering",
    vendor_inventory_to_edit = function() NULL,
    trigger = shiny::reactive({input$add_offering}),
    vendor_info = vendor_info
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
    vendor_info = vendor_info
  )

  vendor_inventory_to_delete <- eventReactive(input$vendor_inventory_id_to_delete, {
    inventory() %>%
      filter(uid == input$vendor_inventory_id_to_delete) %>%
      as.list()
  })

  # shiny::callModule(
  #   inventory_delete_module,
  #   "delete_offering",
  #   vendor_to_delete = vendor_to_delete,
  #   trigger = shiny::reactive({input$vendor_id_to_delete})
  # )

}

# Copy in UI
#inventory_module_ui("inventory_module_ui")

# Copy in server
#callModule(inventory_module_, "inventory_module_ui")


# edit_module -------------------------------------------------------------

inventory_edit_module <- function(input, output, session,
                                  vendor_inventory_to_edit,
                                  trigger = vendor_inventory_to_edit,
                                  vendor_info) {

  ns <- session$ns

  shiny::observeEvent(trigger(), {

    hold <- vendor_inventory_to_edit()

    shiny::showModal(
      shiny::modalDialog(
        title = "Add/Edit Vendor Inventory and Offerings:",
        size = 'l',
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
            shinyWidgets::textInputAddon(ns("capacity"), "Capacity in Liters:", value = if (is.null(hold)) "10L" else paste0(hold$capacity, "L"), placeholder = "1.5L, 10L, 20L", addon = icon("box")),
            shinyWidgets::radioGroupButtons(ns("offer_type"), "Order Type:", choices = c("New", "Swap"), selected = if (is.null(hold)) "New" else hold$offer_type),
            shinyWidgets::currencyInput(ns("price_per_unit"), "Price per Unit:", value = if (is.null(hold)) 100 else hold$price_per_unit,
                                        format = "dotDecimalCharCommaSeparator")
          )
        )
      )
    )
  })

  edit_inventory_dat <- reactive({

    hold <- vendor_inventory_to_edit()

    out <- list(
      uid = if (is.null(hold)) NA else hold$uid,
      data = list(
        vendor_uid = vendor_info()$vendor_uid,
        capacity = as.numeric(substr(input$capacity, 1L, nchar(input$capacity) - 1)),
        offer_type = input$offer_type,
        price_per_unit = input$price_per_unit
      )
    )

    if (is.null(hold)) {
      out$data$created_at = tychobratools::time_now_utc()
      out$data$created_by = session$userData$user()$email
    } else {
      out$data$created_at <- as.character(hold$created_at)
      out$data$created_by <- hold$created_by
    }

    out$data$modified_at = tychobratools::time_now_utc()
    out$data$modified_by = session$userData$user()$email

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
        # creating a new car

        dbExecute(
          conn,
          'INSERT INTO vendor_offerings (
            vendor_uid,
            capacity,
            offer_type,
            price_per_unit,
            created_at,
            created_by,
            modified_at,
            modified_by)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8)',
          params = c(
            unname(dat$data)
          )
        )
      } else {
        # editing an existing car
        dbExecute(
          conn,
          "UPDATE vendor_offerings SET
            vendor_uid=$1,
            capacity=$2,
            offer_type=$3,
            price_per_unit=$4,
            created_at=$5,
            created_by=$6,
            modified_at=$7,
            modified_by=$8 WHERE uid=$9",
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
