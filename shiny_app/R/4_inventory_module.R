inventory_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = 'Inventory and Sales Mix',
        uiOutput(ns("vendor_info_ui"), inline = TRUE),
        DT::DTOutput(ns('inventory_table')) %>%
          shinycustomloader::withLoader()
      )
    )
  )
}

inventory_module <- function(input, output, session, vendor_info) {
  ns <- session$ns

  inventory <- reactive({

    id <- notify("Loading Inventory from Database...")
    on.exit(removeNotification(id), add = TRUE)

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

  inventory_prep <- reactive({
    req(inventory())

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

    inventory() %>%
      # mutate(vendor_name = vendor_info()$vendor_name) %>%
      select(-c(uid:vendor_uid, created_at:modified_by)) %>%
      # select(vendor_name, everything()) %>%
      # add action bttns
      tibble::add_column(" " = actions, .before = 1)
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
          list(targets = 0, orderable = FALSE, width = "75px"),
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


}

# Copy in UI
#inventory_module_ui("inventory_module_ui")

# Copy in server
#callModule(inventory_module_, "inventory_module_ui")
