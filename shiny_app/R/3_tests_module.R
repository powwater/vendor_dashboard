tests_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = 'Water Quality Tests',
        DT::DTOutput(ns('tests_table')) %>%
          shinycustomloader::withLoader(),
        hr()
      )
    )
  )
}

tests_module <- function(input, output, session, vendor_info){
  ns <- session$ns

  tests <- reactive({

    id <- notify("Loading Tests from Database...")
    on.exit(removeNotification(id), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- conn %>%
        dplyr::tbl("vendor_tests") %>%
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

  tests_prep <- reactive({
    req(tests())

    ids <- tests()$uid

    # Edit/Delete buttons
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 105px;" role="group" aria-label="Tests Buttons">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id="', id_,'" style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id="', id_,'" style="margin: 0"><i class="fa fa-trash-o"></i></button>
          <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Test" id="', id_,'" style="margin: 0"><i class="fas fa-id-card"></i></button>
        </div>'
      )
    })

    tests() %>%
      select(-uid, -vendor_uid, -c(created_at:modified_by)) %>%
      select(test_date, ph_value, tds, test_date_tbc, tbc) %>%
      # add action bttns
      tibble::add_column(" " = actions, .before = 1)
  })

  output$tests_table <- DT::renderDT({

    req(tests_prep())

    out <- tests_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- c(" ", "Test Date", "PH Value", "TDS", "Test Date TDC", "TBC")
    esc_cols <- c(-1)
    id <- session$ns("tests_table")

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
      class = 'dt-center stripe cell-border display compact nowrap',
      # Escape the HTML
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "top",
      options = list(
        autoWidth = TRUE,
        # scrollX = TRUE,
        dom = '<Bf>tip',
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "105px"),
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "tests-table", esc_cols),
        initComplete = DT::JS(dt_js),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }"),
        language = list(
          emptyTable = "No Tests for Selected Filters"
        )
      )
    ) %>%
      DT::formatRound("ph_value", digits = 2)

  })

}

# Copy in UI
#vendor_tests_ui("vendor_tests_ui")

# Copy in server
#callModule(vendor_tests_, "vendor_tests_ui")
