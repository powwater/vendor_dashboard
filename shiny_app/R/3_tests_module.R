tests_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = icon_text("vial", 'Water Quality Tests'),
        footer = "Powwater | Tychobra 2021",
        status = "primary",
        solidHeader = TRUE,
        br(),
        br(),
        h4("Vendor Water Quality Test Results:"),
        fluidRow(
          column(
            offset = 2,
            width = 8,
            DT::DTOutput(ns('tests_table')) %>%
              shinycustomloader::withLoader()
          )
        ),
        br(),
        hr(),
        h4("Rating Tiers for Water Quality Tests:"),
        fluidRow(
          column(
            offset = 4,
            width = 4,
            DT::DTOutput(ns("tests_tiers_table")) %>%
              shinycustomloader::withLoader()
          )
        )
      )
    )
  )
}

tests_module <- function(input, output, session, vendor_info, is_mobile) {
  ns <- session$ns

  tests_tiers <- tribble(
    ~min_tds, ~max_tds, ~rating, ~html,
    0,        200,      5,       '<div class=rating-tint><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span></div>',
    201,      350,      4.5,     '<div class=rating-tint><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint half"></span></div>',
    351,      750,      4,       '<div class=rating-tint><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span></div>',
    751,      1500,     3.5,     '<div class=rating-tint><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint full"></span><span class="fa fa-tint half"></span></div>'
  ) %>%
    dplyr::transmute(
      range_tds = paste0(min_tds, " to ", max_tds, " TDS"),
      rating = paste0(rating, " / 5"),
      rating_html = html
    )


  output$tests_tiers_table <- DT::renderDT({

    out <- tests_tiers

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- c("Range (TDS)", "Rating", " ")
    esc_cols <- c(-3)
    id <- session$ns("tests_tiers_table")

    if (isTRUE(is_mobile())) {
      tbl_class <- "table table-striped table-bordered dt-center dt-responsive dt-compact dt-hover nowrap table"
    } else {
      tbl_class <- "table table-striped table-bordered dt-center dt-compact dt-hover nowrap table"
    }

    DT::datatable(
      out,
      rownames = FALSE,
      colnames = cols,
      selection = "none",
      style = "bootstrap",
      class = tbl_class,
      escape = esc_cols,
      options = list(
        dom = 't'
      )
    )
  })

  tests <- reactive({

    # id <- notify("Loading Tests from Database...")
    # on.exit(shinyFeedback::hideToast(), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- conn %>%
        dplyr::tbl("vendor_tests") %>%
        dplyr::filter(vendor_uid == vend) %>%
        dplyr::collect()

    }, error = function(err) {
      msg <- 'Error collecting tests from database.'
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
        '<div class="btn-group" style="width: 35px;" role="group" aria-label="Tests Buttons">
          <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Test" id="', id_,'" style="margin: 0"><i class="fas fa-id-card"></i></button>
        </div>'
      )
    })

    tests() %>%
      select(-uid, -vendor_uid, -c(created_at:modified_by)) %>%
      mutate(test_date_tbc = test_date, tbc = TRUE) %>%
      select(test_date, ph_value, tds, test_date_tbc, tbc) %>%
      mutate(tbc = ifelse(tbc == TRUE || tbc == "true", "TRUE", "FALSE")) %>%
      tibble::add_column(" " = actions, .before = 1)
  })

  output$tests_table <- DT::renderDT({

    req(tests_prep())

    out <- tests_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- c(" ", "Test Date", "PH Value", "TDS", "Test Date TBC", "Total Bacterial Count (TBC)")
    esc_cols <- c(-1, -6)
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
      filter = tbl_filt,
      options = list(
        scrollX = tbl_scroll,
        dom = "<'row'<'col-sm-3'l><'col-sm-6 text-center'B><'col-sm-3'f>>
               <'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i><'col-sm-7'p>>",
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "35px"),
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
      DT::formatRound("ph_value", digits = 2) %>%
      DT::formatStyle("tbc",
                      target = "cell",
                      color = styleEqual(levels = c("TRUE", "FALSE"), values = c("green", "red")))

  })

}

# Copy in UI
#vendor_tests_ui("vendor_tests_ui")

# Copy in server
#callModule(vendor_tests_, "vendor_tests_ui")
