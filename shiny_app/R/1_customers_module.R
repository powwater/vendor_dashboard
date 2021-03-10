customers_module_ui <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      box(
        width = 12,
        title = 'Customers',
        DT::DTOutput(ns('customers_table')) %>%
          shinycustomloader::withLoader(),
        br(),
        googleway::google_mapOutput(ns("customer_locations")),
        br(),
        uiOutput(ns("vendor_region"))
      )
    )
  )


}

customers_module <- function(input, output, session, vendor_info) {
  ns <- session$ns

  # customers <- reactiveVal(NULL)

  customers <- reactive({

    # browser()

    id <- notify("Loading Customers from Database...")
    on.exit(removeNotification(id), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- conn %>%
        dplyr::tbl("orders") %>%
        dplyr::filter(vendor_uid == vend) %>%
        dplyr::distinct(customer_uid, customer_name) %>%
        dplyr::left_join(
          conn %>% dplyr::tbl("customer_locations") %>% dplyr::select(-uid, -c(created_at:modified_by)),
          by = c("customer_uid")
        ) %>%
        dplyr::left_join(
          conn %>% dplyr::tbl("customers"),
          by = c("customer_uid" = "uid", "customer_name")
        ) %>%
        dplyr::rename(customer_location_url = customer_location_url.x, customer_location_url_full = customer_location_url.y) %>%
        dplyr::select(-customer_location) %>%
        dplyr::collect()

    }, error = function(err) {
      msg <- 'Error collecting data from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  customers_prep <- reactive({
    req(customers())

    customers() %>%
      mutate(
        customer_location = Vectorize(create_link)(customer_location_url, customer_location_name)
      ) %>%
      select(
        # customer_number,
        customer_name,
        customer_phone_number,
        customer_location,
        customer_location_address,
        customer_location_vicinity,
        customer_location_lat,
        customer_location_lon
      )
  })

  output$customers_table <- DT::renderDT({

    req(customers_prep())

    out <- customers_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- snakecase::to_title_case(colnames(out))
    esc_cols <- c(-1 * match("customer_location", colnames(out)))
    id <- session$ns("customers_table")

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
        scrollX = TRUE,
        dom = '<Bf>tip',
        columnDefs = list(
          # list(targets = 0, orderable = FALSE),
          list(className = "dt-center dt-col nowrap", targets = "_all")
          # list(render = JS("$.fn.dataTable.render.ellipsis( 20, false )"), targets = 4)
        ),
        buttons = dt_bttns(out, "customers-table", esc_cols)
      )
    )

  })

  output$customer_locations <- renderGoogle_map({

    # browser()

    dat <- customers() %>%
      mutate(
        title = paste0(customer_name, ": ", customer_location_name),
        info = paste0(
          "<div id='bodyContent'>",
          "<iframe width='450px' height='250px'",
          "frameborder='0' style = 'border:0'",
          "src=",
          paste0(
            "https://www.google.com/maps/embed/v1/place?q=place_id:",
            customer_location_place_id,
            "&key=",
            key
          ),
          "></iframe></div>"
        )
      )

    # set_key(key)

    google_map(dat,
               key = key,
               location = c(dat$customer_location_lat, dat$customer_location_lon),
               zoom = 12,
               search_box = TRUE,
               update_map_view = TRUE,
               geolocation = TRUE,
               map_type_control = TRUE,
               zoom_control = TRUE,
               # street_view_control = TRUE,
               scale_control = TRUE,
               rotate_control = TRUE,
               fullscreen_control = TRUE,
               event_return_type = c("list", "json")
    ) %>%
      add_markers(
        lat = "customer_location_lat",
        lon = "customer_location_lon",
        title = "title",
        # label = "customer_location_name",
        layer_id = "customer_locations",
        info_window = "info",
        mouse_over = "customer_location_address",
        # cluster = TRUE,
        update_map_view = TRUE
      )
  })

  # observeEvent(input$vendor_locations_marker_click, {
  #   print(input$vendor_locations_map_marker_click)
  # })

  output$vendor_region <- renderUI({
    HTML(
      paste0(
        '<iframe width="100%" height="450" style="border:0" loading="lazy" allowfullscreen
      src="https://www.google.com/maps/embed/v1/place?q=place_id:',
      vendor_info()$region_id, '&key=', key, '"></iframe>'
      )
    )
  })


}
