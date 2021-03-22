customers_module_ui <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      box(
        width = 12,
        title = icon_text("user-friends", 'Customers'),
        footer = "Powwater | Tychobra 2021",
        status = "primary",
        solidHeader = TRUE,
        height = NULL,
        DT::DTOutput(ns('customers_table')) %>%
          shinycustomloader::withLoader(),
        hr(),
        fluidRow(
          column(
            width = 6,
            offset = 6,
            span(
              uiOutput(ns("map_title")),
              actionButton(ns("map_bttn"),
                           "View All",
                           icon = icon("map")) %>%
                shinyjs::hidden()
            )
          )
        ),
        fluidRow(
          column(
            6,
            h5("Region:"),
            uiOutput(ns("vendor_region"))
          ),
          column(
            6,
            h5("Customer Locations:"),
            googleway::google_mapOutput(ns("customer_locations"))
          )
        )
      )
    ),
    htmltools::tags$script(src = "customers_module.js"),
    htmltools::tags$script(paste0("customers_table_module_js('", ns(''), "')"))
  )


}

customers_module <- function(input, output, session, vendor_info) {

  ns <- session$ns

  customers <- reactive({

    id <- notify("Loading Customers from Database...")
    on.exit(shinyFeedback::hideToast(), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- get_customer_locations_by_vendor(vend, conn)

    }, error = function(err) {
      msg <- 'Error collecting data from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  customers_prep <- reactiveVal(NULL)

  observeEvent(customers(), {
    req(customers())

    ids <- customers()$customer_uid

    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 35px;" role="group" aria-label="Customer Buttons">
          <button class="btn btn-success btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Customer Location" id="', id_,'" style="margin: 0"><i class="fas fa-map-marker"></i></button>
        </div>'
      )
    })

    out <- customers() %>%
      mutate(
        customer_location = Vectorize(create_link)(customer_location_url, customer_location_name),
        customer_coordinates = create_coords_string(customer_location_lat, customer_location_lon)
      ) %>%
      select(
        # customer_number,
        customer_name,
        customer_phone_number,
        customer_location,
        customer_location_address,
        customer_location_vicinity,
        customer_coordinates
      ) %>%
      # add action bttns
      tibble::add_column(" " = actions, .before = 1)

    if (is.null(customers_prep())) {
      customers_prep(out)
    } else {
      replaceData(customers_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }
  })

  output$customers_table <- DT::renderDT({

    req(customers_prep())

    out <- customers_prep()

    n_row <- nrow(out)
    n_col <- ncol(out)
    cols <- c(" ", "Name", "Phone Number", "Location", "Address", "Vicinity", "Coordinates")
    esc_cols <- c(-1, -1 * match("customer_location", colnames(out)))
    id <- session$ns("customers_table")

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
      Shiny.setInputValue('waiter_trigger', '1', {
        priority: 'event',
      });
      }")

    DT::datatable(
      out,
      style = "bootstrap",
      rownames = FALSE,
      colnames = cols,
      class = 'table table-striped table-bordered dt-center compact hover',
      escape = esc_cols,
      extensions = c("Buttons"),
      filter = "top",
      selection = "none",
      # selection = list(mode = "single", selected = NULL, target = "row", selectable = TRUE),
      options = list(
        scrollX = TRUE,
        dom = '<Bf>tip',
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "35px"),
          list(className = "dt-center dt-col", targets = "_all")
        ),
        buttons = dt_bttns(out, "customers-table", esc_cols),
        initComplete = DT::JS(dt_js),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }"),
        language = list(
          emptyTable = "No Orders for Selected Filters"
        )
      )
    )

  })

  customers_table_proxy <- DT::dataTableProxy("customers_table")

  map_data <- reactiveVal(NULL)

  observeEvent(customers(), {

    dat <- customers() %>%
      mutate(
        colour = "red",
        title = paste0(customer_name, ": ", customer_location_name),
        info = paste0(
          "<div id='bodyContent'>",
          "<h4>", customer_name, "</h4>",
          "<h5>", customer_location_name, "<h5><hr>",
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

    map_data(dat)

  })

  output$customer_locations <- renderGoogle_map({
    req(map_data(), customers())

    dat <- map_data()

    key <- googleway::google_keys()$google$default

    vend_dat <- tibble(
      lat = customers()$vendor_location_lat[[1]],
      lon = customers()$vendor_location_lon[[1]],
      address = customers()$vendor_location_address[[1]],
      place_id = vendor_info()$place_id,
      name = vendor_info()$vendor_name,
      colour = "blue"
    ) %>%
      mutate(title = paste0(name, ": ", address),
             info = paste0(
               "<div id='bodyContent'>",
               "<h4>", name, "</h4>",
               "<h5>", address, "<h5><hr>",
               "<iframe width='450px' height='250px'",
               "frameborder='0' style = 'border:0'",
               "src=",
               paste0(
                 "https://www.google.com/maps/embed/v1/place?q=place_id:",
                 place_id,
                 "&key=",
                 key
               ),
               "></iframe></div>"
             ))

    google_map(dat,
               key = key,
               # location = c(vend_dat$lat, vend_dat$lon),
               # zoom = 12,
               # search_box = TRUE,
               update_map_view = TRUE,
               geolocation = TRUE,
               map_type_control = TRUE,
               zoom_control = TRUE,
               # street_view_control = TRUE,
               scale_control = TRUE,
               rotate_control = TRUE,
               fullscreen_control = TRUE,
               event_return_type = "list"
    ) %>%
      add_markers(
        data = dat,
        id = "customer_uid",
        # colour = "colour",
        lat = "customer_location_lat",
        lon = "customer_location_lon",
        title = "title",
        layer_id = "customer_locations",
        info_window = "info",
        close_info_window = TRUE,
        focus_layer = TRUE,
        update_map_view = TRUE
      ) %>%
      add_markers(
        data = vend_dat,
        id = "place_id",
        lat = "lat",
        lon = "lon",
        title = "title",
        info_window = "info",
        close_info_window = TRUE,
        layer_id = "vendor",
        colour = "colour",
        update_map_view = TRUE,
        focus_layer = TRUE
      )
  })

  customer_to_map <- eventReactive(input$customer_id_to_map, {
    map_data() %>%
      filter(customer_uid == input$customer_id_to_map)
  })

  title_txt <- reactiveVal(NULL)

  observeEvent(customer_to_map(), {
    scroll(session$ns("customer_locations"))
    sel <- customer_to_map()
    res <- google_directions(
      origin = paste0("place_id:",
                      sel$customer_location_place_id[[1]]),
      destination = paste0("place_id:", vendor_info()$place_id),
      mode = "driving"
    )
    distance_txt <- res[["routes"]][["legs"]][[1]][["distance"]][["text"]]
    duration_txt <- res[["routes"]][["legs"]][[1]][["duration"]][["text"]]
    txt_out <- paste0("Distance from Customer to Vendor: ",
                      distance_txt,
                      "; Estimated Duration of a Delivery: ",
                      duration_txt)
    sel$poly <- res[["routes"]][["overview_polyline"]][["points"]]
    title_txt(txt_out)
    google_map_update(session$ns("customer_locations"),
                      session = session,
                      data = sel) %>%
      clear_markers(layer_id = "customer_locations") %>%
      add_markers(
        data = sel,
        id = "customer_uid",
        lat = "customer_location_lat",
        lon = "customer_location_lon",
        title = "title",
        info_window = "info",
        layer_id = "single_customer",
        update_map_view = TRUE,
        focus_layer = TRUE
      ) %>%
      add_polylines(polyline = "poly")
    shinyjs::show("map_bttn")
  })

  observeEvent(input$map_bttn, {
    google_map_update(session$ns("customer_locations"),
                      session = session,
                      data = map_data()) %>%
      clear_markers(layer_id = "single_customer") %>%
      add_markers(
        data = map_data(),
        id = "customer_uid",
        lat = "customer_location_lat",
        lon = "customer_location_lon",
        title = "title",
        info_window = "info",
        layer_id = "customer_locations",
        update_map_view = TRUE
      )
    # selectRows(customers_table_proxy, selected = NULL)
    title_txt("All Customer Locations")
    shinyjs::hide("map_bttn")
  })

  observeEvent(input$customer_locations_marker_click, {
    print(input$customer_locations_marker_click)
    id <- input$customer_locations_marker_click$id
    if (id == vendor_info()$place_id) return(NULL)
    sel <- customers() %>%
      mutate(row = row_number()) %>%
      filter(customer_uid == id)
    row <- sel %>% pull(row)
    res <- google_directions(
      origin = paste0("place_id:",
                      sel$customer_location_place_id[[1]]),
      destination = paste0("place_id:", vendor_info()$place_id),
      mode = "driving"
    )
    distance_txt <- res[["routes"]][["legs"]][[1]][["distance"]][["text"]]
    duration_txt <- res[["routes"]][["legs"]][[1]][["duration"]][["text"]]
    txt_out <- paste0("Distance from Customer to Vendor: ",
                      distance_txt,
                      "; Estimated Duration of a Delivery: ",
                      duration_txt)
    title_txt(txt_out)
    shinyjs::show("map_title")
    # selectRows(customers_table_proxy, selected = row)
  })

  output$map_title <- renderUI({
    req(title_txt())
    div(h4(title_txt()))
  })

  output$vendor_region <- renderUI({
    HTML(
      paste0(
        '<iframe width="100%" height="400px" style="border:0" loading="lazy" allowfullscreen
      src="https://www.google.com/maps/embed/v1/place?q=place_id:',
      vendor_info()$region_id, '&key=', key, '"></iframe>'
      )
    )
  })


}
