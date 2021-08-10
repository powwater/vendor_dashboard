customers_module_ui <- function(id) {

  ns <- shiny::NS(id)

  tagList(
    fluidRow(
      box(
        width = 12,
        title = icon_text("user-friends", 'Customers'),
        status = "primary",
        solidHeader = TRUE,
        height = NULL,
        fluidRow(
          column(
            width = 12,
            DT::DTOutput(ns('customers_table')) %>%
              shinycssloaders::withSpinner()
          )
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = icon_text("map-marked-alt", "Maps"),
        status = "primary",
        solidHeader = TRUE,
        height = NULL,
        fluidRow(
          column(
            width = 6,
            tags$span(class = "text-center",
                      shiny::icon("map-marked-alt"),
                      h4(" Vendor Region:", style = "display: inline-block;")),
            uiOutput(ns("vendor_region"))
          ),
          column(
            width = 6,
            tags$span(class = "text-center",
                      shiny::icon("map-marker-alt"),
                      h4("Vendor Customer Locations:", style = "display: inline-block;")),
            div(
              class = "pull-right",
              actionButton(ns("map_bttn"),
                           "View All",
                           icon = icon("map")) %>% shinyjs::hidden()
            ),
            googleway::google_mapOutput(ns("customer_locations"), width = "100%", height = "400px"),
            helpText("Select a customer above to view route information.
                     Click on a marker or line for an information window to appear."),
            uiOutput(ns("map_details"))
          )
        )
      )
    ),
    shiny::tags$script(src = "customers_module.js"),
    shiny::tags$script(paste0("customers_table_module_js('", ns(''), "')"))
  )


}

customers_module <- function(input, output, session, vendor_info, is_mobile) {

  ns <- session$ns

  customers <- reactive({

    # id <- notify("Loading Customers from Database...")
    # on.exit(shinyFeedback::hideToast(), add = TRUE)

    vend <- vendor_info()$vendor_uid

    out <- NULL

    tryCatch({

      out <- get_customer_details_by_vendor(vend, conn)

    }, error = function(err) {
      msg <- 'Error collecting customers from database.'
      print(msg)
      print(err)
      shinyFeedback::showToast('error', msg)
    })

    out

  })

  customers_prep <- reactiveVal(NULL)

  observeEvent(customers(), {
    req(customers())
    hold <- customers()

    ids <- hold$customer_uid

    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 35px;" role="group" aria-label="Customer Buttons">
          <button class="btn btn-success btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="View Customer Location" id="', id_,'" style="margin: 0"><i class="fas fa-map-marker"></i></button>
        </div>'
      )
    })

    out <- hold %>%
      mutate(
        customer_location = Vectorize(create_link)(customer_location_url, customer_location_name),
        customer_coordinates = create_coords_string(customer_location_lat, customer_location_lon),
        customer_region = Vectorize(create_link)(vendor_region_url, vendor_region_name),
        total_paid = Vectorize(format_currency_kes)(total_paid),
        estimated_duration = Vectorize(format_duration_minutes)(estimated_duration),
        estimated_distance = Vectorize(format_distance_km)(estimated_distance)
      ) %>%
      #ratings_to_stars(cols = "average_rating") %>%
      tibble::add_column(" " = actions, .before = 1) %>%
      select(
        " ",
        customer_name,
        #customer_phone_number,
        number_of_orders,
        last_order_date,
        total_paid,
        #average_rating,
        estimated_distance,
        estimated_duration,
        customer_location,
        # customer_location_address,
        customer_region,
        customer_coordinates
      ) %>%
      arrange(desc(last_order_date))

    if (is.null(customers_prep())) {
      customers_prep(out)
    } else {
      replaceData(customers_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }
  })

  output$customers_table <- DT::renderDT({
    req(customers_prep())
    out <- customers_prep()

    cols <- c(" ",  "Customer Name", "# Orders",
              "Last Order Date", "Total Paid",
              "Distance", "Duration",
              "Location", "City", "Coordinates")
    esc_col_names <- c(" ", "customer_location", "customer_region")
    esc_cols <- purrr::map_dbl(esc_col_names, function(x) {
      -1 * match(x, colnames(out))
    })
    id <- session$ns("customers_table")

    n_col <- length(out)
    dt_js <- paste0(
      "function(settings, json) {
      var filters = $('#", id, " td .form-group');
      // columns that should have visible filters
      var cols = [", paste(2:n_col, collapse = ", "), "]
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
      style = "bootstrap",
      rownames = FALSE,
      colnames = cols,
      class = tbl_class,
      escape = esc_cols,
      extensions = tbl_exts,
      filter = tbl_filt,
      selection = "none",
      width = "100%",
      callback = DT::JS('Shiny.setInputValue("waiter_trigger", "1", {
                          priority: "event"
                        });'),
      options = list(
        scrollX = tbl_scroll,
        dom = "<'row'<'col-sm-3'l><'col-sm-6 text-center'B><'col-sm-3'f>>
               <'row'<'col-sm-12'tr>>
               <'row'<'col-sm-5'i><'col-sm-7'p>>",
        columnDefs = list(
          list(targets = 0, orderable = FALSE, className = "bttn_col", width = "35px"),
          list(className = "dt-center", targets = "_all")
        ),
        buttons = dt_bttns(out, "customers-table", esc_cols),
        initComplete = JS(dt_js),
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

  map_data <- reactive({
    req(customers(), session$userData$vendor(), nrow(customers()) > 0)
    hold <- customers()
    vendor_uid <- session$userData$vendor()$vendor_uid


    customer_markers_data <- hold %>%
      filter(!is.na(customer_location_name)) %>%
      mutate(
        title = paste0(customer_name, ": ", customer_location_name),
        info_box = paste0(
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


    vendor_dat <- NULL
    tryCatch({


      vendor_dat <- conn %>%
        tbl("vendors") %>%
        filter(.data$uid == .env$vendor_uid) %>%
        select(
          vendor_uid = uid,
          website,
          phone_number,
          operation_description
        ) %>%
          collect()


    }, error = function(err) {

      msg <- "unable to get vendor data"
      print(msg)
      print(err)
      showToast("error", msg)

    })

    vendor_markers_data <- hold %>%
      select(
        vendor_uid,
        vendor_location_uid,
        vendor_name,
        vendor_location_name,
        vendor_location_address,
        vendor_location_url,
        vendor_location_place_id,
        vendor_location_lat,
        vendor_location_lon
      ) %>%
      filter(!is.na(vendor_location_place_id)) %>%
      left_join(vendor_dat, by = "vendor_uid") %>%
      distinct() %>%
      mutate(
        colour = "blue",
        title = paste0(vendor_name, ": ", vendor_location_name),
        image_path = ifelse(vendor_name == "Dutch Water",
                            "images/profiles/dutch_water.png",
                            "images/profiles/no_picture.png"),
        info_box = as.character(tags$div(fluidPage(shiny::htmlTemplate(
          "www/html/vendor_info_box_template.html",
          image_path = .data$image_path,
          vendor_name = .data$vendor_name,
          vendor_address = .data$vendor_location_address,
          vendor_location = create_link_pull_right(.data$vendor_location_url, .data$vendor_location_address),
          vendor_phone = .data$phone_number,
          vendor_website = create_link_pull_right(.data$website, .data$website),
          vendor_desc = .data$operation_description
        ))))
      )

    list(customers = customer_markers_data,
         vendors = vendor_markers_data)

  })

  output$customer_locations <- renderGoogle_map({
    req(map_data())

    cust_dat <- map_data()$customers
    vend_dat <- map_data()$vendors
    key <- googleway::google_keys()$google$default

    google_map(vend_dat,
               key = key,
               location = c(vend_dat$vendor_location_lat, vend_dat$vendor_location_lon),
               zoom = 7,
               map_type_control = TRUE,
               zoom_control = TRUE,
               scale_control = TRUE,
               rotate_control = TRUE,
               fullscreen_control = TRUE,
               event_return_type = "list"
    ) %>%
      add_markers(
        data = cust_dat,
        id = "customer_uid",
        lat = "customer_location_lat",
        lon = "customer_location_lon",
        title = "title",
        layer_id = "customer_locations_layer",
        info_window = "info_box",
        close_info_window = TRUE,
        update_map_view = TRUE
      ) %>%
      add_markers(
        data = vend_dat,
        id = "vendor_uid",
        lat = "vendor_location_lat",
        lon = "vendor_location_lon",
        title = "title",
        info_window = "info_box",
        close_info_window = TRUE,
        layer_id = "vendor_location_layer",
        colour = "colour",
        update_map_view = TRUE,
        focus_layer = TRUE
      )
  })

  customer_to_map <- eventReactive(input$customer_id_to_map, {
    map_data()$customers %>%
      filter(customer_uid == input$customer_id_to_map)
  })

  details_txt <- reactiveVal(NULL)

  observeEvent(customer_to_map(), {

    shinyjs::show("map_bttn")

    scroll(session$ns("customer_locations"))

    sel <- customer_to_map()

    details <- paste0("Distance from Customer to Vendor: ",
                      format_distance_km(sel$estimated_distance),
                      "; Estimated Duration of a Delivery: ",
                      format_duration_minutes(sel$estimated_duration))

    details_txt(details)

    sel$poly_info <- tagList(
      tags$div(
        tags$span(icon("road"), h4("Customer Vendor Locations",
                                   style = "display:inline-block;")),
        hr(),
        h5(paste0("Customer: ", sel$customer_name)),
        h5(paste0("Vendor: ", sel$vendor_name)),
        hr(),
        p("Distance from Customer to Vendor: ",
          tags$strong(format_distance_km(sel$estimated_distance))),
        br(),
        p("Estimated Duration of a Delivery: ",
          tags$strong(format_duration_minutes(sel$estimated_duration)))
      )
    )

    poly_df <- decode_pl(sel$estimated_polyline)
    sel$poly <- encode_pl(poly_df$lat, poly_df$lon)

    google_map_update(session$ns("customer_locations"),
                      session = session,
                      data = sel) %>%
      clear_markers(layer_id = "customer_locations_layer") %>%
      clear_markers(layer_id = "single_customer_layer") %>%
      clear_polylines(layer_id = "single_customer_polyline") %>%
      add_markers(
        data = sel,
        id = "customer_uid",
        lat = "customer_location_lat",
        lon = "customer_location_lon",
        title = "title",
        info_window = "info_box",
        layer_id = "single_customer_layer",
        update_map_view = TRUE,
        focus_layer = TRUE
      ) %>%
      add_polylines(data = sel,
                    polyline = "poly",
                    info_window = "poly_info",
                    update_map_view = TRUE,
                    focus_layer = TRUE,
                    layer_id = "single_customer_polyline",
                    stroke_weight = 3,
                    stroke_colour = assets$colors$blue,
                    stroke_opacity = 1)
  })

  observeEvent(input$map_bttn, {

    shinyjs::hide("map_bttn")
    details_txt(NULL)

    google_map_update(session$ns("customer_locations"),
                      session = session,
                      data = map_data()) %>%
      clear_markers(layer_id = "single_customer_layer") %>%
      clear_polylines(layer_id = "single_customer_polyline") %>%
      add_markers(
        data = map_data()$customers,
        id = "customer_uid",
        lat = "customer_location_lat",
        lon = "customer_location_lon",
        title = "title",
        layer_id = "customer_locations_layer",
        info_window = "info_box",
        close_info_window = TRUE,
        update_map_view = TRUE
      )
  })



  output$map_details <- renderUI({
    req(details_txt(), !is.null(details_txt()))
    h5(tags$em(details_txt()))
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

  return(customers)
}
