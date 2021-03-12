#' @param start_coord This is a vector of the lat and lng coords at the start location
#' @param end_coord This is a vector of the lat and lng coords at the end location
#' @param map_key This is your Google Maps API key
#'
#' @return data.frame containing the lat and lng coords of the respective route
extract_route <- function(start_coord, end_coord, map_key = googleway:::get_api_key()) {

  # Extract Google Map object
  gmap_obj <- googleway::google_directions(
    origin = start_coord,
    destination = end_coord,
    key = map_key,
    mode = "driving"
  )

  # Extract polyline from the google maps object
  route <- gmap_obj$routes$overview_polyline$points %>%
    # decode polyline into lat and lng
    decode_pl()

}

# usage -------------------------------------------------------------------

library(openxlsx)
library(config)
library(googleway)
library(purrr)
library(dplyr)

config <- config::get()
key <- config$gcp$gmaps_api_key
set_key(key = key)

# load routes data
routes <- openxlsx::read.xlsx("C:/Users/Jimmy Briggs/Dev/powwater/.temp/powwater_adminportal____/data_prep/prepped/data-for-tychobra-working__.xlsx", "routes", detectDates = TRUE)

routes_adj <- routes %>%
  mutate(
    start_coord = map2(vendor_lat, vendor_lon, function(lat, lon) c(lat, lon)),
    end_coord = map2(customer_lat, customer_lon, function(lat, lon) c(lat, lon)),
    route = map2(start_coord, end_coord, possibly(extract_route, otherwise = NA), map_key = key)
  )


routes_adj <- routes_adj %>%
  filter(!is.na(route)) %>%
  mutate(poly_lat = map(.data$route, pluck, 1),
         poly_lon = map(.data$route, pluck, 2),
         polyline = map2_chr(poly_lat, poly_lon, possibly(encode_pl, otherwise = NA))
  )
# routes_adj$route <- map2(routes_adj$start_coord, routes_adj$end_coord, possibly(extract_route, otherwise = NA), map_key = key)

library(leaflet)

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(
    data = routes_adj$route[[1]] %>% as.data.frame(),
    lat = ~lat,
    lng = ~lon
  )

map

lstPalette <- list(fill_colour = colorRampPalette(c("red","blue")),
                   stroke_colour = viridisLite::plasma)

lstPalette <- list(fill_colour = colorRampPalette(c("red","blue")),
                   stroke_colour = viridisLite::plasma)

gmap <- googleway::google_map(
  data = routes_adj
) %>%
  add_circles(lat = "vendor_lat",
              lon = "vendor_lon",
              fill_colour = "vendor_lat",
              stroke_weight = 2,
              layer_id = "order_number",
              info_window = "customer_name",
              mouse_over = "order_number",
              mouse_over_group = "vendor_name",
              update_map_view = TRUE,
              # focus_layer = TRUE,
              z_index = 4,
              stroke_colour = "vendor_name",
              palette = lstPalette,
              legend = T) %>%
  add_polylines(polyline = "polyline",
                layer_id = "order_number",
                info_window = "customer_name",
                mouse_over = "order_number",
                mouse_over_group = "vendor_name",
                update_map_view = TRUE,
                focus_layer = TRUE,
                # legend = TRUE,
                z_index = 3) %>%
    googlewayExtra::add_canvas()

gmap

gmap <- googleway::google_map(
  data = routes_adj
) %>%
  add_polylines(polyline = "polyline",
                   # layer_id = "vendor",
                   # info_window = "customer_name",
                   # mouse_over = "order_number",
                   # mouse_over_group = "vendor_name",
                   update_map_view = TRUE,
                   focus_layer = TRUE,
                   # legend = TRUE,
                   z_index = 3)
  add_circles(lat = "vendor_lat",
              lon = "vendor_lon",
              fill_colour = "vendor_lat",
              stroke_weight = 2,
              # layer_id = "order_number",
              # info_window = "customer_name",
              # mouse_over = "order_number",
              # mouse_over_group = "vendor_name",
              update_map_view = TRUE,
              focus_layer = TRUE,
              z_index = 4,
              stroke_colour = "vendor_name",
              palette = lstPalette,
              legend = T) %>%


gmap
