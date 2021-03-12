res <- google_directions(origin = "place_id:ChIJrToahpQJQBgRZ6ukvDc1LO4", destination = "place_id:ChIJEWIbWucPQBgRlOJYb4bzHas", mode = "driving", key = "AIzaSyB_PPKpl5FeJqASVAulT2FWimKeDMAlJ5o")

df_route <- data.frame(route = res$routes$overview_polyline$points)

df_way <- cbind(
  res$routes$legs[[1]]$end_location,
  data.frame(address = res$routes$legs[[1]]$end_address)
)

df_way$order <- as.character(1:nrow(df_way))
google_map(key = key,
           search_box = TRUE,
           scale_control = TRUE,
           height = 1000,
           location = c(df_way$lat, df_way$lng)) %>%
  add_traffic() %>%
  clear_traffic() %>%
  clear_polylines() %>%
  clear_markers() %>%
  add_traffic() %>%
  add_polylines(data = df_route,
                polyline = "route",
                stroke_colour = "#FF33D6",
                stroke_weight = 7,
                stroke_opacity = 0.7,
                info_window = "New route",
                load_interval = 100) %>%
  add_markers(data = df_way,
              info_window = "end_address",
              label = "order")
