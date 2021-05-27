time_now_utc <- function() {
  lubridate::with_tz(Sys.time(), tzone = "UTC")
}

get_last_updated_date <- function(path = ".") {
  fs::dir_info(path) %>%
    tibble::as_tibble() %>%
    dplyr::pull("modification_time") %>%
    max(na.rm = TRUE)
}

create_link <- function(val, txt = NA) {
  if (identical(length(val), 0L)) return(val)
  if (is.na(txt)) txt <- val
  paste0("<a href='", val, "' target='_blank'>", txt, "</a>")
}

create_link_pull_right <- function(val, txt = NA) {
  if (is.na(txt)) txt <- val
  paste0("<a class='pull-right' href='", val, "' target='_blank'>", txt, "</a>")
}

notify <- function(msg, id = NULL) {
  shinyFeedback::showToast("info", msg, title = "Please Wait")
}

pull_unique <- function(data, var, sort = TRUE,
                        decreasing = FALSE, names = TRUE) {

  hold <- data %>%
    dplyr::pull(!!rlang::sym(var)) %>%
    unique()

  if (sort) hold <- hold %>% sort(decreasing = decreasing)
  if (names) hold <- hold %>% purrr::set_names()

  return(hold)

}

create_coords_string <- function(lat, lon) {

  if (identical(length(lat), 0L)) return(character(0))

  lat <- round(lat, 3)
  lon <- round(lon, 3)

  paste0(
    "(", lat, ", ", lon, ")"
  )

}

create_directions_iframe <- function(key, start, stop, type = "place_id") {

  paste0(
    '<div style="max-width:100%;list-style:none; transition: ',
    'none;overflow:hidden;width:100%;height:500px;">',
    '<div id="display-googlemap" style="height:100%; width:100%;max-width:100%;">',
    '<iframe style="height:100%;width:100%;border:0;" frameborder="0" ',
    'src="https://www.google.com/maps/embed/v1/directions?origin=place_id:',
    start, '&destination=place_id:', stop, '&key=', key,
    '"></iframe></div><a class="googlehtml" href="https://changing.hosting"',
    'id="authorizemaps-data">changing.hosting</a><style>#display-googlemap',
    '.text-marker{}.map-generator{max-width: 100%; max-height: 100%; ',
    'background: none;</style></div>')

}
