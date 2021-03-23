
is_vendor_online <- function(conn,
                             vendor,
                             now = Sys.time(),
                             ...) {

  hours_data <- conn %>% tbl("vendor_working_hours")

  day <- lubridate::wday(now)

  filt <- hours_data %>%
    dplyr::filter(
      day_of_week == day,
      vendor_uid == vendor,
      !is.na(working_hours_start),
      !is.na(working_hours_stop)
    ) %>%
    collect() %>%
    dplyr::mutate(
      start = lubridate::ymd_hms(
        paste0(
          lubridate::as_date(now, tz = "UTC"), " ", working_hours_start
        ),
        tz = "UTC"
      ),
      stop = lubridate::ymd_hms(
        paste0(
          lubridate::as_date(now, tz = "UTC"), " ", working_hours_stop
        ),
        tz = "UTC"
      )
    )

  interval <- filt$start %--% filt$stop

  now <- lubridate::ymd_hms(now, tz = "UTC")

  is_online <- now %within% interval

  return(is_online)

}
