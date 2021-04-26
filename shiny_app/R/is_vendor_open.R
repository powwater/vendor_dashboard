
convert_time_utc <- function(time) {

  today <- lubridate::as_date(Sys.time(), tz = "UTC")
  hold <- paste0(today, " ", time)
  lubridate::ymd_hms(hold, tz = "UTC")

}

is_vendor_open <- function(conn,
                           vendor,
                           now = Sys.time(),
                           ...) {

  hours_data <- conn %>% tbl("vendor_working_hours")

  day <- lubridate::wday(now)

  hours_data_filt <- hours_data %>%
    dplyr::filter(
      day_of_week == day,
      vendor_uid == vendor,
      !is.na(working_hours_start),
      !is.na(working_hours_stop)
    ) %>%
    collect()

  if (nrow(hours_data_filt) == 0) return(FALSE)

  hours_data_filt_adj <- hours_data_filt %>%
    dplyr::mutate(
      start = convert_time_utc(working_hours_start),
      stop = convert_time_utc(working_hours_stop)
    )

  interval <- hours_data_filt_adj$start %--% hours_data_filt_adj$stop

  now <- lubridate::ymd_hms(now, tz = "UTC")

  is_online <- now %within% interval

  return(is_online)

}

