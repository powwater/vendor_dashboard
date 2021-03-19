create_coords_string <- function(lat, lon) {

  lat <- round(lat, 3)
  lon <- round(lon, 3)

  paste0(
    "(", lat, ", ", lon, ")"
  )

}



create_directions_iframe <- function(key, start, stop, type = "place_id") {

  paste0(
    '<div style="max-width:100%;list-style:none; transition: none;overflow:hidden;width:100%;height:500px;"><div id="display-googlemap" style="height:100%; width:100%;max-width:100%;"><iframe style="height:100%;width:100%;border:0;" frameborder="0" src="https://www.google.com/maps/embed/v1/directions?origin=place_id:',
    start, '&destination=place_id:', stop,
    '&key=', key, '"></iframe></div><a class="googlehtml" href="https://changing.hosting" id="authorizemaps-data">changing.hosting</a><style>#display-googlemap .text-marker{}.map-generator{max-width: 100%; max-height: 100%; background: none;</style></div>')

}


#' Function to format a date (usually) for printing.
#'
#' @param date Optional, a date to be formatted.
#' @param time_zone Should the time zone be printed with the date?
#' @param fractional_seconds Should fractional seconds be included when printing
#' the date?
#'
#' @return Character vector.
#'
#' @export
str_date_formatted <- function(date = NA, time_zone = TRUE,
                               fractional_seconds = TRUE) {

  # Get date if not supplied
  if (is.na(date)[1]) date <- lubridate::now(tz = Sys.timezone())

  # Format string
  format_date <- ifelse(
    fractional_seconds,
    "%Y-%m-%d %H:%M:%OS3",
    "%Y-%m-%d %H:%M:%S"
  )

  # Format
  x <- format(date, format = format_date, usetz = time_zone)

  return(x)

}

parse_unix_time <- function(x, tz = "UTC", origin = "1970-01-01") {
  as.POSIXct(x, tz = tz, origin = origin)
}

time_zone <- function(date) attr(date, "tzone")

#' Function to get a formatted date string used to prefix messages.
#'
#' @return Character vector.
#'
#' @export
date_message <- function() stringr::str_c(str_date_formatted(), ": ")

#' @importFrom dplyr pull
#' @importFrom fs dir_info
#' @importFrom tibble as_tibble
get_last_updated_date <- function(path = ".") {
  fs::dir_info(path) %>%
    tibble::as_tibble() %>%
    dplyr::pull("modification_time") %>%
    max(na.rm = TRUE)
}

create_link <- function(val, txt = NA) {
  if (is.na(txt)) txt <- val
  paste0("<a href='", val, "' target='_blank'>", txt, "</a>")
}

dt_bttns <- function(data, filename = "data", escape_cols = NULL) {

  colvis_cols <- c(1:(ncol(data) - 1))

  export_cols <- c(1:(length(data) - 1))

  if (!is.null(escape_cols)) {
    export_cols <- setdiff(export_cols, -1 * escape_cols)
  }

  fileout <- paste0(filename, "-", Sys.Date())

  list(
    list(
      extend = "copy",
      text = '<i class="fa fa-paperclip"></i>',
      titleAttr = 'Copy',
      exportOptions = list(columns = export_cols,
                           modifier = list(selected = NULL))
    ),
    list(
      extend = "print",
      text = '<i class="fa fa-print"></i>',
      titleAttr = 'Print',
      autoPrint = FALSE,
      exportOptions = list(columns = export_cols,
                           modifier = list(selected = NULL))
    ),
    list(
      extend = "excel",
      text = '<i class="fa fa-file-excel-o"></i>',
      titleAttr = "Excel",
      title = fileout,
      exportOptions = list(columns = export_cols,
                           modifier = list(selected = NULL))
    ),
    list(
      extend = "csv",
      text = '<i class="fa fa-file-csv"></i>',
      titleAttr = "CSV",
      title = fileout,
      exportOptions = list(columns = export_cols,
                           modifier = list(selected = NULL))
    ),
    list(
      extend = 'pdf',
      text = '<i class="fa fa-file-pdf-o"></i>',
      titleAttr = 'PDF',
      orientation = 'landscape',
      pageSize = "LEGAL",
      download = 'open',
      title = fileout,
      exportOptions = list(columns = ":visible"),
      modifier = list(selected = NULL)
    ),
    list(
      extend = "colvis",
      text = '<i class = "fa fa-filter"></i>',
      titleAttr = "Column Visibility",
      columns =  colvis_cols
    ),
    list(
      extend = "pageLength",
      text = '<i class="fa fa-list"></i>',
      titleAttr = "Page Length"
    )
  )

}

notify <- function(msg, id = NULL) {
  shinyFeedback::showToast("info", msg, title = "Please Wait")
  # showNotification(msg, id = id, duration = NULL, closeButton = FALSE, type = "message")
}

true_false_formatter <- formattable::formatter("span",
                                               style = x ~ formattable::style(
                                                 font.weight = "bold",
                                                 color = ifelse(
                                                   x == TRUE,
                                                   "forestgreen",
                                                   ifelse(x == FALSE, "red", "black")
                                                 )
                                               ))

true_false_formatter.malicious <- formattable::formatter("span",
                                                         style = x ~ formattable::style(
                                                           font.weight = "bold",
                                                           color = ifelse(
                                                             x == TRUE,
                                                             "red",
                                                             "forestgreen"
                                                           )
                                                         ))

#' @importFrom dplyr pull
#' @importFrom purrr set_names
#' @importFrom rlang sym
pull_unique <- function(data, var, sort = TRUE,
                        decreasing = FALSE, names = TRUE) {

  hold <- data %>%
    dplyr::pull(!!rlang::sym(var)) %>%
    unique()

  if (sort) hold <- hold %>% sort(decreasing = decreasing)
  if (names) hold <- hold %>% purrr::set_names()

  return(hold)

}

format_round_dollar <- function(value) {
  "$" %>%
    paste0(value %>%
             round() %>%
             prettyNum(big.mark = ","))
}

format_round <- function(value, digs = 0) {
  value %>% round(digits = digs) %>% prettyNum(big.mark = ",")
}

action_bttns <- function(id_) {
  paste0(
    '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id="', id_,'" style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id="', id_,'" style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
  )
}

#' @importFrom shiny icon
rating_stars <- function(rating, max_rating = 5) {
  star_icon <- function(empty = FALSE) {
    tagAppendAttributes(shiny::icon("star"),
                        style = paste("color:", if (empty) "#edf0f2" else "orange"),
                        "aria-hidden" = "true"
    )
  }
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
  })
  label <- sprintf("%s out of %s", rating, max_rating)
  div(title = label, "aria-label" = label, role = "img", stars)
}

#' @importFrom shiny icon tagAppendAttributes
#' @importFrom tidyselect all_of
ratings_to_stars <- function(dat, cols = c("rating")) {



  mutate_at(dat, vars(tidyselect::all_of(cols)), function(rating) {
    rating = case_when(
      is.na(rating) || is.null(rating) ~ paste0(tags$span(shiny::icon('star'))),
      rating == 0 ~ paste0(tags$span(shiny::icon('star'))),
      rating == 0.5 ~ paste0(tags$span(
        shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
      )),
      rating == 1 ~ paste0(tags$span(
        shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
      )),
      rating == 1.5 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 2 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 2.5 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 3 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 3.5 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 4 ~ paste0(tagList(
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        ),
        tags$span(
          shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
        )
      )),
      rating == 4.5 ~ paste0(
        tagList(
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star-half') %>% shiny::tagAppendAttributes(style = "color: orange;")
          )
        )
      ),
      rating == 5 ~ paste0(
        tagList(
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          ),
          tags$span(
            shiny::icon('star') %>% shiny::tagAppendAttributes(style = "color: orange;")
          )
        )
      )
    )
  }
  )
}


#' Icon Text
#'
#' Creates an HTML div containing the icon and text.
#'
#' @param icon fontawesome icon
#' @param text text
#'
#' @return HTML div
#' @export
#'
#' @examples
#' icon_text("table", "Table")
#'
#' @importFrom shiny icon tagList
#' @importFrom shiny icon tagList
icon_text <- function(icon, text) {

  i <- shiny::icon(icon)
  t <- paste0(" ", text)

  tags$span(i, t)

}

get_last_updated_date <- function(path = ".") {
  fs::dir_info(path) %>%
    tibble::as_tibble() %>%
    dplyr::pull("modification_time") %>%
    max(na.rm = TRUE)
}
