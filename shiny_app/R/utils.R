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
      titleAttr = "Column Visibility"
    ),
    list(
      extend = "pageLength",
      text = '<i class="fa fa-list"></i>',
      titleAttr = "Page Length"
    )
  )

}

notify <- function(msg, id = NULL) {
  showNotification(msg, id = id, duration = NULL, closeButton = FALSE, type = "message")
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
