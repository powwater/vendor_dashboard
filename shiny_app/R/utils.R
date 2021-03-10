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
