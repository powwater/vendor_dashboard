# create_dt_btns <- function(df, id_col = "uid", bttns = c("edit", "delete", "info", "map", "history"), width = NULL) {
#
#   stopifnot(is.data.frame(df), id_col %in% colnames(df))
#   bttns <- match.arg("bttns", several.ok = TRUE)
#   num_bttns <- length(bttns)
#   if (is.null(width)) {
#     width <- switch(num_bttns,
#                     1 = "35px",
#                     2 = "75px",
#                     3 = "105px",
#                     4 = "135px",
#                     5 = "165px")
#   }
#
#   bttns_html <- bttns %>% purrr::map_chr(~ create_bttn_html)
#
#   bttns <- df[[id_col]] %>%
#     purrr::map_chr(
#       ~ paste0(
#         '<div class = "btn-group"> style="width: ',
#         width,
#         ';" role="group" aria-label="',
#         label, '>
#          <button class="btn btn-default action-button btn-info action_button" id="edit_',
#         .x,
#         '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
#         <button class="btn btn-default action-button btn-danger action_button" id="delete_',
#         .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
#       )
#     )
#
#   tibble::add_column(df, "Buttons" = bttns, .before = 1)
#
# }
#
# create_bttn_html <- function(id, type = c("edit", "delete", "info", "map", "history")) {
#
#   match.arg(type)
#   fn <- switch(type,
#                "edit" = create_bttn_html.edit,
#                "delete" = create_bttn_html.delete,
#                "info" = create_bttn_html.info,
#                "map" = create_bttn_html.map,
#                "history" = create_bttn_html.hist)
#
#   fn(id)
# }
#
# create_bttn_html.edit <- function(id, tooltip = "Edit", icon = "fa fa-edit") {
#   paste0(
#     '<button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="',
#     tooltip,
#     '" id="',
#     id,
#     '" style="margin: 0" onclick=get_id(this.id)><i class="',
#     icon,
#     '"</i></button>'
#   )
#
#
#   paste0(
#     '<button class="btn btn-default action-button btn-info action_button" id="edit_',
#     id,
#     '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>'
#   )
# }
#
# create_bttn_html.delete <- function(id, tooltip = "Delete", icon = "fa fa-trash-alt") {
#   paste0(
#     '<button class="btn btn-danger action-button btn-danger action_button" id="delete_',
#     id,
#     '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button>'
#   )
# }
#
# create_bttn_html.info <- function(id, tooltip = "View Info", icon = "fas fa-id-card") {
#   paste0(
#     '<button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="',
#     tooltip,
#     '" id="',
#     id,
#     '" style="margin: 0" onclick=get_id(this.id)><i class="',
#     icon,
#     '"</i></button>'
#   )
# }
#
# create_bttn_html.map <- function(id, tooltip = "View Map") {
#   paste0(
#     '<button class="btn btn-success btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="',
#     tooltip,
#     '" id="',
#     id,
#     '"style="margin: 0" onclick=get_id(this.id)><i class="fas fa-map-marker"></i></button>'
#   )
# }
