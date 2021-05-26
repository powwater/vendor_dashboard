#' is_powwater_admin_user
#'
#' Check if user's email is from Tychobra or Powwater. If so, do not require invite
#'
#' @param email The user's email to check
#'
#' @importFrom stringr str_detect
#'
#' @export
#'
is_powwater_admin_user <- function(email) {
  stringr::str_detect(email, stringr::regex('(@tychobra\\.com|@powwater\\.com)'))
}
