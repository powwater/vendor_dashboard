# vendor_profile_ui <- function(id) {
#   ns <-  NS(id)
#
#   tagList(
#     box(
#       title = "Vendor User Profile",
#       status = "primary",
#       uiOutput(ns("profile_box"))
#   )
# }
#
# vendor_profile <- function(input, output, session, vendor_info) {
#
#   vendor_data <- reactive({
#
#   })
#
#
#   boxProfile(
#     src = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
#     title = vendor_info()$vendor_name,
#     subtitle = vendor_info()
#
#   )
#
# }
