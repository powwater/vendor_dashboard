get_customer_locations_by_vendor <- function(vendor_id, conn) {

  conn %>%
    dplyr::tbl("orders") %>%
    dplyr::filter(.data$vendor_uid == vendor_id) %>%
    dplyr::distinct(customer_uid, customer_name) %>%
    dplyr::left_join(
      conn %>% dplyr::tbl("customer_locations") %>%
        dplyr::select(-c(created_at:modified_by)) %>%
        rename(customer_location_uid = uid),
      by = c("customer_uid")
    ) %>%
    dplyr::left_join(
      conn %>% dplyr::tbl("customers"),
      by = c("customer_uid" = "uid", "customer_name")
    ) %>%
    dplyr::rename(customer_location_url = customer_location_url.x, customer_location_url_full = customer_location_url.y) %>%
    dplyr::select(-customer_location) %>%
    left_join(
      conn %>% dplyr::tbl("order_routes") %>% dplyr::select(-uid, -order_uid, -c(created_at:modified_by)),
      by = c("customer_location_uid")
    ) %>%
    left_join(
      conn %>% dplyr::tbl("vendor_locations") %>% dplyr::select(-c(created_at:modified_by)),
      by = c("vendor_location_uid" = "uid")
    ) %>%
    collect()

}


get_orders_by_vendor <- function(vendor_id, conn) {
  conn %>%
    dplyr::tbl("orders") %>%
    filter(.data$vendor_uid == vendor_id) %>%
    rename(order_uid = uid) %>%
    collect()
}
