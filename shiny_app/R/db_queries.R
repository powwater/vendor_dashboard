get_customer_locations_by_vendor <- function(vendor_id, conn) {

  conn %>%
    dplyr::tbl("orders") %>%
    dplyr::filter(.data$vendor_uid == vendor_id) %>%
    dplyr::distinct(customer_uid) %>%
    dplyr::left_join(
      conn %>% dplyr::tbl("customer_locations") %>%
        dplyr::select(-c(created_at:modified_by)) %>%
        rename(customer_location_uid = uid),
      by = c("customer_uid")
    ) %>%
    dplyr::left_join(
      conn %>% dplyr::tbl("customers"),
      by = c("customer_uid" = "uid")
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
    left_join(
      conn %>% tbl("customers") %>%
        select(customer_uid = uid, customer_name),
      by = c("customer_uid")
    ) %>%
    left_join(
      conn %>% tbl("vendors") %>%
        select(vendor_uid = uid, vendor_name),
      by = c("vendor_uid")
    ) %>%
    left_join(
      conn %>% tbl("riders") %>%
        select(rider_uid = uid, rider_name),
      by = c("rider_uid")
    ) %>%
    collect()
}

get_routes_by_vendor <- function(vendor_location_id, conn) {

  conn %>%
    dplyr::tbl("order_routes") %>%
    dplyr::filter(vendor_location_uid == vendor_location_id) %>%
    left_join(
      conn %>%
        tbl('vendor_locations') %>%
        select(vendor_location_uid = uid,
               vendor_uid,
               vendor_location_lat,
               vendor_location_lon,
               vendor_location_name,
               vendor_location_address,
               vendor_location_place_id),
      by = "vendor_location_uid"
    ) %>%
    left_join(
      conn %>%
        tbl('customer_locations') %>%
        select(customer_location_uid = uid,
               customer_uid,
               customer_location_lat,
               customer_location_lon,
               customer_location_name,
               customer_location_address,
               customer_location_place_id),
      by = "customer_location_uid"
    ) %>%
    dplyr::collect()

}
