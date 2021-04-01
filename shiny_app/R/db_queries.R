get_customer_details_by_vendor <- function(vendor_id, conn, collect = TRUE) {

  hold <- conn %>%
    dplyr::tbl("orders") %>%
    dplyr::filter(.data$vendor_uid == vendor_id) %>%
    dplyr::group_by(customer_uid, vendor_uid) %>%
    dplyr::summarize(number_of_orders = n(),
                     last_order_date = max(order_date, na.rm = TRUE),
                     total_paid = sum(total_payment_price, na.rm = TRUE),
                     average_rating = mean(vendor_rating, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
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
    dplyr::left_join(
      conn %>% dplyr::tbl("vendors") %>% select(uid, vendor_name),
      by = c("vendor_uid" = "uid")
    ) %>%
    left_join(
      conn %>% dplyr::tbl("vendor_locations") %>% dplyr::select(-c(created_at:modified_by)),
      by = c("vendor_location_uid" = "uid", "vendor_uid")
    ) %>%
    select(
      customer_uid,
      vendor_uid,
      number_of_orders,
      last_order_date,
      total_paid,
      average_rating,

      customer_location_uid,
      customer_location_place_id,
      customer_name,
      customer_phone_number,
      customer_location_name,
      customer_location_address,
      customer_location_url,
      customer_location_lat,
      customer_location_lon,

      vendor_location_uid,
      vendor_location_place_id,
      vendor_name,
      # vendor_phone_number,
      vendor_location_name,
      vendor_location_address,
      vendor_location_url,
      vendor_location_lat,
      vendor_location_lon,
      vendor_region_name,
      vendor_region_place_id,
      vendor_region_url,

      origin_id,
      destination_id,
      estimated_distance,
      estimated_duration,
      estimated_polyline
    )

  if (!collect) return(hold)

  hold %>% collect()

}


get_orders_by_vendor <- function(vendor_id, conn, collect = TRUE) {
  hold <- conn %>%
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
    )

  if (!collect) return(hold)

  hold %>% collect()
}

get_completed_orders_by_vendor <- function(vendor_id, conn, collect = TRUE) {
  hold <- get_orders_by_vendor(vendor_id, conn, collect = FALSE) %>%
    filter(order_status == "Completed")

  if (!collect) return(hold)

  hold %>% collect()
}

get_routes_by_vendor <- function(vendor_location_id, conn, collect = TRUE) {

  hold <- conn %>%
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
    )

  if (!collect) return(hold)

  hold %>% collect()
}

get_riders_by_vendor <- function(vendor_id, conn, collect = TRUE) {

  hold <- conn %>%
    dplyr::tbl("orders") %>%
    dplyr::filter(vendor_uid == vendor_id) %>%
    select(vendor_uid, rider_uid) %>%
    distinct() %>%
    left_join(
      conn %>%
        tbl('riders') %>%
        select(rider_uid = uid,
               everything()),
      by = "rider_uid"
    )

  if (!collect) return(hold)

  hold %>% collect()

}
