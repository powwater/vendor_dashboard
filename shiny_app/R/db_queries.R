get_customer_details_by_vendor <- function(vendor_id, conn) {

  hold <- conn %>%
    dplyr::tbl("orders") %>%
    dplyr::filter(.data$vendor_uid == vendor_id) %>%
    dplyr::group_by(customer_uid, vendor_uid) %>%
    dplyr::summarize(
      number_of_orders = n(),
      last_order_date = max(created_at, na.rm = TRUE),
      total_paid = sum(total_payment_price, na.rm = TRUE)#,
      #average_rating = mean(vendor_rating, na.rm = TRUE)
    ) %>%
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
    dplyr::left_join(
      conn %>% dplyr::tbl("vendors") %>% select(uid, vendor_name),
      by = c("vendor_uid" = "uid")
    ) %>%
    dplyr::left_join(
      conn %>% dplyr::tbl("vendor_locations") %>%
        dplyr::rename(vendor_location_uid = uid) %>%
        dplyr::select(-c(created_at:modified_by)),
      by = c("vendor_uid")
    ) %>%
    left_join(
      conn %>% dplyr::tbl("distance_matrix") %>% dplyr::select(-uid, -c(created_at:modified_by)),
      by = c("customer_location_uid", "vendor_location_uid")
    ) %>%
    mutate(
      origin_id = paste0("place_id:", .data$vendor_location_place_id),
      destination_id = paste0("place_id:", customer_location_place_id)
    ) %>%
    select(
      customer_uid,
      vendor_uid,
      number_of_orders,
      last_order_date,
      total_paid,
      #average_rating,

      customer_location_uid,
      customer_location_place_id,
      customer_first_name,
      customer_last_name,
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
      estimated_distance = distance_val,
      estimated_duration = duration_val,
      estimated_polyline = polyline
    ) %>%
    collect() %>%
    mutate(customer_name = paste0(customer_first_name, " ", customer_last_name)) %>%
    select(-c(customer_first_name, customer_last_name))
}


get_orders_by_vendor <- function(vendor_id, conn) {
  hold <- conn %>%
    dplyr::tbl("orders") %>%
    filter(.data$vendor_uid == vendor_id) %>%
    select(
      order_uid = uid,
      customer_uid,
      vendor_uid,
      rider_uid,
      order_number,
      order_datetime = created_at,
      order_address,
      order_type,
      order_status,
      vendor_response,
      vendor_response_text,
      price_of_water,
      delivery_fee,
      delivery_commission,
      vendor_commission,
      discount_applied,
      discount_amount,
      total_payment_price,
      time_vendor_prep,
      time_rider_to_vendor,
      time_rider_to_customer,
      total_delivery_time,
      vendor_rating,
      rider_rating,
      modified_at
    ) %>%
    left_join(
      conn %>% tbl("customers") %>%
        select(customer_uid = uid, customer_first_name, customer_last_name),
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
    collect() %>%
    mutate(customer_name = paste0(customer_first_name, " ", customer_last_name)) %>%
    select(-c(customer_first_name, customer_last_name))
}

get_riders_by_vendor <- function(conn, vendor_id) {

  conn %>%
    tbl('riders') %>%
    filter(.data$vendor_uid == .env$vendor_id) %>%
    rename(rider_uid = uid) %>%
    collect()
}
