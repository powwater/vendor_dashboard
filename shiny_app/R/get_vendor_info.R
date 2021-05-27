get_vendor_info <- function(conn, vendor_uid_) {

  vendor_location_data <- conn %>%
    tbl("vendor_locations") %>%
    filter(vendor_uid == vendor_uid_) %>%
    select(vendor_location_uid = uid, vendor_uid, vendor_location_place_id, vendor_region_place_id) %>%
    collect()

  vendor_name <- conn %>%
    tbl("vendors") %>%
    filter(uid == vendor_uid_) %>%
    pull(vendor_name)

  list(
    vendor_uid = vendor_location_data$vendor_uid,
    vendor_location_uid = vendor_location_data$vendor_location_uid,
    vendor_name = vendor_name,
    place_id = vendor_location_data$vendor_location_place_id,
    region_id = vendor_location_data$vendor_region_place_id
  )

}
