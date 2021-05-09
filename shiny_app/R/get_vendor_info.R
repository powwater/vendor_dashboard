vendor_users <- tibble::tribble(
  ~user_uid, ~vendor_uid,
  "45d04a30-dba4-45c4-a69e-847c8e7be7f9", "c401b531-719d-4cad-82e7-71db3ffba166"
)

get_vendor_info <- function(conn, vendor_uid) {

  vendor_uid_ <- vendor_uid

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
