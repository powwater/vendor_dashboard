google_geocode <- function(input,
                           source = "google",
                           wkt = FALSE,
                           verbose = TRUE) {

  # Catch factors
  if (is.factor(input)) input <- as.character(input)

  # Geocode addresses
  if (verbose) {

    df <- ggmap::geocode(
      input,
      source = source,
      output = "latlona",
      override_limit = TRUE
    )

  } else {

    suppressWarnings(
      suppressMessages(
        df <- ggmap::geocode(
          input,
          source = source,
          output = "latlona",
          override_limit = TRUE
        )
      )
    )

  }

  if (class(df) == "data.frame") {

    # Add if missing
    if (!"address" %in% names(df)) df$address <- NA

    # Rename and add input string
    df <- df %>%
      mutate(input = input) %>%
      select(input,
             address_geocode = address,
             latitude = lat,
             longitude = lon)

    # Make a vector, not a good name here
    if (wkt) df <- stringr::str_c("POINT (", df$longitude, " ", df$latitude, ")")

  } else {

    # Empty data frame
    df <- data.frame()

  }

  return(df)

}
