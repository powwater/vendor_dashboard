format_round <- function(value, digs = 0) {
  value %>% round(digits = digs) %>% prettyNum(big.mark = ",")
}

format_round_dollar <- function(value) {
  "$" %>%
    paste0(value %>%
             round() %>%
             prettyNum(big.mark = ","))
}

format_currency_kes <- function(num) {
  if (is.na(num) || is.nan(num)) num <- 0
  paste0(formattable::currency(num, "", sep = "", big.mark = ","), " KES")
}

format_duration_minutes <- function(num) {
  if (is.na(num) || is.nan(num)) num <- 0
  paste0(round(num / 60, 0), " Minutes")
}

format_distance_km <- function(num) {
  if (is.na(num) || is.nan(num)) num <- 0
  paste0(round(num / 1000, 2), " Kilometers")
}

format_phone_number <- function(string,
                                type = c("national", "international"),
                                region = c("KE", "US")) {


  if (identical(length(string), 0L)) return(string)

  if (substr(string, 1L, 2L) == "+1") {
    region <- "US"
  }

  phone <- dialr::phone(string, region)

  format(phone,
         format = toupper(type),
         home = if (type == "national") toupper(region) else NULL,
         clean = TRUE,
         strict = TRUE) %>%
    as.character()
}

true_false_formatter <- formattable::formatter(
  "span",
  style = x ~ formattable::style(
    font.weight = "bold",
    color = ifelse(
      x == TRUE,
      "forestgreen",
      ifelse(x == FALSE, "red", "black")
    )
  )
)

true_false_formatter.malicious <- formattable::formatter(
  "span",
  style = x ~ formattable::style(
    font.weight = "bold",
    color = ifelse(
      x == TRUE,
      "red",
      "forestgreen"
    )
  )
)

format_true_false <- function(log, true_val = TRUE, false_val = FALSE) {

  log <- coalesce(log, false_val)

  formattable::formattable(log, formatter = true_false_formatter)

}
