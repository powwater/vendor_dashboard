docker_is_running <- function() {

  response <- system2("docker", "ps ", stdout = TRUE, stderr = TRUE)

  if (length(response) == 1 &&
      stringr::str_detect(response[[1]], "CONTAINER.+CREATED.+NAMES")) {
    return(TRUE)
  }

  if (length(response) == 1)
    return(FALSE)
  
  TRUE
}

docker_images <- function(list_all = FALSE) {
  prettyprint_format <- paste(
    "table {{.ID}}",
    "{{.Repository}}",
    "{{.Tag}}",
    "{{.Digest}}",
    "{{.CreatedSince}}",
    "{{.CreatedAt}}",
    "{{.Size}}",
    sep = "|"
  )

  all_flag <- ifelse(list_all, "--all ", "")

  docker_cmd <- paste("images ",
                      all_flag,
                      "--format ",
                      "\"",
                      prettyprint_format,
                      "\"",
                      sep = "")

  listing <-
    system2("docker", docker_cmd, stdout = TRUE, stderr = FALSE)

  if (length(listing) > 1) {
    images <- listing %>%
      readr::read_delim(
        delim = "|",
        col_types = readr::cols(.default = readr::col_character())
      )

    colnames(images) <-
      colnames(images) %>% snakecase::to_snake_case()

    return(images)

  }

  return(tibble::tibble())

}

docker_containers <- function() {
  prettyprint_format <- paste(
    "table {{.ID}}",
    "{{.Image}}",
    "{{.Command}}",
    "{{.CreatedAt}}",
    "{{.RunningFor}}",
    "{{.Ports}}",
    "{{.Status}}",
    "{{.Size}}",
    "{{.Names}}",
    "{{.Labels}}",
    "{{.Mounts}}",
    "{{.Networks}}",
    sep = "|"
  )
  all_flag <- ""
  docker_cmd <- paste("ps ",
                      all_flag,
                      "--format ",
                      "\"",
                      prettyprint_format,
                      "\"",
                      sep = "")
  listing <- system2("docker", docker_cmd, stdout = TRUE,
                     stderr = FALSE)
  if (length(listing) > 1) {
    containers <- listing %>% readr::read_delim(
      delim = "|",
      col_types = readr::cols(.default = readr::col_character())
    )
    colnames(containers) <-
      colnames(containers) %>% snakecase::to_snake_case()
    return(containers)
  }
  return(tibble::tibble())
}

docker_build <- function(build_options, build_context_path) {
  docker_cmd <-
    glue::glue("build ", build_options, " ", build_context_path)
  result <- .system2_to_docker(docker_cmd)

}

docker_build_local_db <- function(path = "database") {
  opts <- paste0("-e .env -e R_CONFIG_ACTIVE=-t ", "pow_local_db")
  docker_build("-t ")

}
# is_docker_up <- function() {
#
#   response <- system2("docker", "ps ", stdout = TRUE, stderr = TRUE)
#
#   if (length(response) == 1) {
#     out <- stringr::str_detect(response[[1]], "CONTAINER.+CREATED.+NAMES")
#     message <- if (out) "Docker is up but running no containers" else response[[1]]
#   }
#   else {
#     message <- c("Docker is up, running these containers:", response)
#     out <- TRUE
#   }
#   usethis::ui_info(message)
#   out
# }
