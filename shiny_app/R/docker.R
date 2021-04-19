docker_is_running <- function() {
  response <- system2("docker", "ps ", stdout = TRUE,
                      stderr = TRUE)

  if (length(response) == 1 && stringr::str_detect(response[[1]], "CONTAINER.+CREATED.+NAMES")) {
    return(TRUE)
  }

  if (length(response) == 1) return(FALSE)

  TRUE
}

