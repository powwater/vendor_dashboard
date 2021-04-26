
get_cran_deps <- function(deps) {
  deps[sapply(deps, function(el) identical(el$Repository, "CRAN"))]
}

get_gh_deps <- function(deps) {
  deps[sapply(deps, function(el) !is.null(el$GithubRepo))]
}

cran_packages_cmd <- function(cran_deps) {
  cran_deps_string <- unlist(lapply(cran_deps, cran_install_string))
  paste0(
    "# CRAN R packages \n",
    paste(cran_deps_string, collapse = " \n"),
    "\n"
  )
}

gh_packages_cmd <- function(gh_deps) {
  github_deps_string <- unlist(lapply(gh_deps, github_install_string))
  paste0(
    "# GitHub R packages \n",
    paste(github_deps_string, collapse = " \n"),
    "\n"
  )
}

cran_install_string <- function(dep) {
  paste0("RUN R -e \"remotes::install_version('", dep$Package, "', version = '",
         dep$Version,
         "', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))\"")
}

github_install_string <- function(dep) {
  paste0("RUN R -e \"remotes::install_github('", dep$GithubUsername, "/",
         dep$Package, "', ref = '", dep$GithubSHA1, "', upgrade='never')\"")
}

r_command_string <- function(command) {
  paste0("RUN R -e \"", command, "\"")
}
