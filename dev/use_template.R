use_template <- function(template_path, out_path, data = list()) {
  contents <- strsplit(whisker::whisker.render(usethis:::read_utf8(template_path), data), "\n")[[1]]
  usethis:::write_over(out_path, contents)
  file.edit(out_path)
}
