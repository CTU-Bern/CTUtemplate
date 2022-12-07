# functions from usethis (to avoid heavy dep on usethis)
use_template <- function(template,
                         save_as = template,
                         data = list(),
                         open = FALSE,
                         package = "usethis") {
  template_contents <- render_template(template, data, package = package)
  base::writeLines(con = save_as, text = template_contents)
  if (open) {
    if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
      rstudioapi::navigateToFile(save_as)
    } else {
      utils::file.edit(save_as)
    }
  }
  return(invisible(NULL))
}

#' @importFrom fs path_package
render_template <- function(template, data = list(), package = "CTUtemplate") {
  template_path <- find_template(template, package = package)
  strsplit(whisker::whisker.render(
    base::readLines(template_path, encoding = "UTF-8", warn = FALSE),
    data), "\n")[[1]]
}

find_template <- function(template_name, package = "CTUtemplate") {
  path <- tryCatch(
    path_package(package = package, "templates", template_name),
    error = function(e) ""
  )
  if (identical(path, "")) {
    stop(
      glue::glue("Could not find template {template_name}."
    ))
  }
  path
}
