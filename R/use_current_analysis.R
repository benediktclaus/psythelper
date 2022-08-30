#' Use Current Analysis Template
#'
#' @return Nothing, used for side effects
#' @export
use_current_analysis <- function() {
  template_path <- path(path_package("psythelper"), "templates", "analyses.R")
  folder_path <- path(path_wd(), "03 R")

  file_copy(template_path, folder_path, overwrite = TRUE)
}
