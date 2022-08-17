#' Export Raw Data
#'
#' After evaluation, export the raw data in R.
#'
#' @param object An object of class `psythelper`
#'
#' @importFrom purrr pluck
#'
#' @return A tibble
#' @export
ext_raw_data <- function(object) {
  pluck(object, "data")
}
