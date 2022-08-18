#' Extract Instruments in Dataset
#'
#' Returns the instruments in a dataframe. It basically detects named columns
#' with only NAs.
#'
#' @param data A tidy dataframe
#'
#' @importFrom dplyr select_if
#'
#' @return A vector of strings
.ext_instruments <- function(data) {
  data %>%
    select_if(~ all(is.na(.))) %>%
    names()
}
