#' Print the results of an Evaluation
#'
#' @param x an object of class `"psythelper"`
#' @param ... Additional arguments
#'
#' @importFrom crayon green
#'
#' @return Nothing, called for side effects
#' @export
print.psythelper <- function(x, ...) {
  dates <- length(x$data$datum)
  n_instruments <- length(x$instruments)

  cat(green("Therapy Evaluation Results"))
  cat(paste0("\n\nTherapy was evaluated on ", dates, " measurements for ", n_instruments, " instruments."))
}
