#' Create Results Table
#'
#' Create a Word file containing a formated table with all instrument results.
#'
#' @param object An object of class `psythelper`
#'
#' @importFrom dplyr left_join arrange rename_with all_of
#' @importFrom snakecase to_title_case
#' @importFrom gt gt cols_align tab_spanner gtsave
#'
#' @return A Word file in folder `"04 Figures"`
#' @export
create_table <- function(object) {
  data_raw <- ext_raw_data(object)
  instruments_raw <- object$instruments

  instruments <- instruments_raw[!instruments_raw %in% c("fet", "belastung")]

  data <- data_raw %>%
    select(.data$datum, all_of(instruments))

  if ("belastung" %in% instruments_raw) data_belastung <- data_raw %>% select(.data$datum, starts_with("belastung_")) %>% rename_with(~ str_remove(., "belastung_")) else data_belastung <- NULL
  if (!is.null(data_belastung)) data <- data %>% left_join(data_belastung, by = "datum")

  data <- data %>%
    arrange(.data$datum) %>%
    rename_with(~ to_title_case(., abbreviations = c("MoM", "DI", "AI", "WHO", "WI")))

  if (!"belastung" %in% instruments_raw) {
    results_table <- data %>%
      gt() %>%
      cols_align(align = "left", columns = "Datum")
  } else {
    results_table <- data %>%
      rename_with(~ str_replace_all(., " ", "/"), .cols = "Arbeit Ausbildung":"Familie Haus") %>%
      gt() %>%
      cols_align(align = "left", columns = "Datum") %>%
      tab_spanner(label = "Belastung", columns = "Arbeit/Ausbildung":"Familie/Haus")
  }

  gtsave(results_table, "04 Figures/_Ergebnisse.docx")
}
