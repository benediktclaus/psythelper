#' Clean the Most Recent Dataset
#'
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr select mutate across row_number
#' @importFrom readr write_rds write_excel_csv2
#' @importFrom lubridate as_date dmy_hms
#'
#' @return No return value, called for side effects only (creation of two files
#'   in folder "02 Data")
#' @export
clean_data <- function() {
  recent_dataset <- .get_recent_dataset()

  patient_import <- read_excel(recent_dataset) %>%
    clean_names(ascii = FALSE, transliterations = "de-ASCII") %>%
    select(-(.data$collector:.data$ip_adresse)) %>%
    mutate(
      id = row_number(), .before = 1
    ) %>%
    mutate(
      across(.data$gestartet_am:.data$zuletzt_aktualisiert_am, dmy_hms),
      across(.data$gestartet_am:.data$zuletzt_aktualisiert_am, as_date)
    ) %>%
    mutate(
      datum = .data$zuletzt_aktualisiert_am,
      .after = .data$zuletzt_aktualisiert_am
    )


  write_rds(patient_import, "02 Data/therapie-verlauf.rds")
  write_excel_csv2(patient_import, "02 Data/therapie-verlauf.csv")
}
