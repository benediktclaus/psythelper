#' Clean the Most Recent Dataset
#'
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr select mutate across row_number rename
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
    filter(status == "Abgeschlossen") %>%
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

  instruments <- .ext_instruments(patient_import)
  if ("wi_d" %in% instruments) patient_import <- rename(patient_import, x1_machen_sie_sich_oft_sorgen_krank = .data$x1_machen_sie_sich_oft_sorgen_moeglicherweise_eine_ernsthafte_krankheit_zu_haben)
  if ("oci_r" %in% instruments) patient_import <- rename(patient_import, x18_ich_bekomme_haeufig_abscheuliche_gedanken = .data$x18_ich_bekomme_haeufig_abscheuliche_gedanken_und_es_faellt_mir_schwer_sie_wieder_loszuwerden)
  if ("sias" %in% instruments) patient_import <- rename(patient_import, x1_ich_werde_nervoes = .data$x1_ich_werde_nervoes_wenn_ich_mit_einer_autoritaetsperson_lehrer_vorgesetzten_sprechen_muss)
  if ("sias" %in% instruments) patient_import <- rename(patient_import, x20_ich_bin_unsicher_ob = .data$x20_ich_bin_unsicher_ob_ich_jemanden_gruessen_soll_den_ich_nur_oberflaechlich_kenne)


  write_rds(patient_import, "02 Data/therapie-verlauf.rds")
  write_excel_csv2(patient_import, "02 Data/therapie-verlauf.csv")
}
