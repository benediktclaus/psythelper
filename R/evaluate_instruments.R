#' Evaluate All Instruments
#'
#' @param data_path Wehre lies the data? Defaults to `"02 Data/therapie-verlauf.rds"`
#'
#' @importFrom stringr str_detect
#' @importFrom readr read_rds
#'
#' @return Does not really return anything, is mainly called for its side
#'   effects.
#' @export
evaluate_instruments <- function(data_path = "02 Data/therapie-verlauf.rds") {
  # Get data file
  data <- read_rds(data_path)

  # Get all instruments
  instruments <- .ext_instruments(data)


  # Convert responses and evaluate instruments
  if ("mom_di" %in% instruments) data <- .evaluate_mom_di(data)
  if ("mom_ai" %in% instruments) data <- .evaluate_mom_ai(data)
  if ("gad_7" %in% instruments) data <- .evaluate_gad_7(data)
  if ("who_5" %in% instruments) data <- .evaluate_who_5(data)
  if ("wi_d" %in% instruments) data <- .evaluate_wi_d(data)
  if ("oci_r" %in% instruments) data <- .evaluate_oci_r(data)
  if ("sias" %in% instruments) data <- .evaluate_sias(data)


  # Rename therapy success, if available
  if (any(str_detect(names(data), "therapieerfolg"))) {
    data <- data %>%
      rename(
        therapieerfolg = .data$therapieerfolg_sehr_viel_schlechter_100_sehr_viel_besser_100,
        belastung_arbeit_ausbildung = .data$arbeit_oder_ausbildung_gar_nicht_0_sehr_schwer_kann_nicht_mehr_arbeiten_100,
        belastung_freizeit_sozialleben = .data$freizeit_und_sozialleben_gar_nicht_0_sehr_schwer_100,
        belastung_familie_haus = .data$familienleben_und_haeusliche_pflichten_gar_nicht_0_sehr_schwer_100
      )

    instruments <- c(instruments, "therapieerfolg")
  }


  # Rename therapy expectations, if available
  if (any(str_detect(names(data), "aenderung_ich"))) {
    data <- data %>%
      rename(
        aenderung_groesse = .data$groesse_einer_aenderung_ich_erwarte_ueberhaupt_keine_aenderung_0_ich_erwarte_eine_vollstaendige_heilung_100,
        aenderung_sicherheit = .data$sicherheit_einer_aenderung_ich_bin_sehr_unsicher_ob_sich_etwas_veraendern_wird_0_ich_bin_absolut_sicher_dass_sich_etwas_veraendern_wird_100,
        aenderung_geschwindigkeit = .data$geschwindigkeit_einer_aenderung_ich_denke_es_wird_sofort_eine_aenderung_eintreten_0_ich_denke_eine_aenderung_wird_zeit_benoetigen_100
      )

    instruments <- c(instruments, "fet")
  }


  # If there are impairments, list them as instrument
  if (any(str_detect(names(data), "belastung"))) instruments <- c(instruments, "belastung")


  # Plot instruments
  if ("mom_di" %in% instruments) .plot_mom_di(data)
  if ("mom_ai" %in% instruments) .plot_mom_ai(data)
  if ("gad_7" %in% instruments) .plot_gad_7(data)
  if ("who_5" %in% instruments) .plot_who_5(data)
  if ("wi_d" %in% instruments) .plot_wi_d(data)
  if ("oci_r" %in% instruments) .plot_oci_r(data)
  if ("fet" %in% instruments) .plot_fet(data)
  if ("belastung" %in% instruments) .plot_burden(data)
  if ("therapieerfolg" %in% instruments) .plot_success(data)


  # Return an object of class "psythelper"
  results <- list(
    instruments = instruments,
    data = data
  )

  class(results) <- "psythelper"

  # Return list
  return(results)
}

#' Evaluate The MoM-DI
#'
#' Evaluate the Mind over Mood Depression Inventory.
#'
#' @param data A dataset containing all responded items for the Instrument in German.
#' @param first_variable Name of the first variable
#' @param last_variable Name of the last variable
#'
#' @return A tibble containing all items with a sum score
.evaluate_mom_di <- function(data, first_variable = .data$x1_traurige_oder_depressive_stimmung, last_variable = .data$x19_verringerter_sexualtrieb) {
  data %>%
    mutate(
      across({{ first_variable }} : {{ last_variable }},
             ~ case_when(. == "Nie" ~ 0, . == "Gelegentlich" ~ 1, . == "H\u00e4ufig" ~ 2, . == "Meistens" ~ 3)),
      mom_di = rowSums(across({{ first_variable }} : {{ last_variable }}))
    )
}

#' Evaluate the MoM-AI
#'
#' Evaluate the Mind over Mood Anxiety Inventory.
#'
#' @inheritParams .evaluate_mom_di
#'
#' @return A tibble containing all items with a sum score
.evaluate_mom_ai <- function(data, first_variable = .data$x1_nervoes, last_variable = .data$x24_furcht_dass_etwas_schreckliches_geschehen_wird) {
  data %>%
    mutate(
      across({{ first_variable }} : {{ last_variable }},
             ~ case_when(. == "Gar nicht" ~ 0, . == "Manchmal" ~ 1, . == "H\u00e4ufig" ~ 2, . == "Meistens" ~ 3)),
      mom_ai = rowSums(across({{ first_variable }} : {{ last_variable }}))
    )
}

#' Evaluate the GAD-7
#'
#' Evaluate the GAD-7 questionnaire.
#'
#' @inheritParams .evaluate_mom_di
#'
#' @return A tibble containing all items with a sum score
.evaluate_gad_7 <- function(data, first_variable = .data$x1_nervositaet_aengstlichkeit_oder_anspannung, last_variable = .data$x7_gefuehl_der_angst_so_als_wuerde_etwas_schlimmes_passieren) {
  data %>%
    mutate(
      across({{ first_variable }} : {{ last_variable }},
             ~ case_when(. == "\u00dcberhaupt nicht" ~ 0, . == "An einzelnen Tagen" ~ 1, . == "An mehr als der H\u00e4lfte der Tage" ~ 2, . == "Beinahe jeden Tag" ~ 3)),
      gad_7 = rowSums(across({{ first_variable }} : {{ last_variable }}))
    )
}

#' Evaluate the WHO-5
#'
#' Evaluate the World Health Organization well-being index.
#'
#' @inheritParams .evaluate_mom_di
#'
#' @return A tibble containing all items with a sum score
.evaluate_who_5 <- function(data, first_variable = .data$x1_war_ich_froh_und_guter_laune, last_variable = .data$x5_war_mein_alltag_voller_dinge_die_mich_interessieren) {
  data %>%
    mutate(
      across({{ first_variable }} : {{ last_variable }},
             ~ case_when(. == "Die ganze Zeit" ~ 5, . == "Meistens" ~ 4, . == "Etwas mehr als die H\u00e4lfte der Zeit" ~ 3, . == "Etwas weniger als die H\u00e4lfte der Zeit" ~ 2, . == "Ab und zu" ~ 1, . == "Zu keinem Zeitpunkt" ~ 0)),
      who_5 = rowSums(across({{ first_variable }} : {{ last_variable }})),
      who_5 = .data$who_5 * 4
    )
}

#' Evaluate the WI-d
#'
#' Evaluate the Whiteley Index in its German form.
#'
#' @inheritParams .evaluate_mom_di
#'
#' @return A tibble containing all items with a sum score
.evaluate_wi_d <- function(data, first_variable = .data$x1_machen_sie_sich_oft_sorgen_krank, last_variable = .data$x14_haben_sie_angst_krank_zu_werden) {
  data %>%
    mutate(
      across({{ first_variable }} : {{ last_variable }},
             ~ case_when(. == "Nein" ~ 0, . == "Ja" ~ 1)),
      wi_d = rowSums(across({{ first_variable }} : {{ last_variable }}))
    )
}

#' Evaluate the OCI-R
#'
#' Evaluate the Obsessive-Compulsive Inventory Revised in its German form.
#'
#' @inheritParams .evaluate_mom_di
#'
#' @return A tibble containing all items with a sum score
.evaluate_oci_r <- function(data, first_variable = .data$x1_ich_bewahre_so_viele_gegenstaende_auf_dass_sie_mich_behindern, last_variable = .data$x18_ich_bekomme_haeufig_abscheuliche_gedanken) {
  data %>%
    mutate(
      across({{ first_variable }} : {{ last_variable }},
             ~ case_when(. == "Gar nicht" ~ 0, . == "Wenig" ~ 1, . == "Mittel" ~ 2, . == "Stark" ~ 3, . == "Sehr stark" ~ 4)),
      oci_r = rowSums(across({{ first_variable }} : {{ last_variable }}))
    )
}

#' Evaluate the SIAS
#'
#' Evaluate the Social Interaction Anxiety Scale.
#'
#' @inheritParams .evaluate_mom_di
#'
#' @return A tibble containing all items with a sum score
.evaluate_sias <- function(data, first_variable = .data$x1_ich_werde_nervoes, last_variable = .data$x20_ich_bin_unsicher_ob) {
  data %>%
    mutate(
      across({{ first_variable }} : {{ last_variable }},
             ~ case_when(. == "Überhaupt nicht" ~ 0, . == "Ein wenig" ~ 1, . == "Ziemlich" ~ 2, . == "Stark" ~ 3, . == "Sehr stark" ~ 4)),
      sias = rowSums(across({{ first_variable }} : {{ last_variable }}))
    )
}
