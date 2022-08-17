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
      mom_di = rowSums(across({{ first_variable }} : {{ last_variable }}))
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
.evaluate_wi_d <- function(data, first_variable = .data$x1_machen_sie_sich_oft_sorgen_moeglicherweise_eine_ernsthafte_krankheit_zu_haben, last_variable = .data$x14_haben_sie_angst_krank_zu_werden) {
  data %>%
    mutate(
      across({{ first_variable }} : {{ last_variable }},
             ~ case_when(. == "Nein" ~ 0, . == "Ja" ~ 1)),
      wi_d = rowSums(across({{ first_variable }} : {{ last_variable }}))
    )
}
