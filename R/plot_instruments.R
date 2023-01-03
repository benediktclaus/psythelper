#' Plot Instruments
#'
#' @param data A tidy dataframe containing at least columns "datum" and a
#'   supported instrument
#' @param datum Date column, defaults to "datum"
#' @param instrument An instrument column
#' @param x_label x-axis label
#' @param y_label y-axis label
#' @param y_limit Upper y-axis label, defaults to 0
#' @param filename File name for saving
#' @param fig_path Path to folder to save to, defaults to `"04 Figures/"`
#' @param fig_width Figure width, defaults to 15
#' @param fig_height Fig height, defaults to 10
#' @param fig_units Dimension units, defaults to `"cm"`
#'
#' @importFrom ggplot2 ggplot geom_line geom_point scale_x_date expand_limits labs aes_
#' @importFrom scales label_date_short
#' @importFrom benelib save_custom_plot
#'
#' @return A ggplot2 object
.plot_instruments <- function(data, datum = "datum", instrument, x_label = "Datum", y_label = "Instrument", y_limit = 0, filename = "test.png", fig_path = "04 Figures/", fig_width = 15, fig_height = 10, fig_units = "cm") {
  plot <- data %>%
    ggplot(aes_(as.name(datum), as.name(instrument))) +
    geom_line() +
    geom_point() +
    scale_x_date(labels = scales::label_date_short()) +
    expand_limits(y = c(0, y_limit)) +
    labs(x = x_label, y = y_label)

  save_custom_plot(filename = filename, plot = plot, path = fig_path, width = fig_width, height = fig_height, units = fig_units)
}

#' Plot MoM-DI
#'
#' @inheritParams .plot_instruments
#'
#' @return A ggplot2 object
.plot_mom_di <- function(data) {
  .plot_instruments(data, instrument = "mom_di", y_label = "MoM-DI", y_limit = 19*3, filename = "MoM-DI.png")
}

#' Plot MoM-AI
#'
#' @inheritParams .plot_instruments
#'
#' @return A ggplot2 object
.plot_mom_ai <- function(data) {
  .plot_instruments(data, instrument = "mom_ai", y_label = "MoM-AI", y_limit = 24*3, filename = "MoM-AI.png")
}

#' Plot WHO-5
#'
#' @inheritParams .plot_instruments
#'
#' @return A ggplot2 object
.plot_who_5 <- function(data) {
  .plot_instruments(data, instrument = "who_5", y_label = "WHO-5", y_limit = 20*5, filename = "WHO-5.png")
}

#' Plot GAD-7
#'
#' @inheritParams .plot_instruments
#'
#' @return A ggplot2 object
.plot_gad_7 <- function(data) {
  .plot_instruments(data, instrument = "gad_7", y_label = "GAD-7", y_limit = 7*3, filename = "GAD-7.png")
}

#' Plot WI-d
#'
#' @inheritParams .plot_instruments
#'
#' @return A ggplot2 object
.plot_wi_d <- function(data) {
  .plot_instruments(data, instrument = "wi_d", y_label = "WI-D", y_limit = 14, filename = "WI-D.png")
}

#' Plot OCI-R
#'
#' @inheritParams .plot_instruments
#'
#' @return A ggplot2 object
.plot_oci_r <- function(data) {
  .plot_instruments(data, instrument = "oci_r", y_label = "OCI-R", y_limit = 4*18, filename = "OCI-R.png")
}

#' Plot SIAS
#'
#' @inheritParams .plot_instruments
#'
#' @return A ggplot2 object
.plot_sias <- function(data) {
  .plot_instruments(data, instrument = "sias", y_label = "SIAS", y_limit = 4*20, filename = "SIAS.png")
}


#' Plot FET
#'
#' @param fig_height Figure height, defaults to 15
#' @param filename File name for saving, defaults to `"FET.png"`
#' @inheritParams .plot_instruments
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr starts_with case_when
#' @importFrom ggplot2 scale_y_continuous
#'
#' @return A ggplot2 object
.plot_fet <- function(data, filename = "FET.png", fig_path = "04 Figures/", fig_width = 15, fig_height = 15, fig_units = "cm") {
  plot <- data %>%
    select(.data$id, .data$datum, starts_with("aenderung")) %>%
    pivot_longer(
      cols = starts_with("aenderung"),
      names_to = "outcome",
      values_to = "value"
    ) %>%
    mutate(
      outcome = case_when(
        outcome == "aenderung_groesse" ~ "Gr\u00f6\u00dfe",
        outcome == "aenderung_sicherheit" ~ "Sicherheit",
        outcome == "aenderung_geschwindigkeit" ~ "Geschwindigkeit"
      ),
      outcome = as_factor(.data$outcome)
    ) %>%
    ggplot(aes(.data$datum, .data$value, color = .data$outcome)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    scale_x_date(labels = scales::label_date_short()) +
    # facet_wrap(~ outcome, ncol = 1) +
    expand_limits(y = c(0, 100)) +
    labs(x = "Datum", y = "Erwartung", color = "Skala")

  save_custom_plot(plot, filename = filename, path = fig_path, height = fig_height, width = fig_width, units = fig_units)
}

#' Plot Burden
#'
#' @param fig_height Figure height, defaults to 15
#' @param filename File name for savong, defaults to `"Belastung.png"`
#' @inheritParams .plot_instruments
#'
#' @importFrom forcats as_factor
#' @importFrom ggplot2 aes
#'
#' @return A ggplot2 object
.plot_burden <- function(data, filename = "Belastung.png", fig_path = "04 Figures/", fig_width = 15, fig_height = 15, fig_units = "cm") {
  plot <- data %>%
    select(.data$id, .data$datum, starts_with("belastung")) %>%
    pivot_longer(
      cols = starts_with("belastung"),
      names_to = "outcome",
      values_to = "value"
    ) %>%
    mutate(
      outcome = case_when(
        outcome == "belastung_arbeit_ausbildung" ~ "Arbeit/Ausbildung",
        outcome == "belastung_freizeit_sozialleben" ~ "Sozialleben",
        outcome == "belastung_familie_haus" ~ "Familie/Haus"
      ),
      outcome = as_factor(.data$outcome)
    ) %>%
    ggplot(aes(.data$datum, .data$value, color = .data$outcome)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    scale_x_date(labels = scales::label_date_short()) +
    # facet_wrap(~ outcome, ncol = 1) +
    expand_limits(y = c(0, 100)) +
    labs(x = "Datum", y = "Belastung", color = "Bereich")

  save_custom_plot(plot, filename = filename, path = fig_path, height = fig_height, width = fig_width, units = fig_units)
}

#' Plot Therapy success
#'
#' @param filename File name for savong, defaults to `"Therapieerfolg.png"`
#' @inheritParams .plot_instruments
#'
#' @return A ggplot2 object
.plot_success <- function(data, filename = "Therapieerfolg.png", fig_path = "04 Figures/", fig_width = 15, fig_height = 10, fig_units = "cm") {
  plot <- data %>%
    ggplot(aes(.data$datum, .data$therapieerfolg)) +
    geom_line() +
    geom_point() +
    scale_x_date(labels = scales::label_date_short()) +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    expand_limits(y = c(0, 100)) +
    labs(x = "Datum", y = "Therapieerfolg")

  save_custom_plot(plot = plot, filename = filename, path = fig_path, height = fig_height, width = fig_width, units = fig_units)
}
