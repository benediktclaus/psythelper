#' Binded Function To Set Up Patient Project
#'
#' Function used by RStudio to setup a patient project.
#'
#' @param path Path, where project should be created
#' @param ... Optional arguments
#'
#' @importFrom fs path dir_create file_show
#' @importFrom purrr walk
#' @importFrom benelib use_custom_folder
#'
#' @noRd
patient_project <- function(path, ...) {
  arguments <- list(...)

  # Create project folder
  dir_create(path = path)


  # Create subfolders
  folder_list <- list("Raw Data", "Data", "R", "Figures")
  folder_list %>%
    walk(~ use_custom_folder(path, folder_name = .))


  # Create Analyses File if checked
  if (arguments[["analyses"]])       use_patient_analysis_template(path(path, "03 R"))
  if (arguments[["open_data_folder"]]) file_show(path(path, "01 Raw Data"))
}

#' Create an analysis file
#'
#' @param folder Folder path
#'
#' @importFrom fs path_package path_wd file_copy
#'
#' @return Called for side effects.
use_patient_analysis_template <- function(folder = NA) {
  # Check correct folder name format
  if (!is.na(folder) & !is.character(folder)) stop("The folder name must be a string.")
  template_path <- path(path_package("psythelper"), "templates", "analyses.R")


  # If folder is defined, use it
  # If not, use default folder "03 R"
  if (!is.na(folder)) {
    destination_path <- path(path_wd(), folder)
  } else {
    destination_path <- path(path_wd(), "03 R")
  }


  file_copy(template_path, destination_path)
}
