#' Get Most Recent File Dataset in a Folder
#'
#' @param folder_path A folder path where the datafiles live. Defaults to "01
#'   Raw Data/"
#'
#' @importFrom fs dir_info
#' @importFrom dplyr slice_max pull `%>%`
#' @importFrom rlang .data
#'
#' @return A string (filename of most recent file)
.get_recent_dataset <- function(folder_path = "01 Raw Data/") {
  dir_info(folder_path) %>%
    slice_max(n = 1, order_by = .data$modification_time) %>%
    pull(.data$path)
}
