#' Read the most recent species list
#' 
#' This function reads the most recent species list from a specified CSV file.
#' 
#' @param file_path A string specifying the path to the CSV file containing the species list.
#' @param prius_version A integer specifying the version of PRIUS. Default is "2".
#' 
#' @details
#' If file_path = NULL (the default), the function will look for the species list in the
#' current working directory under the path "prius{prius_version}/temp/".
#' The function searches for files matching the pattern "prius{prius_version}_list_v" and reads the most recent one.
#' If no matching file is found, an error is raised.
#' 
#' @return A data frame containing the species list.
#' 
#' @examples
#' \dontrun{
#' species_list <- read_species_list("path/to/species_list/", TRUE)
#' }
#' 

read_species_list <- function(file_path = NULL,
                              prius_version = 2) {
  
  # Construct the file name based on PRIUS version
  file_pattern <- paste0("prius", prius_version, "_list_v")
  
  # Construct the full file path
  if(is.null(file_path)) {
    file_path <- file.path(getwd(), paste0("prius", prius_version, "/temp/"))
  }
  
  # List all files in the specified directory matching the pattern
  files <- list.files(path = file_path, pattern = file_pattern, full.names = TRUE)
  
  # Only keep CSV files
  csv_files <- files[grepl("\\.csv$", files)]
  
  # Convert to a dataframe
  if (length(csv_files) == 0) {
      stop("No species list found in the specified directory.")
  } else {
    latest_file <- data.frame(
      file_path = csv_files
    ) |>
      dplyr::mutate(
        version_date = as.Date(substr(basename(file_path), 
                              nchar(file_pattern) + 1, 
                              nchar(file_pattern) + 8), "%y-%m-%d")) |>
      dplyr::filter(!is.na(version_date)) |>
      dplyr::filter(version_date == max(version_date, na.rm = TRUE)) |>
      dplyr::pull(file_path)

    # Read the most recent species list
    species_list <- read.csv(latest_file, stringsAsFactors = FALSE)
  }
  
  return(species_list)
}