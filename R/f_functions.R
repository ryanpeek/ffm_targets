# f_functions for nhd flow attributes

# get_ziplist ----------------------------------

#' Function to get list of files in given folder
#' @param folder directory to look in
#' @param extension file extension (e.g., "*zip)
#' @return dataframe of files
#' @export
#' @example get_zip_list(glue("{dat_dir}/{subfold_dir}"), "*zip")

get_zip_list <- function(folder, extension, recurse=FALSE){
  #library(dplyr)
  #library(fs)
  #library(glue)

  fs::dir_info(folder,
               type = "file",
               regexp = glue::glue("{extension}"),
               recurse = recurse) %>%
    dplyr::select(path, size) %>%
    dplyr::mutate(filename = fs::path_file(path))
}

