#' Get RDS of SciBase Files for NHD Catchment Attributes
#'
#' @description Function to pull relevant download URL and file name from SciBase
#'
#' @param doi A web URL of the DOI to a valid ScienceBase entry
#' @param ... see  sbtools and Science Base for more
#' information on the options accepted.
#'
#' @value returns and saves an RDS file with URLs and names of files from SciBase Link.
#'
#' @example get_scibase_urls(doi="10.5066/F7765D7V")
#'

# get rds of SciBase Files
#library(sbtools)
#library(dplyr)

# specify a DOI (e.g., "10.5066/F7765D7V")
#doi <- "10.5066/F7765D7V"
f_get_scibase_urls <- function(doi, ...){
  # get the DOI info
  doi_info <- sbtools::query_sb_doi(doi, limit = 5000)
  # get id for items
  id_item <- doi_info[[1]]$id # i.e,. item ID: 5669a79ee4b08895842a1d47

  # list ALL files (takes a min)
  all_files <- sbtools::item_list_files(id_item, recursive = TRUE)
  # add file sizes for future viewing
  all_files <- all_files %>%
    dplyr::mutate(size_mb = size * 1e-6, .after=size)
  glue::glue("There are {nrow(all_files)} files in total.")

  fs::dir_create("data_input")
  clean_doi <- gsub("\\/", "_",x = doi, perl = TRUE)

  readr::write_rds(all_files, file = here::here(glue::glue("data_input/sb_files_doi_{clean_doi}.rds")))

}
