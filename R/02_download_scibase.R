#' Download ScienceBase files based on list of URLs
#'
#' @description Function to download SciBase data from list of URLs
#'
#' @param url_list A list of URLs of the DOI to a valid ScienceBase entry
#' @param dl_dir   where the data will be saved
#'
#' @value saves zipped files from SciBase Links
#'

# download SciBase Files
#library(purrr)
#library(glue)
#library(fs)

# download dir
#dl_dir <- "data_input/scibase_nhd"

f_download_scibase <- function(url_list, dl_dir){

  # check/clean dir
  if (fs::dir_exists(here(dl_dir))) {
    glue::glue("Directory {here(dl_dir)} exists!")
  } else {
    fs::dir_create(here(dl_dir))
    glue::glue("Directory {dl_dir} created")
  }

  # now download (this can take awhile)
  map2(url_list$url, url_list$fname,
       ~ if(!file.exists(glue("{here(dl_dir)}/{.y}"))){
         download.file(.x, glue("{here(dl_dir)}/{.y}"))}
       else(print("File exists")))

  print("Done with SciBase!")
  print("Moving to Krug Runoff...")

  # krug
  download.file("https://water.usgs.gov/GIS/dsdl/runoff.e00.zip",
                destfile = glue("{here(dl_dir)}/krug_runoff_avg_ann_1951-1980.e00.zip"))

  # unzip and make KRUG RUNOFF
  unzip(glue("{here(dl_dir)}/krug_runoff_avg_ann_1951-1980.e00.zip"),
        exdir = glue("{here(dl_dir)}"))

  # rename
  fs::file_move(path = glue("{here(dl_dir)}/runoff.e00"),
                new_path = glue("{here(dl_dir)}/krug_runoff_avg_ann_1951-1980.e00"))

  # deleted krug zip
  fs::file_delete(path = glue("{here(dl_dir)}/krug_runoff_avg_ann_1951-1980.e00.zip"))
  print("Done!")

  # now list the files!
  ziplist <- get_zip_list(glue("{here(dl_dir)}"), "*zip", recurse = FALSE)
  return(ziplist)
}
