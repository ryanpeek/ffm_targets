# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
#library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tidyverse", "glue","sf","nhdplusTools", "purrr","sbtools",
               "here","fs","vroom", "rmapshaper","lwgeom"), # packages that your targets need to run
  format = "rds" # default storage format
)

# Load all functions:
for (file in list.files("R", full.names = TRUE)) source(file)

# Load function 1 by 1, # Source other scripts as needed.
# source("R/01_get_scibase_urls.R")
# source("R/02_download_scibase.R")
# source("R/03_clean_nhd_flowlines.R")
# source("R/f_extract_to_comids.R")

# Replace the target list below with your own:
list(
  tar_target(scibase_urls,
             f_get_scibase_urls("10.5066/F7765D7V"), format="rds"),
  tar_target(scibase_url_file, scibase_urls, format = "rds"),
  tar_target(xwalk,
             readr::read_csv("data_input/08_accumulated_final_xwalk.csv")),
  tar_target(scibase_to_download,
             # filter to filenames of interest
             scibase_url_file %>%
               filter(grepl("^PPT[0-9]{4}_CAT_CONUS", fname)|
                        grepl("^TAV[0-9]{4}_CAT_CONUS", fname)|
                        grepl("^RUN[0-9]{4}_CAT_CONUS", fname)|
                        fs::path_ext_remove(fname) %in%
                        fs::path_ext_remove(xwalk$source_file))),
  tar_target(scibase_filelist,
             f_download_scibase(scibase_to_download, "data_input/scibase_nhd"),
             cue = tar_cue("never")),
  tar_target(get_comids,
             f_get_nhd_comids("nhd_flowlines_vaa.rds")),
  tar_target(scibase_comids,
             purrr::map(scibase_filelist$path,
                        ~f_extract_to_comids(.x,
                                             comids = get_comids$comid,
                                             outdir = "data_output/scibase_nhd"))),
  tar_target(scibase_comid_csvs,
              get_zip_list("data_output/scibase_nhd/", "*csv")),
  tar_target(make_met_data,
             f_make_met_data(scibase_comid_csvs, "data_output"))
)

# tar_glimpse()
# tar_make()
# tar_validate()
# tar_visnetwork()
