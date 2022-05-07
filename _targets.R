# R. Peek 2022
# Created by use_targets().

# Load packages required to define the pipeline:
# library(tidyverse)
# library(glue)
# library(fs)
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tidyverse", "glue","sf","nhdplusTools",
               "purrr","sbtools", "units",
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
  # step 1: get urls for the DOI in question
  tar_target(scibase_urls,
             f_get_scibase_urls("10.5066/F7765D7V", "data_input")),

  # load the urls so they are available
  #tar_target(scibase_url_file, scibase_urls, format = "rds"),

  # load the crosswalk for variable names/files
  tar_target(xwalk,
             readr::read_csv("data_input/08_accumulated_final_xwalk.csv")),

  # step 2: filter to just files we need and download the files
  tar_target(scibase_to_download,
             scibase_urls %>%
               filter(grepl("^PPT[0-9]{4}_CAT_CONUS", fname)|
                        grepl("^TAV[0-9]{4}_CAT_CONUS", fname)|
                        grepl("^RUN[0-9]{4}_CAT_CONUS", fname)|
                        fs::path_ext_remove(fname) %in%
                        fs::path_ext_remove(xwalk$source_file))),
  # download
  tar_target(scibase_filelist,
             f_download_scibase(scibase_to_download, "data_input/scibase_nhd"),
             cue = tar_cue("never")),

  # step 3: load nhd flowlines and revise catchments
  tar_target(revised_catchments,
             f_revise_catchments(indir = "data_input",
                                 outdir = "data_output",
                                 catch_input = "catchments_final_lshasta.rds")),
  # step 4: get comids
  tar_target(comids,
             f_get_comids(revised_catchments[["flowlines"]])),

  # step 5: filter NHD science base data to comids of interest
  tar_target(filter_scibase_comids,
             purrr::map(scibase_filelist$path,
                        ~f_extract_to_comids(.x,
                                             comids = comids$comid,
                                             outdir = "data_output/scibase_nhd")) %>%
               pluck(., 1)),

  # step 6: make krug csv (requires having run revise_catchments.R)
  tar_target(krug_data,
              f_get_krug_data("data_input/scibase_nhd/krug_runoff_avg_ann_1951-1980.e00",
                              "data_output/nhdplus_vaa.gpkg",
                              revised_catchments[["flowlines"]],
                              "data_output/scibase_nhd/")),

  # step 7: make the seasonal precip/tav/run variables
  tar_target(met_data,
             f_make_met_data(filter_scibase_comids, "data_output")),


  # step 8: combine all the data!
  tar_target(cat_ffc_data,
             f_combine_met_cat_data(met_data, revised_catchments[["catchments"]], "data_output")),

  tar_target(accum_data,
             f_make_accumulation(cat_ffc_data,
                                 revised_catchments[["flowlines"]],
                                 xwalk, "data_output"))

)


## Interacting with targets

## quick overview of main components of workflow
# tar_glimpse()

## overview of workflow with links between
# tar_visnetwork()

## DO STUFF!
# tar_make()

## make sure data are valid and pipeline works
# tar_validate()

## to reset and rerun, use invalidate
# tar_invalidate(met_data)

## check warnings
# tar_meta(fields=warnings)
# just spatial warnings so all ok
