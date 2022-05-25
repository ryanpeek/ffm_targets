# R. Peek 2022
# Created by use_targets().

# Load packages required to define the pipeline:
# library(tidyverse)
# library(glue)
# library(fs)
library(targets)
library(tarchetypes) # Load other packages as needed

# Set target options:
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tidyverse", "glue", "here", "fs",
               "purrr","sbtools", "units", "janitor",
               "randomForest",
               "vroom", "sf","nhdplusTools",
               "rmapshaper","lwgeom"), # packages that your targets need to run
  format = "rds" # default storage format
)
options(dplyr.summarise.inform = FALSE)

# Load all functions (or source one by one if needed)
for (file in list.files("R", full.names = TRUE)) source(file)

# specify metrics:
## mag
# ffm_mag <- c("FA_Mag",
#              "Wet_BFL_Mag_50","Wet_BFL_Mag_10",
#              "SP_Mag",
#              "DS_Mag_90","DS_Mag_50")

## peak
#ffm_peak <- c("Peak_2","Peak_5","Peak_10")


## non peak/non mag
# ffm_non <- c("FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur",
#              "SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS",
#              "Peak_Dur_2","Peak_Fre_2", "Peak_Dur_5","Peak_Fre_5",
#              "Peak_Dur_10","Peak_Fre_10")

# all

ffm_metrics <- c("FA_Mag", "Wet_BFL_Mag_50","Wet_BFL_Mag_10",
                 "SP_Mag","DS_Mag_90","DS_Mag_50",
                 "Peak_2","Peak_5","Peak_10",
                 "FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur",
                 "SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS",
                 "Peak_Dur_2","Peak_Fre_2", "Peak_Dur_5","Peak_Fre_5",
                 "Peak_Dur_10","Peak_Fre_10")

# Target List (Steps of Workflow) -----------------------
list(

  ## STEP 1: get urls for the DOI in question ------------
  tar_target(scibase_urls,
             f_get_scibase_urls("10.5066/F7765D7V", "data_input")),

  # load the crosswalk for variable names/files
  tar_target(xwalk,
             readr::read_csv("data_input/08_accumulated_final_xwalk.csv")),

  ## STEP 2: filter and download raw files that match xwalk file vars --------
  tar_target(scibase_to_download,
             scibase_urls %>%
               filter(grepl("^PPT[0-9]{4}_CAT_CONUS", fname)|
                        grepl("^TAV[0-9]{4}_CAT_CONUS", fname)|
                        grepl("^RUN[0-9]{4}_CAT_CONUS", fname)|
                        fs::path_ext_remove(fname) %in%
                        fs::path_ext_remove(xwalk$source_file))),
  # Download once
  tar_target(scibase_filelist,
             f_download_scibase(scibase_to_download, "data_input/scibase_nhd"),
             cue = tar_cue("never")),

  ## STEP 3: load nhd flowlines and revise catchments --------------------

  # can change this function to f_revise_catchments_full for full catch
  # or to f_revise_catchments_north to drop terminal basins in south
  tar_target(revised_catchments_north,
             f_revise_catchments_north(indir = "data_input",
                                 outdir = "data_output",
                                 startcomid = "3917946")),
                                 #catch_input = "catchments_final_lshasta.rds")),


  ## STEP 4: filter NHD science base data to comids of interest ----------
  tar_target(filter_scibase_comids,
             purrr::map(scibase_filelist$path,
                        ~f_extract_to_comids(.x,
                                             comids = revised_catchments_north[["flowlines"]]$comid,
                                             outdir = "data_output/scibase_nhd")) %>%
               pluck(., 1)),

  ## STEP 5: make krug_runoff csv  -----------------------------------------
  tar_target(krug_data,
              f_get_krug_data("data_input/scibase_nhd/krug_runoff_avg_ann_1951-1980.e00",
                              "data_output/nhdplus_full_vaa.gpkg",
                              revised_catchments_north[["flowlines"]],
                              "data_output/scibase_nhd/")),

  ## STEP 6: make the seasonal precip/tav/run variables --------------------
  tar_target(met_data,
             f_make_met_data(filter_scibase_comids, "data_output", "north")),


  # STEP 7: combine all the data -------------------------------------------
  tar_target(cat_ffc_data,
             f_combine_met_cat_data(met_data,
                                    revised_catchments_north[["catchments"]],
                                    "data_output",
                                    "north")),

  # STEP 8: make the accumulated data for comids ---------------------------
  tar_target(accum_data,
             f_calc_accum_data(cat_ffc_data,
                               revised_catchments_north[["comlist_accum"]],
                               xwalk,
                               "data_output",
                               "north")),

  # STEP 9: Run the model: TAKES A LONG TIME -------------

  ## this step can take a while. Go make coffee/tea.
  ## Defaults to running for ALL metrics, but can specify
  ## if you don't specify a metric(s), defaults to all 24.
  ## If models run once they shouldn't change and this is skipped.
  tar_target(ffm_mods,
             f_run_ffm_models(accum_data,
                              "data_input/met.csv.zip", xwalk,

                              ffm_metrics)),


  # STEP 10: Predict FFM from model ----------------------

  tar_target(ffm_predictions,
             f_make_ffm_predictions(ffm_mods, accum_data,
                                    xwalk,
                                    ffm_metrics,
                                    "ffm_predictions",
                                    "north")),

  # STEP 11: COMBINE DATA IN SUMMARY ----------------------
  tar_target(ffm_final_summary,
             f_combine_ffm(ffm_predictions,
                           outdir = "data_output/ffm_preds_summary",
                           filename = "final_ffm_preds_lshasta_north"))

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
