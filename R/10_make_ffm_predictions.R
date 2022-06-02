# 10_RUN MODELS

#### Apply FFM Models to REFERENC DATA FROM CA NHD Network
#### Version 3.3 updated Mar 10, 2022 by Ryan Peek (rapeek@ucdavis.edu)
#### Version 3.2, updated September 21, 2021 by Ted Grantham (tgrantham@berkeley.edu)


# LIBRARIES ---------------------------------------------------------------

# library(dplyr)
# library(readr) # read zipped csv
# library(glue) # better pasting of things together
# library(fs) # more stable file paths
# library(purrr)
# library(janitor)

# REQUIRES:
# source("code/f_run_rf_model.R")
# source("code/f_make_ffm_preds.R")
# source("code/f_write_ffm_out.R")

# FFM METRIC GROUPS ------------------------------------------------------

# Here is the list of metrics to predict across the stream network:

## MAG metrics:
#metrics_mag <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50")

## PEAK MAG metrics:
#metrics_peak <- c("Peak_2","Peak_5","Peak_10")

## NON-PEAK, NON-MAG:
#metrics_nonpeakmag <- c("FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur","SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS", "Peak_Dur_2","Peak_Fre_2", "Peak_Dur_5","Peak_Fre_5", "Peak_Dur_10","Peak_Fre_10")

# rfs_list <- ffm_mods
# accum_data <- accum_data
# xwalk <- xwalk
# ffm_metrics <- ffm_metrics
# outdir <- "ffm_predictions"
# modelname <- "north"


f_make_ffm_predictions <- function(rfs_list, accum_data, xwalk, ffm_metrics, outdir, modelname){

  if(!length(rfs_list[grepl(paste(ffm_metrics, collapse = "|"), rfs_list)]) == length(ffm_metrics)){
    stop("Missing rf model files...")
  } else(
    print(glue('{length(rfs_list[grepl(paste(ffm_metrics, collapse = "|"), rfs_list)])} of {length(ffm_metrics)} metrics have rf model files...')))

  # now rename to match MET data in models
  accum_data <- accum_data %>% select(xwalk$mod_input_final)
  names(accum_data) <- xwalk$mod_input_raw

  ## Split Accumulation Data by COMID
  dat_split <- accum_data %>%
    split(.$COMID) #%>%

  # Specify ffm_metrics:
  if(missing(ffm_metrics)){
    ffm_metrics <-
      c(
        # mag
        ffm_mag <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50"),
        # peak
        ffm_peak <- c("Peak_2","Peak_5","Peak_10"),
        # non peak/non mag
        ffm_non <- c("FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur",
                     "SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS",
                     "Peak_Dur_2","Peak_Fre_2", "Peak_Dur_5","Peak_Fre_5",
                     "Peak_Dur_10","Peak_Fre_10"))
  }

  # else everything else is whatever is typed individually
  curmets <- c(ffm_metrics) # ribbit ribbit
  print(curmets)

  # make predictions, condense, and export
  for(i in seq_along(curmets)){

    print(glue("Working on {curmets[i]}"))

    # read in rf model -----
    rf_mod <- readRDS(glue("data_output/ffm_models/rf_{curmets[i]}_ref_models_for_preds.rds"))

    # 1. Make preds ------------------------
    map(dat_split, ~f_run_rf_predictions(metric = curmets[i],
                                         rf = rf_mod,
                                         comid_data = .x , outdir,
                                         modelname))

    # 4. Export files ----------------------
    #f_write_ffm_out(nhd_metrics = nhd, metric = curmets[i])
    # 5. Remove temp files -----------------
    #fs::file_delete(fs::dir_ls("model_output/modresults"))

  }
  print("All models completed!")

  # get list of filepaths for next step
  filepaths <- fs::dir_ls(glue("data_output/{outdir}"), glob = "*.csv")

}
