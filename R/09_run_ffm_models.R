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

## test run, specify metrics
# ffm_metrics <- c("Peak_2", "Peak_5", "Peak_10")

f_run_ffm_models <- function(accum_data, met_data_file, xwalk, ffm_metrics){

  ## Load reference data ("data_input/met.csv.zip") and drop unused vars
  met <- read_csv(glue("{met_data_file}")) %>% #clean_names() %>%
    # drop these vars (not used/important in model)
    select(-c("pmpe", "BDMAX", "PMAX_WS", "TMIN_WS", "PERMH_WS", "PMIN_WS"))

  # fix names of accum vars to match using xwalk
  # sum(names(accum_data) %in% xwalk$mod_input_final) all names exist
  # arrange so exactly same order as xwalk
  accum_data <- accum_data %>% dplyr::select(xwalk$mod_input_final)
  # identical(names(accum_data), xwalk$mod_input_final) # should be true
  # now rename to match MET data in models
  names(accum_data) <- xwalk$mod_input_raw

  ## Split by COMID
  dat_split <- accum_data %>%
    split(.$COMID) #%>%
  # names(dat_split) # check each is named

  # Specify metrics:
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

  # else everything else is whatever is typed
  curmets <- c(ffm_metrics) # ribbit ribbit
  print(curmets)

  # run random forest model: this takes awhile
  rfs <- map(curmets, ~f_run_rf_model(.x, met))

  # save model objects (large!)
  #save(rfs, file = "data_output/ffm_rf_ref_models_for_preds.rda", compress = "xz")
  #return("data_output/ffm_rf_ref_models_for_preds.rda")
  #return(rfs)
  rfs_lst <- list.files("data_output/ffm_models/", pattern = "*.rds")
  return(rfs_lst)
}
