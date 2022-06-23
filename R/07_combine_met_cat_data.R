# extract zipped data from NHD and filter to comids all in same function (not looped)
# R. Peek

library(vroom)
library(glue)
library(dplyr)
library(fs)

# requires:
## filtered files with a COMID column
# metdata <- met_data
# catchment_dat <- revised_catchments_north[["catchments"]]
# outdir <- "data_output"
# modelname <- "north"

# use to return: make_cat_ffc_data
f_combine_met_cat_data <- function(metdata, catchment_dat, outdir, modelname){

  # metdata <- read_csv(glue("{here('data_output/met_seasonal_metrics_{modelname}.csv')}"))

  # get the files from the scibase_nhd location
  filelist <- get_zip_list(glue("{outdir}/scibase_nhd/"), extension = "*.csv")

  # filelist should be list of trimmed csvs, remove the met data
  nonmet_files <- filter(filelist,
                      !grepl("(?=.*ppt)(?=.*cat)",
                            x = filename, perl=TRUE, ignore.case = TRUE),
                        !grepl("(?=.*tav)(?=.*cat)",
                            x = filename, perl=TRUE, ignore.case = TRUE),
                        !grepl("(?=.*run)(?=.*cat)",
                               x = filename, perl=TRUE, ignore.case = TRUE))

  # read in
  ffc_df <- map(nonmet_files$path, ~read_csv(.x, show_col_types = FALSE)) %>%
    reduce(left_join, by="COMID") %>%
    select(-c(starts_with("filename"), contains("NODATA"))) %>%
    janitor::clean_names()

  # get list of names only for use elsewhere
  ffc_names <- names(ffc_df) %>% as_tibble()

  # get xwalk names and compare
  xwalk <- read_csv("data_input/08_accumulated_final_xwalk.csv")

  # names to keep
  ffc_to_keep <- ffc_names[ffc_names$value %in% xwalk$dat_output,]

  # join data
  ffc_final <- left_join(metdata, ffc_df, by="comid") %>%
    # add elev_rng:
    mutate(elv_rng = cat_elev_max-cat_elev_min, .after="cat_elev_max") %>%
    # select cols in xwalk
    select(matches(xwalk$dat_output)) #%>%
    # add back in specifics that are missing?
    #left_join(., ffc_df %>% select(comid, tot_ewt, tot_wtdep), by="comid")

  #names(ffc_final) %>% as_tibble() %>% View(title = "finnames")
  xwalk[!xwalk$dat_output %in% names(ffc_final),] %>% View()

  # now select vars
  cat_ffc_data <- ffc_final %>%
    left_join(catchment_dat %>% dplyr::select(comid, area, totda, area_weight), by="comid") %>%
    select(comid, totda, area_sf=area, area_weight, comid_wy, everything())

  #names(cat_ffc_data) %>% as_tibble() %>% View(title = "finnames2")

  # what's missing or named differently?
  #xwalk[!xwalk$mod_input_final %in% names(cat_ffc_data),c(1:4,6)] %>% arrange(accum_op_class) %>% View()

  # these can all be calculated and added in next script

  # export
  write_csv(cat_ffc_data, file = glue("{outdir}/ffc_combined_metrics_raw_{modelname}.csv"))

  # write_rds(met_final, file = glue("{outdir}/met_seasonal_metrics_{modelname}.rds"))
  return(cat_ffc_data)

}
