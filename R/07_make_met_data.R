# extract zipped data from NHD and filter to comids all in same function (not looped)
# R. Peek

#library(vroom)
#library(glue)
#library(dplyr)
#library(fs)

# requires:
## path to zipped file with a COMID column
## list of the comids to filter by
## out directory

f_make_met_data <- function(filelist, outdir){

  # check if file exists
  if(file.exists(glue("{outdir}/met_seasonal_metrics.csv"))){
    print("met_seasonal_metrics exists, overwriting...")
  }
  # filelist should be list of trimmed csvs (by)
  ppt_files <- filter(filelist,
                      grepl("(?=.*ppt)(?=.*cat)",
                            x = filename, perl=TRUE, ignore.case = TRUE))

  tav_files <- filter(filelist,
                      grepl("(?=.*tav)(?=.*cat)",
                            x = filename, perl=TRUE, ignore.case = TRUE))

  run_files <- filter(filelist,
                      grepl("(?=.*run)(?=.*cat)",
                            x = filename, perl=TRUE, ignore.case = TRUE))

  # combine files of interest:
  dat_files <- bind_rows(ppt_files, tav_files, run_files)

  # use purrr to read all in and combine?
  met_dat <- map(dat_files$path, ~read_csv(.x, show_col_types = FALSE))
  met_dat <- map(met_dat, ~select(.x, (COMID:filename))) %>%
    map(~select(.x, -c(filename)))

  # inspect data
  #map(met_dat, ~names(.x))
  #map(met_dat, ~ncol(.x))
  #map(met_dat, ~pluck(.x, "COMID")) %>% map(., ~class(.x))

  # bind together
  met_df <- met_dat %>% reduce(left_join, by="COMID")

  # get list of names only for use elsewhere
  met_all <- names(met_df) %>% as_tibble()

  # make long for easier calc
  met_df_long <- met_df %>%
    # drop 2016 and 2017 first:
    dplyr::select(-c(matches("2016|2017"))) %>%
    # then split and pivot
    pivot_longer(!COMID,
                 names_to = c("metric", "month","wa_yr"),
                 names_pattern = "CAT_([[:alpha:]]{3})_([[:alpha:]]{3})([[:digit:]]{4})",
                 values_to = "value") %>%
    janitor::clean_names() %>%
    # fix names to reflect sample model needs
    mutate("comid_wy" = glue("{comid}_{wa_yr}"), .after="comid",
           "wa_yr" = as.integer(wa_yr),
           var_mon_wy = tolower(glue("{metric}_{month}_wy"))) %>%
    select(comid, comid_wy, wa_yr, month, metric, var_mon_wy, value) %>%
    # add pwy
    arrange(comid, var_mon_wy, wa_yr) %>%
    mutate(value_pwy = lag(value))

  # calculate the seasonal pieces
  met_df_seas <- met_df_long %>%
    mutate(season = case_when(
      grepl("JAN|FEB|MAR", month, ignore.case = TRUE) ~ "wint",
      grepl("APR|MAY|JUN", month, ignore.case = TRUE) ~ "sprg",
      grepl("JUL|AUG|SEP", month, ignore.case = TRUE) ~ "summ",
      grepl("OCT|NOV|DEC", month, ignore.case = TRUE) ~ "fall"
    )) %>%
    group_by(comid_wy, metric) %>%
    mutate(ann_wy = sum(value),
           ann_pwy = sum(value_pwy)) %>%
    ungroup() %>%
    group_by(comid_wy, metric, season) %>%
    mutate(seas_wy = sum(value),
           seas_pwy = sum(value_pwy)) %>%
    ungroup()

  # now make by water year and pivot wider
  met_metrics_wy <- met_df_seas %>%
    pivot_wider(id_cols = c(comid, comid_wy, wa_yr),
                names_from = c(metric, month),
                names_glue = "{metric}_{month}_{.value}",
                values_from = c(value, value_pwy, ann_wy, ann_pwy)) %>%
    rename_with(tolower) %>%
    rename_with(., ~gsub("_value$", "_wy", .x, perl = TRUE)) %>%
    rename_with(., ~gsub("_value_", "_", .x)) %>%
    # probably better way to do this but easy for now...
    # just pull one ann col and rename
    select(comid:tav_sep_pwy,
           ppt_ann_wy = ppt_apr_ann_wy,
           run_ann_wy = run_apr_ann_wy,
           tav_ann_wy = tav_apr_ann_wy,
           ppt_ann_pwy = ppt_apr_ann_pwy,
           run_ann_pwy = run_apr_ann_pwy,
           tav_ann_pwy = tav_apr_ann_pwy
    )

  # same for seasonal
  met_seas_wy <- met_df_seas %>%
    select(comid:wa_yr, metric, season, seas_wy, seas_pwy) %>%
    distinct(.keep_all = TRUE) %>%
    pivot_wider(id_cols = c(comid, comid_wy, wa_yr),
                names_from = c(metric, season),
                names_glue = "{metric}_{season}_{.value}",
                values_from = c(seas_wy, seas_pwy)) %>%
    rename_with(tolower) %>%
    rename_with(., ~gsub("_seas_wy$", "_wy", .x, perl = TRUE)) %>%
    rename_with(., ~gsub("_seas_pwy", "_pwy", .x))


  # join and add quarterly sums
  met_df_full <- left_join(met_metrics_wy, met_seas_wy) %>%
    ## add quarterly sums: sum1/sum2/sum3/sum4
    #sum1 <- ann_wy + ann_pwy
    #sum2 <- ann_wy + summ_pwy
    #sum3 <- ann_wy + sprg_pwy
    #sum4 <- ann_wy + wint_pwy
    mutate(ppt_sum1 = ppt_ann_wy + ppt_ann_pwy,
           ppt_sum2 = ppt_ann_wy + ppt_summ_pwy,
           ppt_sum3 = ppt_ann_wy + ppt_sprg_pwy,
           ppt_sum4 = ppt_ann_wy + ppt_wint_pwy,
           #tav
           tav_sum1 = tav_ann_wy + tav_ann_pwy,
           tav_sum2 = tav_ann_wy + tav_summ_pwy,
           tav_sum3 = tav_ann_wy + tav_sprg_pwy,
           tav_sum4 = tav_ann_wy + tav_wint_pwy,
           #run
           run_sum1 = run_ann_wy + run_ann_pwy,
           run_sum2 = run_ann_wy + run_summ_pwy,
           run_sum3 = run_ann_wy + run_sprg_pwy,
           run_sum4 = run_ann_wy + run_wint_pwy)

  # get sample names and reorder
  input_sample <- read_csv("data_input/example_input_10000042.csv") %>%
    janitor::clean_names()
  met_final <- met_df_full %>%
    select(any_of(names(input_sample))) # match col order to input
  # n=105

  # export
  #write_rds(met_final, file = glue("{outdir}/met_seasonal_metrics.rds"))
  write_csv(met_final, file = glue("{outdir}/met_seasonal_metrics.csv"))

  #met_out <- read_csv(glue("{outdir}/met_seasonal_metrics.csv"))
  return(met_final)
}
