# make catchment areas calculate additional variables
# using area weighted average (catch area / tot basin area)

# requires:
# the cleaned and prepped cat_ffc_data (raw by catchment/comid)
# tar_load(cat_ffc_data)
# cat_data <- cat_ffc_data
# tar_load(revised_catchments_north)
# comlist <- revised_catchments_north[["comlist_accum"]]
# tar_load(xwalk) # loads obj named "xwalk"
# outdir <- "data_output"
# modelname="north"

# trying this with essentially NO accumulation

f_calc_accum_data <- function(cat_data, comlist, xwalk, outdir, modelname){

  ## AWA -----------------------------------------
  # SUM of value * area weight + value * area weight

  # select all variables that need "avg of catch values"
  varnames_awa <- xwalk %>%
    filter(accum_op_class=="AWA") %>%
    select(dat_output)

  # filter to vars
  cat_df_awa <- cat_data %>%
    # drop the non info vars
    select(comid:wa_yr, varnames_awa$dat_output)

  # filter to dataframe that is list of all comids for a given comid
  dat_ls_awa <- map(comlist, ~filter(cat_df_awa, comid %in% .x) %>%
                      select(-c(comid, comid_wy)))

  dat_awa <- map(dat_ls_awa, ~group_by(.x, wa_yr) %>%
                   summarise(
                     across(
                       .cols  = ppt_jan_wy:krug_runoff,
                       ~sum(.x*area_weight),
                       .names = "{col}_awa")
                   ))

  # collapse as dataframe:
  dat_df_awa <- bind_rows(dat_awa, .id = "comid") %>%
    mutate(comid=as.numeric(comid)) %>%
    # fix the awa ending
    rename_with(~str_remove(., '_awa')) #%>%
  # rename calculated vars back to original variables of interest
  rename(ann_min_precip_basin = cat_minp6190,
         ann_max_precip_basin = cat_maxp6190,
         pptavg_basin = cat_ppt7100_ann,
         et_basin = cat_et,
         pet_basin = cat_pet,
         rh_basin = cat_rh,
         wtdepave = cat_wtdep)

  # rm temp files
  rm(dat_ls_awa, varnames_awa, cat_df_awa, dat_awa)

  ## MAX/MIN/RANGE/SUM----------------------------------------------------


  # # select all variables that need "avg of catch values"
  # varnames_oth <- xwalk %>%
  #   filter(accum_op_class %in% c("AVG", "MAX","MIN","RNG","SUM")) %>%
  #   filter(!dat_output %in% c("comid", "comid_wy", "wa_yr"))
  #
  # # filter to vars
  # cat_df_oth <- cat_data %>%
  #   # drop the non info vars
  #   select(comid:wa_yr, varnames_oth$dat_output)
  #
  # # filter to dataframe that is list of all comids for a given comid
  # dat_ls_oth <- map(comlist, ~filter(cat_df_oth, comid %in% .x) %>%
  #                     select(-c(comid, comid_wy)))
  #
  # # iterate over
  # dat_oth <- map(dat_ls_oth, ~group_by(.x, wa_yr) %>%
  #                  summarise(
  #                    # across(
  #                    #   # min cols
  #                    #   .cols  = c(cat_elev_min),
  #                    #   ~min(.x),
  #                    #   .names = "{col}_min"),
  #                    # across(
  #                    #   # max
  #                    #   .cols = c(cat_elev_max),
  #                    #   ~max(.x),
  #                    #   .names = "{col}_max"),
  #                    # # mean
  #                    # across(
  #                    #   .cols = c(cat_tav7100_ann),
  #                    #   ~mean(.x, na.rm=TRUE),
  #                    #   .names = "{col}_avg"),
  #                    across(
  #                      .cols = c(area_sf), # should = totda from catch data
  #                      ~sum(.x),
  #                      .names = "{col}_sum")
  #                  )
  # )

  # collapse and rename
  # dat_df_oth <- bind_rows(dat_oth, .id = "comid") %>%
  #   mutate(comid=as.numeric(comid)) %>%
  #   rename(area_sf = area_sf_sum)
  #
  # rm(dat_ls_oth, varnames_oth, cat_df_oth, dat_oth)

  ## ECO Dominant -------------------------------------------------------
  # this is no longer needed data already provides dom per catch
  # # this section no longer needed because calcs are more simple (catch vals)
  # RP, 2022-06-23

  # cat_df_eco <- cat_data %>%
  #   select(comid:wa_yr, eco3)
  #
  # # filter to dataframe that is list of all comids for a given comid
  # dat_ls_eco <- map(comlist, ~filter(cat_df_eco, comid %in% .x) %>%
  #                     select(-c(comid, comid_wy)))
  #
  # dat_eco <- map(dat_ls_eco, ~group_by(.x) %>%
  #                  summarise(
  #                    eco3_dom = names(which.max(table(eco3)))))
  #
  # dat_df_eco <- bind_rows(dat_eco, .id = "comid") %>%
  #   mutate(comid=as.numeric(comid)) %>%
  #   rename(eco3=eco3_dom)
  #
  # rm(dat_eco, cat_df_eco, dat_ls_eco)

  ## NO CALC ----------------------------------------------------------

  # need the no calc vars, subset and then join
  varnames_nocalc <- xwalk %>%
    filter(accum_op_class %in% c("NONE")) %>%
    filter(!dat_output %in% c("comid", "comid_wy", "wa_yr")) %>%
    select(dat_output)

  # subset data
  dat_df_nocalc <- cat_data %>%
    select(comid, wa_yr, varnames_nocalc$dat_output) %>%
    # rename
    rename(pptavg_cat = cat_ppt7100_ann,
           t_avg_cat = cat_tav7100_ann,
           ann_min_precip = cat_minp6190,
           ann_max_precip = cat_maxp6190,
           et_cat = cat_et,
           pet_cat = cat_pet,
           rh_cat = cat_rh,
           depth_wattab = tot_ewt,
           t_avg_basin=tot_tav7100_ann,
           ann_min_precip_basin = tot_minp6190,
           ann_max_precip_basin = tot_maxp6190,
           pptavg_basin = tot_ppt7100_ann,
           et_basin = tot_et,
           pet_basin = tot_pet,
           rh_basin = tot_rh,
           wtdepave = cat_wtdep,
           cat_prsnow = tot_prsnow
           )

  # names(dat_df_nocalc) %>% as_tibble() %>% View()

  rm(varnames_nocalc)

  # Combine All -------------------------------------------------------------



  dat_final <-
    dat_df_nocalc %>%
    #left_join(dat_df_awa, dat_df_oth) %>%
    #left_join(dat_df_nocalc) %>%
    mutate(comid_wy = glue("{comid}_{wa_yr}"), .after=comid)

  # RENAME & WRITE IT OUT -----------------------

  # reorder
  input_sample <- read_csv("data_input/example_input_10000042.csv")
  input_sample_names <- data.frame("inputs" = names(input_sample))

  # bind w cross walk
  names_df <- input_sample_names %>% left_join(., xwalk, by=c("inputs"="mod_input_raw")) %>%
    # there are 6 vars that we don't have data for, dropping here:
    # pmpe, BDMAX, PMAX_WS, TMIN_WS, PERMH_WS, PMIN_WS
    filter(!is.na(mod_input_final))

  # missing? # NOPE!
  #xwalk[!xwalk$mod_input_final %in% names_df$mod_input_final,] %>% arrange(accum_op_class)

  # reorder to match sample
  dat_final2 <- dat_final %>%
    select(names_df$mod_input_final) %>%
    # filter out years 1945-1949
    filter(!wa_yr %in% c(1945:1949, 2016))

  # check name order
  # names(dat_final2) %>% as_tibble() %>% View()

  # fix odd magnitude issue:
  # rfact, et_cat, et, pptavg_cat, pptavg_basin off by order of 10
  # see here:
  #dat_final2 %>% select(cat_rfact, et_cat, et_basin, pptavg_cat, pptavg_basin, krug_runoff) %>% summary()

  dat_final2 <- dat_final2 %>%
    # scale down by 10
    mutate(across(c(cat_rfact, et_cat, et_basin, pptavg_cat, pptavg_basin), .fns = function(x)(x/10)))

  #dat_final2 %>% select(cat_rfact, et_cat, et_basin, pptavg_cat, pptavg_basin, krug_runoff) %>% summary()

  write_csv(dat_final2, file = glue("{outdir}/accumulated_raw_metrics_{modelname}.csv"))
  return(dat_final2)

}


