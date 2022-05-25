# make catchment areas calculate additional variables
# using area weighted average (catch area / tot basin area)

# requires:
# the cleaned and prepped cat_ffc_data (raw by catchment/comid)

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
    rename_with(~str_remove(., '_awa')) %>%
    # rename calculated vars back to original variables of interest
    rename(ann_min_precip_basin = cat_minp6190,
           ann_max_precip_basin = cat_maxp6190,
           pptavg_basin = cat_ppt7100_ann,
           et_basin = cat_et,
           pet_basin = cat_pet,
           rh_basin = cat_rh,
           wtdepave = cat_wtdep) #%>%
    # bind back to comid
    #left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
    #relocate(comid, hydroseq, .before="wa_yr")

  # rm temp files
  rm(dat_ls_awa, varnames_awa, cat_df_awa, dat_awa)

  ## MAX/MIN/RANGE/SUM----------------------------------------------------

  # select all variables that need "avg of catch values"
  varnames_oth <- xwalk %>%
    filter(accum_op_class %in% c("AVG", "MAX","MIN","RNG","SUM")) %>%
    filter(!dat_output %in% c("comid", "comid_wy", "wa_yr")) %>%
    mutate(dat_output = case_when(
      dat_output=="t_avg_basin" ~ "cat_tav7100_ann",
      TRUE ~ dat_output
    ))

  # filter to vars
  cat_df_oth <- cat_data %>%
    # drop the non info vars
    select(comid:wa_yr, varnames_oth$dat_output)

  # filter to dataframe that is list of all comids for a given comid
  dat_ls_oth <- map(comlist, ~filter(cat_df_oth, comid %in% .x) %>%
                      select(-c(comid, comid_wy)))

  # iterate over
  dat_oth <- map(dat_ls_oth, ~group_by(.x, wa_yr) %>%
                   summarise(
                     across(
                       # min cols
                       .cols  = c(cat_elev_min),
                       ~min(.x),
                       .names = "{col}_min"),
                     across(
                       # max
                       .cols = c(cat_elev_max),
                       ~max(.x),
                       .names = "{col}_max"),
                     # mean
                     across(
                       .cols = c(cat_tav7100_ann),
                       ~mean(.x, na.rm=TRUE),
                       .names = "{col}_avg"),
                     across(
                       .cols = c(area_sf),
                       ~sum(.x),
                       .names = "{col}_sum")
                   )
  )

  # add range after
  dat_oth <- map(dat_oth, ~mutate(.x,
                                  elv_rng = cat_elev_max_max - cat_elev_min_min))

  # collapse and rename to t_avg_basin
  dat_df_oth <- bind_rows(dat_oth, .id = "comid") %>%
    mutate(comid=as.numeric(comid)) %>%
    rename(t_avg_basin=cat_tav7100_ann_avg) %>%
    rename(cat_elev_min = cat_elev_min_min,
           cat_elev_max = cat_elev_max_max,
           area_sf = area_sf_sum)

  rm(dat_ls_oth, varnames_oth, cat_df_oth, dat_oth)

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
           depth_wattab = tot_wtdep)

  # names(dat_df_nocalc)
  # cat_wtdep w awa is wtdepave

  rm(varnames_nocalc)

  ## ECO Dominant -------------------------------------------------------

  cat_df_eco <- cat_data %>%
    select(comid:wa_yr, eco3)

  # filter to dataframe that is list of all comids for a given comid
  dat_ls_eco <- map(comlist, ~filter(cat_df_eco, comid %in% .x) %>%
                      select(-c(comid, comid_wy)))

  dat_eco <- map(dat_ls_eco, ~group_by(.x) %>%
                   summarise(
                     eco3_dom = names(which.max(table(eco3)))))

  dat_df_eco <- bind_rows(dat_eco, .id = "comid") %>%
    mutate(comid=as.numeric(comid)) %>%
    rename(eco3=eco3_dom)

  rm(dat_eco, cat_df_eco, dat_ls_eco)

  # Combine All -------------------------------------------------------------

  dat_final <- left_join(dat_df_awa, dat_df_oth) %>%
    left_join(dat_df_nocalc) %>%
    left_join(dat_df_eco) %>%
    mutate(comid_wy = glue("{comid}_{wa_yr}"), .after=comid) # add comid_wa_yr

  # Rename & Reorder -----------------------------------------

  # # use the cross walk to reorder and rename
  # # first pull the column names into a df
  # final_names <- dat_final %>% colnames %>% as_tibble("final_names")
  #
  # # now join with the xwalk and match up or fix
  # xwalk_final <- left_join(final_names, xwalk, by=c("value"="dat_output")) %>%
  #   select(mod_input_final = value, mod_input_raw, model_input_clean=mod_input_final,
  #          check, accum_op, accum_op_class, source_file, variable_description)
  #
  # # identify the missmatches and pull out:
  # xwalk_final_mismatch <- xwalk_final %>% filter(is.na(model_input_clean)) %>%
  #   select(mod_input_final)
  #
  # # now match back to the xwalk but use the model_input_clean field
  # xwalk_final_sel <- left_join(xwalk_final_mismatch, xwalk, by=c("mod_input_final")) %>%
  #   select(mod_input_final, mod_input_raw, check, accum_op, accum_op_class, source_file, variable_description)
  #
  # # now merge it all together!!
  # xwalk_fin <- xwalk_final %>% filter(!is.na(mod_input_raw)) %>%
  #   bind_rows(xwalk_final_sel) %>%
  #   select(-model_input_clean)

  # WRITE IT OUT
  #write_csv(xwalk_fin, file="data_clean/08_accumulated_final_xwalk.csv")

  #rm(xwalk_final, xwalk_final_sel, xwalk_final_mismatch)

  rm(dat_df_eco, dat_df_nocalc, dat_df_oth, dat_df_awa)

  # reorder
  input_sample <- read_csv("data_input/example_input_10000042.csv")
  input_sample_names <- data.frame("inputs" = names(input_sample))

  # bind w cross walk
  names_df <- input_sample_names %>% left_join(., xwalk, by=c("inputs"="mod_input_raw")) %>%
    filter(!is.na(mod_input_final))

  # reorder to match sample
  dat_final2 <- dat_final %>%
    select(names_df$mod_input_final) %>%
    # filter out years 1945-1949
    filter(!wa_yr %in% c(1945:1949))

  write_csv(dat_final2, file = glue("{outdir}/accumulated_raw_metrics_{modelname}.csv"))
  return(dat_final2)

}


