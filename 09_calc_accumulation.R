# make catchment areas calculate additional variables
# using area weighted average (catch area / tot basin area)

# requires:
# the cleaned and prepped cat_ffc_data (raw by catchment/comid)
# flowlines with comid and tocomid field
# xwalk dataset for variable names
# outdir

f_calc_accum_data <- function(cat_data, flowlines, xwalk, outdir){

  # MAKE THE NETWORK ------------------------

  flowlines_trim <- dplyr::select(flowlines,
                           comid, tocomid, hydroseq,
                           lengthkm, areasqkm, area_weight, totdasqkm) %>%
    st_drop_geometry() %>%
    filter(!is.na(tocomid))

# to view a network use this:
  # library(networkD3)
  # p <- simpleNetwork(flowlines, Source = "comid", Target = "tocomid", height = "400px",
  #                    width = "400px",
  #                    fontSize = 16,
  #                    fontFamily = "serif",
  #                    nodeColour = "darkblue",
  #                    linkColour = "steelblue",
  #                    opacity = 0.9, zoom = TRUE, charge = -40)

  # fix the network edges and identify u/s & d/s "ends" termini
    flow_tips <- flowlines_trim %>%
    filter(!comid %in% tocomid) %>%
    pull(comid) %>%
    unique() %>%
    as.character()

    # for single drainage network this should be a single outlet or zero
    flow_out <- flowlines_trim %>%
      filter(!tocomid %in% comid) %>%
      pull(tocomid) %>%
      unique() %>%
      as.character()

    # convert edge matrix to igraph

    # OPTIONAL: make into a matrix:
    # turn edge dataframe into matrix for igraph conversion
    # flow_matrix <- flowlines_trim %>%
    #   select(1:2) %>%
    #   mutate(across(c(comid:tocomid), ~as.character(.))) %>%
    #   as.matrix()
    # flow_igraph <- graph_from_edgelist(flow_matrix, directed = FALSE)

    # creating igraph obj w/out directionality (directed=FALSE) allows u/s path creation
    flow_igraph <- graph_from_data_frame(flowlines_trim, directed=FALSE)


    # CALC FLOW NETWORK --------------------------------

    # get list all simple paths for every comid that isn't a tip
    # flow_comids_mid <- flowlines_trim %>%
    #   filter(!comid %in% flow_us_ends, !comid %in% flow_ds_ends) %>%
    #   pull(comid) %>% as.character()
    # flow_mids + flow_tips = unique comids

    # then use that to calculate flowpaths for mid comids
    flowpaths <- map(flow_out,
                          ~all_simple_paths(flow_igraph,
                                            from = .x,
                                            to = flow_tips)) %>%
      # unlist to make easier to work with
      unlist(., recursive = FALSE)

    ## CONVERT FLOWPATHS TO DATAFRAME -------------------

    # list of lists: 1 item for each mid comid, sub 1 item for every tip
    flowpaths_df <- map(flowpaths, ~as_ids(.x)) %>%
      as.matrix(.) %>% as.data.frame(.) %>%
      magrittr::set_colnames("paths_lst") %>%
      rowid_to_column(var="row_id")

    # create final dataframe of paths
    flowpaths_df <- unnest(flowpaths_df, paths_lst) %>%
      group_by(row_id) %>%
      mutate(paths_chr = paste0(paths_lst, collapse = " ")) %>%
      select(row_id, paths_chr) %>%
      unique() %>%
      mutate(comlist = strsplit(paths_chr, " "),
             downstream_comid = map_chr(comlist, c(2)),
             upstream_comid = map_chr(comlist, last)) %>%
      select(row_id, downstream_comid, upstream_comid, comlist, paths_chr)

    ## MAKE LIST FOR ACCUMULATION ROUTING

    ## returns a list of comids for any given comid
    comid_ls <- map(flowpaths_df$comlist,
                    ~unlist(strsplit(.x, ", "))) %>%
      map(., ~as.numeric(.x))

    ## get the list
    comid_ls2 <- map_depth(comid_ls, 1,
                     ~dplyr::filter(flowlines_trim, comid %in% .x) %>%
                       pull(comid)) #%>%
      map(., ~discard(., .x==0)) #%>%
      set_names(flowpaths_df$upstream_comid)

    map(comid_ls2, length)


  # hydroseq ls of comids
  map_depth(comid_ls, 2, ~length(.x)) # check lengths, 1 is a trib or spur

  # n=33
  # ACCUMULATION! ----------
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
  dat_ls_awa <- map(comid_ls, ~filter(cat_df_awa, comid %in% .x) %>%
                      select(-c(comid, comid_wy)))

  dat_awa <- map(dat_ls_awa, ~group_by(.x, wa_yr) %>%
                   summarise(
                     across(
                       .cols  = ppt_jan_wy:krug_runoff,
                       ~sum(.x*area_weight),
                       .names = "{col}_awa")
                   ))

  # collapse as dataframe:
  dat_df_awa <- bind_rows(dat_awa, .id = "hydroseq") %>%
    mutate(hydroseq=as.numeric(hydroseq)) %>%
    # fix the awa ending
    rename_with(~str_remove(., '_awa')) %>%
    # rename calculated vars back to original variables of interest
    rename(ann_min_precip_basin = cat_minp6190,
           ann_max_precip_basin = cat_maxp6190,
           pptavg_basin = cat_ppt7100_ann,
           et_basin = cat_et,
           pet_basin = cat_pet,
           rh_basin = cat_rh,
           wtdepave = cat_wtdep) %>%
    # bind back to comid
    left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
    relocate(comid, hydroseq, .before="wa_yr")

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
    select(comid:wa_yr, varnames_oth$dat_output) %>%
    # add area weights
    left_join(catch_areas_filt_no_sf, by=c("comid"="comid_catch")) %>%
    select(comid:wa_yr, comid_flowline:upper, everything()) %>%
    # fix area_sf
    select(-area_sf.x) %>%
    rename(area_sf = area_sf.y)

  # filter to dataframe that is list of all comids for a given comid
  dat_ls_oth <- map(comid_ls, ~filter(cat_df_oth, comid %in% .x) %>%
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

  #dat_oth[[30]] %>% filter(wa_yr == 1950) %>% names()

  # collapse and rename to t_avg_basin
  dat_df_oth <- bind_rows(dat_oth, .id = "hydroseq") %>%
    mutate(hydroseq = as.numeric(hydroseq)) %>%
    rename(t_avg_basin=cat_tav7100_ann_avg) %>%
    rename(cat_elev_min = cat_elev_min_min,
           cat_elev_max = cat_elev_max_max,
           area_sf = area_sf_sum) %>%
    # bind back to comid
    left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
    relocate(comid, hydroseq, .before="wa_yr")

  rm(dat_ls_oth, varnames_oth, cat_df_oth, dat_oth)

  ## NO CALC ----------------------------------------------------------

  # need the no calc vars, subset and then join
  varnames_nocalc <- xwalk %>%
    filter(accum_op_class %in% c("NONE")) %>%
    filter(!dat_output %in% c("comid", "comid_wy", "wa_yr")) %>%
    select(dat_output)

  # subset data
  dat_df_nocalc <- cat_data %>% select(comid, wa_yr, varnames_nocalc$dat_output) %>%
    # rename
    rename(pptavg_cat = cat_ppt7100_ann,
           t_avg_cat = cat_tav7100_ann,
           ann_min_precip = cat_minp6190,
           ann_max_precip = cat_maxp6190,
           et_cat = cat_et,
           pet_cat = cat_pet,
           rh_cat = cat_rh,
           depth_wattab = cat_wtdep) %>%
    left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
    relocate(comid, hydroseq, .before="wa_yr")

  names(dat_df_nocalc)
  # here cat_wtdep is actually "depth_wattab", cat_wtdep w awa is wtdepave

  rm(varnames_nocalc)

  ## ECO Dominant -------------------------------------------------------

  cat_df_eco <- cat_data %>%
    select(comid:wa_yr, eco3)

  # filter to dataframe that is list of all comids for a given comid
  dat_ls_eco <- map(comid_ls, ~filter(cat_df_eco, comid %in% .x) %>%
                      select(-c(comid, comid_wy)))

  dat_eco <- map(dat_ls_eco, ~group_by(.x) %>%
                   summarise(
                     eco3_dom = names(which.max(table(eco3)))))

  dat_df_eco <- bind_rows(dat_eco, .id = "hydroseq") %>%
    mutate(hydroseq=as.numeric(hydroseq)) %>%
    rename(eco3=eco3_dom) %>%
    left_join(., st_drop_geometry(flow_trim[,c(1,2)])) %>%
    relocate(comid, hydroseq, .before=1)

  rm(dat_eco, cat_df_eco, dat_ls_eco)

  # Combine All -------------------------------------------------------------

  dat_final <- left_join(dat_df_awa, dat_df_oth) %>%
    left_join(dat_df_nocalc) %>%
    left_join(dat_df_eco) %>%
    select(-hydroseq) %>% # drop
    mutate(comid_wy = glue("{comid}_{wa_yr}"), .after=comid) # add comid_wa_yr

  # Rename & Reorder -----------------------------------------

  # use the cross walk to reorder and rename
  # first pull the column names into a df
  final_names <- dat_final %>% colnames %>% as_tibble("final_names")

  # now join with the xwalk and match up or fix
  xwalk_final <- left_join(final_names, xwalk, by=c("value"="dat_output")) %>%
    select(mod_input_final = value, mod_input_raw, model_input_clean, check, accum_op, accum_op_class, source_file, variable_description)

  # identify the missmatches and pull out:
  xwalk_final_mismatch <- xwalk_final %>% filter(is.na(model_input_clean)) %>%
    select(mod_input_final)

  # now match back to the xwalk but use the model_input_clean field
  xwalk_final_sel <- left_join(xwalk_final_mismatch, xwalk, by=c("mod_input_final"="model_input_clean")) %>%
    select(mod_input_final, mod_input_raw, check, accum_op, accum_op_class, source_file, variable_description)

  # now merge it all together!!
  xwalk_fin <- xwalk_final %>% filter(!is.na(mod_input_raw)) %>%
    bind_rows(xwalk_final_sel) %>%
    select(-model_input_clean)

  # WRITE IT OUT
  write_csv(xwalk_fin, file="data_clean/08_accumulated_final_xwalk.csv")

  rm(xwalk_final, xwalk_final_sel, xwalk_final_mismatch)
  rm(dat_df_eco, dat_df_nocalc, dat_df_oth, dat_df_awa)

  # reorder
  input_sample <- read_csv("data_raw/sample.csv")
  input_sample_names <- data.frame("inputs" = names(input_sample))

  # bind w cross walk
  names_df <- input_sample_names %>% left_join(., xwalk_fin, by=c("inputs"="mod_input_raw")) %>%
    filter(!is.na(mod_input_final))

  # reorder to match sample
  dat_final2 <- dat_final %>%
    select(unique(names_df$mod_input_final)) %>%
    # filter out years 1945-1949
    filter(!wa_yr %in% c(1945:1949))
