# make revised catchment and flowlines accumulations files

# Libraries ---------------------------------------------------------------

# library(tidyverse)
# library(glue)
# library(fs)
# library(sf)
# library(here)
# library(nhdplusTools)
# library(units)
# library(mapview)
# mapviewOptions(fgb = FALSE)

# Vars --------------------------------------------------------------------

# indir <- "data_input"
# outdir <- "data_output"
# catch_input <- "catchments_final_lshasta.rds"
# startcomid: the outlet: "3917946"
# startcomid <- "3917946"
# function ---------

f_revise_catchments_full <- function(indir, outdir,
                                startcomid){

  comidstart <- list(featureSource = "comid",
                     featureID = startcomid)
  # get map
  nhdPlot <- plot_nhdplus(comidstart, actually_plot = FALSE,
                          streamorder = 1)

  # get hucs
  h8 <- get_huc8(nhdPlot$outlets)
  h12_all <- get_huc12(h8)
  h12_lsh <- h12_all %>%
    filter(huc12 %in% c("180102070301", "180102070302", "180102070303", "180102070304"))

  # pull flowlines
  flowlines <- navigate_nldi(comidstart,
                             mode = "UT",
                             distance = 100)

  # crop to just the h12s (st_intersection will split across across H12)
  flowlines_trim <- flowlines$UT_flowlines[h12_lsh,] %>%
    # drop comid that extends outside
    filter(!nhdplus_comid %in% c(3917382))

  # pull for HUC above just to pull catchments...
  huc_feat <- list(featureSource = "huc12pp",
                   featureID = "180102070501")
  flowline_huc <- navigate_nldi(huc_feat,
                                mode = "UT",
                                distance = 50)

  # pull extra comids
  flowlines_huc <- flowline_huc$UT_flowlines[h12_lsh[h12_lsh$huc12 %in% c(180102070303, 180102070304),],] %>%
    # filter canal comids that are completely outside of HUC12s
    filter(!nhdplus_comid %in% c(3917928, 3917930, 3917242, 3917382, 3917954))
  # st_intersection splits comid across h12 boundaries (gives duplicates)

  # note: COMIDs 3917382 and 3917928 extend inside/outside of h12
  # dropping both here

  # Make a distinct final comid list
  final_comids <- c(flowlines_huc$nhdplus_comid, flowlines_trim$nhdplus_comid) %>% unique(.)

  # bind
  flowlines_final <- bind_rows(flowlines_huc, flowlines_trim) %>%
    distinct(nhdplus_comid, .keep_all = TRUE)

  # NHDPLus Geopackage ------------------------------------------------------

  # check if vaa exists
  if(!fs::file_exists(glue("{outdir}/nhdplus_full_vaa.gpkg"))){

    print("downloading nhd data...")

    nhd_vaa <- nhdplusTools::subset_nhdplus(
      comids = unique(as.integer(final_comids)),
      output_file = glue("{outdir}/nhdplus_full_vaa.gpkg"),
      nhdplus_data = "download",
      flowline_only = FALSE,
      return_data = FALSE,
      overwrite = TRUE)
  } else({
    print(glue("{outdir}/nhdplus_full_vaa.gpkg already exists!"))
    print(st_layers(glue("{outdir}/nhdplus_full_vaa.gpkg")))}
  )

  # get catch/flow VAA
  db <- here(glue("{outdir}/nhdplus_full_vaa.gpkg"))
  nhd_flowlines <- st_read(db, "NHDFlowline_Network", quiet = TRUE)
  nhd_catchments <- st_read(db, "CatchmentSP", quiet = TRUE)

  # Filter CANALS and Segments that we need
  comid_trim <- nhd_flowlines %>%
    filter(fcode %in% c(46006, 46003)) %>%
    # add these connectors back in
    bind_rows(., nhd_flowlines %>%
                filter(comid %in%
                         # canals
                         c(948010091, 948010089, 948010092,3917222,
                           3917226, 3917270, 3917952, 3917266,
                           948010095, 948010096)))

  # get Catchments that match
  catch_trim <- nhd_catchments %>%
    filter(featureid %in% comid_trim$comid)


  # Generate Clean COMID network --------------------------------------------

  print(glue("Cleaning nhd data and calculating areas..."))

  # edit comids and nodes:
  comid_trim_rev <- comid_trim %>%
    select(-c(id, fdate:gnis_name, reachcode:wbareacomi, shape_length, frommeas:areasqkm, divdasqkm:enabled)) %>%
    mutate(
      # the start node
      fromnode = case_when(
        comid == 948010095 ~ 10013981,
        comid == 948010096 ~ 10086523,
        comid == 948010092 ~ 10086520,
        TRUE ~ fromnode),
      # end node
      tonode = case_when(
        comid == 948010095 ~ 10086523,
        comid == 948010096 ~ 10014067,
        comid == 948010092 ~ 10014066,
        TRUE ~ tonode),
      # dnhydroseq
      dnhydroseq = case_when(
        #is.na(dnhydroseq) ~ NA_real_,
        comid == 948010095 ~ 10020959,
        comid == 948010096 ~ 10021351,
        comid == 948010046 ~ 10020959,
        comid == 948010092 ~ 10032346,
        TRUE ~ dnhydroseq),
      # uphydroseq
      uphydroseq = case_when(
        comid == 948010095 ~ 0,
        comid == 948010096 ~ 10026762,
        comid == 948010046 ~ 10020959,
        comid == 948010091 ~ 0,
        comid == 948010092 ~ 0,
        TRUE ~ uphydroseq))

  print(glue("make clean flownet and sort..."))
  # now make cleaned flownet
  flownet <- get_tocomid(comid_trim_rev, return_dendritic = TRUE, missing = 0, add = TRUE) %>%
    select(comid, tocomid, fromnode, tonode, hydroseq, dnhydroseq, ftype, fcode, lengthkm, levelpathi, totdasqkm, terminalfl, startflag, terminalpa, divergence) %>%
    mutate(
      tocomid = case_when(
        comid == 948010095L ~ 948010096L,
        comid == 948010092L ~ 3917948L,
        comid == 948010046L ~ 948010096L,
        TRUE ~ tocomid),
      fromnode = case_when(
        comid == 948010092L ~ 10086520,
        comid == 948010095L ~ 10013981, # or NA_real_
        comid == 948010096 ~ 10086523,
        TRUE ~ fromnode),
      tonode = case_when(
        comid == 948010092L ~ 10086523,
        TRUE ~ tonode))

  # sort and split
  flownet_sort <- get_sorted(flownet, split = TRUE)

  # if dendritic and different Terminal groups exist, use split = TRUE:
  # flownet_sort_main <- filter(flownet_sort, terminalID == 3917946)

  # add sort_order
  flownet_sort['sort_order'] <- 1:nrow(flownet_sort)

  print(glue("calculate arbolate sum..."))
  flownet_sort[["arbolatesum"]] <- calculate_arbolate_sum(
    dplyr::select(flownet_sort,
                  ID = comid, toID = tocomid, length = lengthkm))

  # Get path lengths -------------------

  print(glue("calculate updated path lengths..."))

  # get pathlengths: need ID, toID, length
  flowlines_net <- flownet_sort %>%
    rename(ID=comid, toID=tocomid) %>%
    # this updates the pathlength total (main path to basin terminal)
    left_join(., nhdplusTools::get_pathlength(.)) %>%
    relocate(pathlength, .after=lengthkm)

  # function to calculate us comids ---------------------------

  get_us_comids <- function (network, ID, distance = NULL)
  {
    # drop geometry:
    if ("sf" %in% class(network))
      network <- sf::st_set_geometry(network, NULL)
    # get vector of cols to use
    netcols_needed <- c("ID", "toID", "lengthkm", "levelpathi","pathlength", "hydroseq","dnhydroseq")
    # filter to cols
    if (!sum(names(network) %in% netcols_needed)==length(netcols_needed)){
      print("Columns missing from network...double check inputs!")
    }
    network <- dplyr::select(network, all_of(netcols_needed))

    # fix names
    names(network) <- tolower(names(network))

    # set the starting ID to search from and search distance in network
    start_comid <- dplyr::filter(network, id == ID)
    if (!is.null(distance)) {
      if (distance < start_comid$lengthkm)
        return(ID)
    }

    # function to get the comids
    private_get_UT <- function(network, ID){
      # first get the mainstem comids and the trib comids
      main <- filter(network, id %in% ID)
      if (length(main$hydroseq) == 1) {
        full_main <- filter(network, levelpathi %in% main$levelpathi &
                              hydroseq >= main$hydroseq)
        trib_lpid <- filter(network, dnhydroseq %in% full_main$hydroseq &
                              !levelpathi %in% main$levelpathi &
                              hydroseq >= main$hydroseq)$levelpathi
      }
      else {
        full_main <- filter(network, levelpathi %in% main$levelpathi)
        trib_lpid <- filter(network, dnhydroseq %in% full_main$hydroseq &
                              !levelpathi %in% main$levelpathi)$levelpathi
      }
      # if trib comid to start, re-run and grab comids associated u/s
      trib_comid <- filter(network, levelpathi %in% trib_lpid)$id
      if (length(trib_comid) > 0) {
        return(c(full_main$id, private_get_UT(network, trib_comid)))
      }
      # or just give comids for mainstem
      else {
        return(full_main$id)
      }
    }
    # return dat
    all <- private_get_UT(network, ID)

    # check and sort if distance is set
    if (!is.null(distance)) {
      stop_pathlength <- start_comid$pathlength - start_comid$lengthkm +
        distance
      network <- filter(network, ID %in% all)
      return(filter(network, pathlength <= stop_pathlength)$id)
    }
    else {
      return(all)
    }
  }

  # Apply to each comid -----------------

  print(glue("calculate all upstream comids for each comid..."))
  # make list of us comids for each comid:
  final_comlist <- map(flowlines_net$ID, ~get_us_comids(flowlines_net, .x)) %>%
    set_names(flowlines_net$ID)

  # Filter Catch to Clean Network -------------------------------------------

  print(glue("calculate total area and weights..."))

  # calc total area:
  lsh_area <- sum(st_area(catch_trim) %>% set_units("km^2"))

  # calculate areas and weights
  catch_trim <- catch_trim %>%
    mutate(
      areasqkm = units::set_units(st_area(geom),"m^2") %>%
        # make km2
        set_units("km^2") %>% drop_units(),
      # now make a weight by total area
      area_weight = areasqkm/lsh_area %>%
        drop_units(), .after=areasqkm) %>%
    rename(comid=featureid) %>%
    # drop these cols:
    select(-c(shape_length, shape_area))

  # calculate total drainage area
  catchment_area <- flowlines_net %>%
    # add back in the correct areas to use in the calculation
    left_join(st_drop_geometry(catch_trim) %>%
                select(comid, areasqkm), by=c("ID"="comid")) %>%
    select(ID, toID, area=areasqkm) %>%
    mutate(totda = calculate_total_drainage_area(.),
           # add previous calc
           nhdptotda = flowlines_net$totdasqkm)

  # join back to catch_main
  final_catch <- left_join(catch_trim, st_drop_geometry(catchment_area) %>%
                             select(ID, totda, nhdptotda),
                           by=c("comid"="ID"))

  # now add back to lsh_flownet
  final_flownet <- flowlines_net %>%
    select(-c( totdasqkm)) %>%
    left_join(., st_drop_geometry(final_catch),
              by=c("ID"="comid")) %>%
    rename(totdasqkm = totda, comid=ID, tocomid=toID) %>%
    select(-c(id, sourcefc))
  print(glue("writing out data to {outdir}..."))

# SAVE OUT ----------------------------------------------------------------

  # WRITE TO RDS
  # write_rds(final_comlist,
  #           file=glue("{outdir}/cleaned_full_lshasta_comid_accum_list.rds"))
  # write_rds(final_flownet,
  #           file = glue("{outdir}/sf_flowlines_trimmed_w_areas.rds"))
  # write_rds(final_catch,
  #           file=glue("{outdir}/cleaned_full_lshasta_nhd_catchments.rds"))

  # need a csv with comid, area, and drain area
  final_catch %>% st_drop_geometry() %>%
    select(comid, areasqkm:nhdptotda) %>%
    write_csv(file = glue("{outdir}/sf_catch_trimmed_w_areas.csv"))

  # save out flowlines cleaned
  final_flownet %>%
    write_rds(file=glue("{outdir}/sf_flowlines_trimmed_w_areas.rds"))

  # clean enviro:
  # rm(list = ls()[grep("^catch|^com|flowline|^h|^flownet", ls())])

  return(list(flowlines = final_flownet, catchments = final_catch, comlist_accum = final_comlist))
}

