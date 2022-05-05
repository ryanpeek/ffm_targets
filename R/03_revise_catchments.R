# make revised catchment area file


# Libraries ---------------------------------------------------------------

# library(tidyverse)
# library(glue)
# library(fs)
# library(sf)
# library(nhdplusTools)
# library(units)
#library(mapview)
#mapviewOptions(fgb = FALSE)

# Vars --------------------------------------------------------------------

# indir <- "data_input"
# outdir <- "data_output"
# catch_input <- "catchments_final_lshasta.rds"


# function ---------

f_revise_catchments <- function(indir, outdir,
                                catch_input){

  # comids
  catch <- read_rds(glue("{indir}/{catch_input}"))

  # drop splinters, leftover from previous method
  to_drop <- c(1386713,1387823, 1387701,1387917, 1387877,
               1387655, 1387682, 1387926, 1387559, 1387104,
               1387360, 1387795,1387830, 1520905, 1386208,1386450,
               1386396,1386475, 1386300,1386459,1386291,
               1386638, 1386532, 1386593, 1386796,
               1387661, 1387838, 1387679, 1386893,
               1386339, 1387033)
  # filter
  catch_comids <- catch %>% filter(!GRIDCODE %in% to_drop)

  # Calc Areas --------------------------------------------------------------

  catch_comids <- catch_comids %>%
    # recalc areas
    mutate(area_rev = units::set_units(st_area(geom),"m^2") %>%
             set_units("km^2") %>%
             drop_units(), .after = AreaSqKM) %>%
    select(FEATUREID, AreaSqKM, area_rev, comid,
           comid_group=comid_f, upper, geom)

  # make just a vector of comids
  comids <- catch_comids$FEATUREID

  # NHDPLus Geopackage ------------------------------------------------------

  # check if vaa exists
  if(!fs::file_exists(glue("{outdir}/nhdplus_vaa.gpkg"))){

    print("downloading nhd data...")

    nhd_vaa <- nhdplusTools::subset_nhdplus(
      comids = unique(as.integer(comids)),
      output_file = glue("{outdir}/nhdplus_vaa.gpkg"),
      nhdplus_data = "download",
      flowline_only = FALSE,
      return_data = TRUE,
      overwrite = TRUE)
  } else({
    print(glue("{outdir}/nhdplus_vaa.gpkg already exists!"))
    print(st_layers(glue("{outdir}/nhdplus_vaa.gpkg")))}
  )

  # get catch VAA
  catch_vaa <- sf::st_read(glue("{outdir}/nhdplus_vaa.gpkg"), "CatchmentSP", quiet=TRUE)

  # get nhd flowlines & filter
  nhd_vaa <- sf::st_read(glue("{outdir}/nhdplus_vaa.gpkg"), "NHDFlowline_Network", quiet=TRUE) %>%
    filter(comid %in% comids)

  # Preview Map -------------------------------------------------------------

  # quick view w mapview
  #mapview(catch_comids) + mapview(catch_vaa, alpha=0.4, col.regions="yellow") + mapview(nhd_vaa, color="cyan4")

  # Remove Canals (keep intermittent/perennial) -----------------------------

  # drop canals?
  canal_comids <- c(3917940, 3917216,3917928,
                    948010095, 3917954, 3917382,
                    # side canals
                    3917266, 3917952, 3917270,
                    948010096, 948010046,
                    948010047, 948010048)

  # FCODE defs: https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm

  # canal: 33600,
  # aquaduct: 33601,
  # stormwater: 33603
  # artificial path: 55800
  # ephemeral: 46007
  # intermittent: 46003
  # perennial: 46006

  nhd_trim <- nhd_vaa %>%
    filter(fcode %in% c(46006, 46003))
  # filter(!comid %in% canal_comids)

  catch_trim <- catch_vaa %>%
    filter(featureid %in% nhd_trim$comid)

  # preview
  # mapview(catch_trim, alpha=0.4, col.regions="gray") + mapview(nhd_trim, color="cyan4")

  # Generate Clean COMID network --------------------------------------------

  print(glue("Cleaning nhd data and calculating areas..."))
  # generate clean comid network
  flownet <- get_tocomid(nhd_trim, return_dendritic = TRUE, missing = 0, add = TRUE)

  # sort and split
  flownet_sort <- get_sorted(flownet, split = TRUE)

  # if split = TRUE with dendritic true:
  flownet_sort_main <- filter(flownet_sort, terminalID == 3917946)
  # filter to stream that gets to outlet
  flownet_sort_main['sort_order'] <- 1:nrow(flownet_sort_main)

  # arbolate
  flownet_sort_main[["arbolatesum"]] <- calculate_arbolate_sum(
    dplyr::select(flownet_sort_main,
                  ID = comid, toID = tocomid, length = lengthkm))

  # plot based on upstream flowpath
  # plot(sf::st_geometry(flownet_sort_main), lwd = flownet_sort_main$arbolatesum / 10)

  # Filter Catch to Clean Network -------------------------------------------

  print(glue("filter catchment to comids..."))
  catch_main <- catch_vaa %>%
    filter(featureid %in% flownet_sort_main$comid)

  print(glue("calculate total area and weights..."))

  # calc total area:
  lsh_area <- sum(st_area(catch_main) %>% set_units("km^2"))

  # calculate areas and weights
  catch_main <- catch_main %>%
    mutate(
      areasqkm = units::set_units(st_area(geom),"m^2") %>%
        set_units("km^2") %>% drop_units(),
      area_weight = areasqkm/lsh_area %>%
        drop_units(), .after=areasqkm) %>%
    rename(comid=featureid)

  #mapview(catch_main, alpha=0.4, col.regions="gray") + mapview(flownet_sort_main, color="cyan4")

  # calculate total drainage area
  catchment_area <- prepare_nhdplus(flownet_sort_main, 0, 0,
                                    purge_non_dendritic = FALSE,
                                    warn = FALSE) %>%
    # add back in the correct areas to use in the calculation
    left_join(st_drop_geometry(catch_main) %>%
                select(comid, areasqkm), by=c("COMID"="comid")) %>%
    select(ID = COMID, toID = toCOMID, area=areasqkm) %>%
    mutate(totda = calculate_total_drainage_area(.),
           # add previous calc
           nhdptotda = flownet_sort_main$totdasqkm)

  # join back to catch_main
  catch_main <- left_join(catch_main, catchment_area %>%
                            select(ID, totda, nhdptotda),
                          by=c("comid"="ID"))

  # now add back to lsh_flownet
  flownet_main <- flownet_sort_main %>%
    select(-c(areasqkm, totdasqkm)) %>%
    left_join(., st_drop_geometry(catch_main),
              by=c("comid")) %>%
    rename(totdasqkm = totda) %>%
    select(-ends_with(".y"), -ends_with(".x"))

  print(glue("writing out data to {outdir}..."))

  # write out!!
  # need a csv with comid, area, and drain area
  catch_main %>% st_drop_geometry() %>%
    write_csv(file = glue("{outdir}/sf_catch_trimmed_w_areas.csv"))

  # save out flowlines cleaned
  flownet_main %>%
    write_rds(file=glue("{outdir}/sf_flowlines_trimmed_w_areas.rds"))

  return(flownet_main)
}

