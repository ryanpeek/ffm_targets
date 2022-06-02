# download KRUG_RUNOFF Data

# requires s_revise_catchments.R to have been run to get the gpkg
# library(tidyverse)
# library(sf)
# library(glue)
# library(here)
# #library(mapview)
# #mapviewOptions(fgb = FALSE)
# library(rmapshaper)
# library(lwgeom)


# inputfile <- "data_input/scibase_nhd/krug_runoff_avg_ann_1951-1980.e00"
# nhd_data <- "data_output/nhdplus_full_vaa.gpkg"
# target_input <- revised_catchments_north[["flowlines"]]
# outdir <- "data_output/scibase_nhd/"

# function
f_get_krug_data <- function(inputfile, nhd_data,
                            target_input, outdir){

  # read in krug
  krug <- sf::st_read(glue::glue("{inputfile}")) %>%
    st_transform(5070)

  # get catchments
  print("load catchment data...")
  catch_vaa <- sf::st_read(here(glue("{nhd_data}")), "CatchmentSP", quiet=TRUE) %>%
    st_transform(st_crs(krug))

  # create a dissolved boundary w rmapshaper
  catch_diss <- ms_dissolve(catch_vaa)

  # get ca boundary
  ca <- urbnmapr::get_urbn_map("states", sf=TRUE) %>%
    filter(state_abbv =="CA") %>%
    st_transform(5070)

  # crop to ca
  print("crop krug to ca...")
  ca_krug <- st_intersection(krug, ca)

  # split by ca krug
  print("split by ca krug...")
  catch_krug <- lwgeom::st_split(catch_diss, ca_krug) %>%
    st_collection_extract(c("POLYGON")) %>%
    mutate(rowid = 1:nrow(.)) %>%
    slice(1:2) %>%
    st_join(ca_krug, join=st_touches) %>%
    mutate(INCHES = ifelse(is.na(INCHES), 5, 10))

  #mapview(catch_krug, zcol="INCHES") + mapview(ca_krug, zcol="INCHES")
  print("load flowlines...")
  # get flowlines and join
  # needs this file: read_rds("data_output/sf_flowlines_trimmed_w_areas.rds")
  lsh_flowline <- target_input %>%
    dplyr::select(comid, areasqkm, lengthkm, hydroseq) %>%
    st_transform(st_crs(krug))

  # join flowlines
  krug_comids <- st_join(lsh_flowline, catch_krug, join=st_nearest_feature) #%>%

  # preview
  # st_drop_geometry(krug_comids) %>% group_by(comid) %>% tally()
  # mapview(krug_comids, zcol="INCHES") + mapview(catch_krug, zcol="INCHES")

  # make csv of this
  krug_runoff_csv <- krug_comids %>% st_drop_geometry() %>%
    dplyr::select(COMID=comid, krug_runoff = INCHES) %>%
    mutate(source = "krug_runoff_avg_ann_1951-1980.e00")

  # write to data_clean
  #fs::dir_create(here("data_output/nhd_catchdat"))
  write_csv(krug_runoff_csv, file = glue("{outdir}/KRUG_RUNOFF.csv"))
  print("KRUG_RUNOFF.csv trimmed & saved in scibase_nhd")
  return(krug_runoff_csv)
}

# setup vars
# inputfile <- "data_input/scibase_nhd/krug_runoff_avg_ann_1951-1980.e00"
# flowlines <- "data_output/nhdplus_vaa_full.gpkg"
# outdir <- "data_output/scibase_nhd/"


#f_get_krug_data(inputfile, flowlines, outdir)
