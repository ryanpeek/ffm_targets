# 03_clean_nhd_flowlines
# this script pulls catchment and flowline data for use in accumulation and filtering of the data

# Get LOIs and Flowlines --------------------------------------------------

f_get_comids <- function(data){

  # read_rds(here("data_input/nhd_flowlines_vaa.rds")
  # flowlines
  flowlines <- read_rds(glue("{data}")) %>%
    # remove all the junk just get what we need
    select(comid, fromnode, tonode, divergence, ftype,
           areasqkm, lengthkm, totdasqkm, terminalID,
           sort_order, arbolatesum, gnis_id, hydroseq) %>%
    st_transform(4269)
  return(flowlines)
}


# Clean  ------------------------------------------------------------------

# f_clean_nhd_flowlines <- function(data){
#
#   # List sink/isolated segments (N=19)
#   sinks <- c(3917228, 3917212, 3917214, 3917218, 3917220,
#              3917960, 3917958, 3917276, 3917278, 3917274,
#              3917282, 3917284, 3917286, 3917280, 3917268,
#              3917256, 3917250, 3917272, 3917956)
#
#   # make just sinks
#   fsinks <- flowlines %>% filter(comid %in% sinks)
#   fmain <- flowlines %>% filter(!comid %in% sinks)
#   flowline_list <- list("fsinks" = fsinks, "fmain" = fmain)
#   return(flowline_list)
# }


# Clean Catchments --------------------------------------------------------

#
# library(units)
#
# # clean catch
# # drop these catch gridcodes, they are all splinters
# to_drop <- c(1386713,1387823, 1387701,1387917, 1387877,
#              1387655, 1387682, 1387926, 1387559, 1387104,
#              1387360, 1387795,1387830, 1520905, 1386208,1386450,
#              1386396,1386475, 1386300,1386459,1386291, 1386638, 1386532,
#              1386593, 1386796, 1387661, 1387838, 1387679, 1386893,
#              1386339, 1387033)
# # note 1387724 and 1387816 have some slivers/edges
# # run with and without !GRIDCODE to see
# catch_clean <- catch %>% filter(!GRIDCODE %in% to_drop) %>%
#   # recalc areas?
#   mutate(area2 = units::set_units(st_area(geom),"m^2") %>% set_units("km^2") %>% drop_units())
# # select(catch_clean, comid, FEATUREID, AreaSqKM, area2) %>% View()
#
# # get nhd flowlines
# nhd_vaa <- sf::st_read(here("data_input/nhdplus_vaa.gpkg"), "NHDFlowline_Network", quiet=TRUE) %>%
#   filter(comid %in% lsh_fmain$comid)
#
# nhd_vaa_full <- sf::st_read(here("data_input/nhdplus_vaa.gpkg"), "NHDFlowline_Network", quiet=TRUE) %>%
#   filter(comid %in% lsh_flowline$comid)
#
# # get nhd catchments
# catch_vaa <- sf::st_read(here("data_input/nhdplus_vaa.gpkg"), "CatchmentSP", quiet=TRUE)



