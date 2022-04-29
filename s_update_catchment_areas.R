# update Catchment areas

# Catchments --------------------------------------------------------------

# get catchments
catch_lsh <- read_rds("data_clean/catchments_final_lshasta.rds")
# FEATUREID here is original COMID from catchment
# comid and comid_f are tied to the flowlines

# compare areas and get updated areas
catch_areas <- tibble(comid_catch=catch_lsh$FEATUREID,
                      comid_flowline=catch_lsh$comid,
                      "area_old"=catch_lsh$AreaSqKM,
                      "area_sf"=sf::st_area(catch_lsh) %>%
                        units::set_units("km^2") %>%
                        units::drop_units() %>% round(digits=3))

# bind back to sf
catch_areas <- left_join(catch_areas, catch_lsh,
                         by=c("comid_catch"="FEATUREID")) %>%
  select(-c(SOURCEFC:AreaSqKM, comid:comid_f)) %>%
  st_as_sf()

# quick preview
# catch_areas %>% mutate(comid_flowline=as.factor(comid_flowline)) %>% mapview(zcol="comid_flowline") + mapview(flow_trim, color="steelblue")

# filter out small slivers and get just comids of interest
catch_areas_filt <- catch_areas %>%
  filter(comid_flowline %in% flow_trim$comid) # n=76 comids

#mapview(catch_areas_filt, zcol="comid_flowline") +
#  mapview(flow_trim, zcol="comid")

# get total area
(lsh_area <- sum(catch_areas_filt$area_sf)) # 329.519 sqkm

# add area weight:
catch_areas_filt <- catch_areas_filt %>%
  mutate(area_weight = area_sf/lsh_area, .after="area_sf")

catch_areas_filt_no_sf <- st_drop_geometry(catch_areas_filt)

# write out and use to filter to comids of interest
#write_csv(catch_areas_filt_no_sf, file = "data_clean/08_catch_areas_filt_comid.csv")
