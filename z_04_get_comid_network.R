# 04_get_comid_network
# this script pulls revised flowline data for use in accumulation network
# and list of comids
# this is a modified version of the function from the NHDtoolsplus package
# edited by R. Peek, 2022-May-09
# get u/s trib comids from starting comid
# requires a full network dataset that contains these cols:
# c("comid", "tocomid", "lengthkm", "levelpathi","pathlength", "hydroseq","dnhydroseq")
# length and pathlengths should be accurate
# pass COMID to start from
# and distance measure (to search within if needed)


# Function ----------------------------------------------------------------

# needs flowline data and location for comlist save location
f_get_comid_network <- function(flowlines){

  print(glue("making clean flowlines layer..."))
  # clean flowline layer (to use for mapping and getting comid list)
  flowlines_trim <- flowlines %>%
    # remove all the junk just get what we need
    select(comid, fromnode, tonode, divergence, ftype,
           areasqkm, lengthkm, totdasqkm, terminalID,
           sort_order, arbolatesum, gnis_id, hydroseq) %>%
    st_transform(4269)
  #return(flowlines_trim)

  ## MAKE THE NETWORK ------------------------

  print(glue("making flowline network for accumulation..."))
  # get pathlengths: need ID, toID, length
  flowlines_net <- flowlines %>%
    select(ID=comid, toID=tocomid,
           lengthkm, levelpathi, hydroseq, dnhydroseq,
           areasqkm, area_weight, totdasqkm,
           sort_order, arbolatesum) %>%
    # this updates the pathlength total (main path to basin terminal)
    left_join(., nhdplusTools::get_pathlength(.)) %>%
    relocate(pathlength, .after=lengthkm)

  ## FUCTION TO GET LIST OF COMIDS U/S ------------------
  # this is the slightly modified nhdplusTools:::get_UT function.

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

  # make list of us comids for each comid:
  comlist <- map(flowlines_net$ID, ~get_us_comids(flowlines_net, .x))

  # make it named
  comlist <- comlist %>%
    set_names(flowlines_net$ID)

  # see which are tips (only single comid)
  #keep(comlist, ~length(.x)==1) %>% names(.) %>% as.numeric()

  # get only items that AREN'T outlet or TIPS
  #keep(comlist, ~length(.x)!=1) %>% names(.) %>% as.numeric()

  return(list(comids = flowlines_trim , comlist_accum = comlist))
}
