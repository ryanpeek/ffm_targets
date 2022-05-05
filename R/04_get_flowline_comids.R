# 04_clean_nhd_flowlines
# this script pulls catchment and flowline data for use in accumulation and filtering of the data

# Get LOIs and Flowlines --------------------------------------------------

f_get_comids <- function(data){

  #flowlines <- read_rds(glue("{data}")) %>%
  flowlines <- data %>%
    # remove all the junk just get what we need
    select(comid, fromnode, tonode, divergence, ftype,
           areasqkm, lengthkm, totdasqkm, terminalID,
           sort_order, arbolatesum, gnis_id, hydroseq) %>%
    st_transform(4269)
  return(flowlines)
}

