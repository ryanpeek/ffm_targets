# COMBINE PREDICTIONS BY COMID

# LIBRARIES ---------------------------------------------------------------

# library(tidyverse)
# options(dplyr.summarise.inform = FALSE) # turn off messages
# library(glue) # better pasting of things together
# library(fs) # more stable file paths
# library(purrr)
# library(janitor)

# requires
# filepaths <- ffm_predictions
# outdir = "data_output/ffm_preds_summary"
# filename = "final_ffm_preds_lshasta_north"


f_combine_ffm <- function(filepaths, outdir, filename){

  #filepaths <- fs::dir_ls(glue("{indir}"), glob = "*.csv")
  filenames <- gsub(path_file(filepaths), pattern = ".csv", replacement = "") %>%
    # need to fix the names and drop comids
    stringr::str_remove(pattern = "_[0-9]{7}+$")

  # check for lengths
  stopifnot(length(filenames)==length(filepaths))  # must be same

  # read in data and add column of metric names
  dat <- map2_df(filepaths, filenames, ~read_csv(.x) %>%
                   mutate(metric = .y)) %>%
    arrange(comid)

  # write single file
  fs::dir_create(outdir)
  write_csv(dat, file = glue("{outdir}/{filename}_median.csv"))

  # save LOI version only, us, to ds
  lois <- c(3917198, 3917950, 3917946)
  dat %>% filter(comid %in% lois) %>%
    write_csv(., file = glue("{outdir}/{filename}_LOIs_median.csv"))

}
