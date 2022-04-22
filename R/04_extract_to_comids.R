# extract zipped data from NHD and filter to comids all in same function (not looped)
# R. Peek

#library(vroom)
#library(glue)
#library(dplyr)
#library(fs)

# requires:
## path to zipped file with a COMID column
## list of the comids to filter by
## out directory

f_extract_to_comids <- function(infile, comids, outdir){

  if(!file.exists(glue("{infile}"))){

    print(glue("Reading {path_file(infile)}"))
    f1 <- vroom(infile, show_col_types = FALSE) %>% # fast readin
      dplyr::rename_with(., toupper) %>%
      dplyr::filter({if("COMID" %in% names(.)) COMID else NULL} %in% comids)

    # check dimensions (one way to filter out zeros)
    if(!nrow(f1)>0){
      stop("no data in file")
    }

    print(glue("There are {nrow(f1)} rows"))

    # get filename
    f1 <- f1 %>% mutate(filename = fs::path_file(infile))

    # Write out to single csv -----------------------------------------------
    print("Writing out files")
    fs::dir_create(outdir)
    write_csv(f1, file = glue("{outdir}/{path_ext_remove(path_file(infile))}.csv"))
  }
  else(print(glue("{infile} exists")))

  #ziplist <- get_zip_list(glue("{outdir}"), extension = "*csv")

}
