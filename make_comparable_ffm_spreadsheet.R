# pull in different FFM iterations and compare

library(readr)
library(dplyr)
library(glue)

# Get Data ----------------------------------------------------------------

ffm_full <- read_csv("data_output/ffm_preds_summary/final_ffm_preds_lshasta_fullwatershed_LOIs_median.csv")

ffm_min <- read_csv("data_output/ffm_preds_summary/final_ffm_preds_lshasta_minwatershed_LOIs_median.csv")

ffm_north <- read_csv("data_output/ffm_preds_summary/final_ffm_preds_lshasta_northwatershed_LOIs_median.csv")

ffm_orig <- read_csv("data_output/ffm_preds_summary/final_ffm_preds_lshasta_original.csv")


# Filter to COMIDs and p50 ------------------------------------------------

lois <- unique(ffm_full$comid)

ffm_orig <- ffm_orig %>% filter(comid %in% lois) %>%
  select(comid, metric, orig_p50=p50, flow_component)

ffm_full <- ffm_full %>%
  select(comid, metric, full_p50=p50)

ffm_min <- ffm_min %>%
  select(comid, metric, min_p50=p50)

ffm_north <- ffm_north %>%
  select(comid, metric, north_p50=p50)

# Join Data ---------------------------------------------------------------

ffm_j1 <- left_join(ffm_full, ffm_min) %>%
  left_join(ffm_north)

# now add orig
ffm_j2 <- left_join(ffm_j1, ffm_orig)

# round each to 3 dig
ffm_out <- ffm_j2 %>%
  mutate(across(full_p50:orig_p50, ~round(., 3))) %>%
  # make comid a factor upstream to downstream
  mutate(comid = factor(comid, levels = c(3917198, 3917950, 3917946))) %>%
  arrange(metric, comid)

# write out ---------------------------------------------------------------

write_csv(ffm_out, file = glue("data_output/ffm_preds_summary/ffm_p50_comparison_{Sys.Date()}.csv"))
