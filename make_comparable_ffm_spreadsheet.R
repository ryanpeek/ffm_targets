# pull in different FFM iterations and compare

library(readr)
library(dplyr)
library(glue)

# Get Data ----------------------------------------------------------------

ffm_north <- read_csv("data_output/ffm_preds_summary/final_ffm_preds_lshasta_north_LOIs_median.csv")

ffm_orig <- read_csv("data_output/ffm_preds_summary/final_ffm_preds_lshasta_original.csv")

# Filter to COMIDs and p50 ------------------------------------------------

lois <- unique(ffm_north$comid)

ffm_orig <- ffm_orig %>% filter(comid %in% lois) %>%
  select(comid, metric, orig_p50=p50, flow_component)

ffm_north <- ffm_north %>%
  select(comid, area, metric, north_p50=p50) %>%
  # fix the metric name
  tidyr::separate(metric, sep = "[_[:num:]_north")

# Join Data ---------------------------------------------------------------

# join
ffm_df <- left_join(ffm_orig, ffm_north) %>%
  select(comid:metric, ends_with("p50"), flow_component)

# round each to 3 dig
ffm_out <- ffm_df %>%
  mutate(across(ends_with("p50"), ~round(., 3))) %>%
  # make comid a factor upstream to downstream
  mutate(comid = factor(comid, levels = c(3917198, 3917950, 3917946))) %>%
  arrange(metric, comid)

# write out ---------------------------------------------------------------

write_csv(ffm_out, file = glue("data_output/ffm_preds_summary/ffm_p50_comparison_{Sys.Date()}.csv"))
