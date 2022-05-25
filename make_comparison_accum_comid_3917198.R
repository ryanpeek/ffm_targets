# make comparison between accumulated metrics for COMID 3917198

# Get Libraries -----------------

library(tidyverse)
library(janitor)
library(glue)

# Get Data ----------------------------------------------------------------

# files w orig data
inputfiles <- list.files("data_input", pattern = "^3917.")
file_list <- c(glue("data_input/{inputfiles}"))

# read in
orig_df <- read_csv(file_list)

# add info
orig_df <- orig_df %>%
  mutate(datatype = "orig", .after=comid)

# revised
rev <- read_csv("data_output/accumulated_raw_metrics_north.csv") %>%
  filter(comid %in% c(unique(orig_df$comid))) %>%
  mutate(datatype="revised", .after=comid)

# check names and bind:
names(orig_df)[220:263]
names(rev)[220:263]

# set names as same in both:
names(rev) <- names(orig_df)
compare_df_cols(rev, orig_df)

# bind
df_all <- bind_rows(orig_df, rev) %>%


write_csv(rev, "data_output/accum_comparison.csv")

names(rev)

# Compare all Years One Metric --------------------------------------------

ggplot() +
  geom_point(data=df_all, aes(x=wa_yr, y=ppt_jan_wy, group=wa_yr, color=modtype, shape=modtype))


ggplot() +
  geom_point(data=df_all, aes(x=wa_yr, y=tav_sum1, group=wa_yr, color=modtype, shape=modtype))

ggplot() +
  geom_point(data=df_all, aes(x=wa_yr, y=ucs, color=modtype, shape=modtype), alpha=0.5, size=5)

# krug?
ggplot() +
  geom_point(data=raw, aes(x=wa_yr, y=krug_runoff)) +
  geom_point(data=rev, aes(x=wa_yr, y=krug_runoff), color="red") +
  ylim(c(2, 6))

