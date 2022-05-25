# make comparison between accumulated metrics for COMID 3917198

# Get Libraries -----------------

library(tidyverse)
library(glue)


# Get Data ----------------------------------------------------------------

# the original raw data:
raw <- read_csv("data_input/3917198_accum_raw.csv") %>%
  mutate(modtype="raw")

# revised
rev <- read_csv("data_output/accumulated_raw_metrics_north.csv") %>%
  filter(comid==3917198) %>%
  mutate(modtype="revised")

#df_all <- bind_rows(raw, rev)

write_csv(rev, "data_output/3917198_accum_revised_north.csv")

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

