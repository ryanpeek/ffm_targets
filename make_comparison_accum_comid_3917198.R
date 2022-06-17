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
  arrange(comid, wa_yr)

# write out
#write_csv(df_all, "data_output/accum_comparison_north_v2.csv")

# Add Labels --------------------------------------------------------------

headwaters <- c(3917136, 3917138, 3917194)
loi <- 3917198
nonhw <- c(3917198, 3917914)
df_all <- df_all %>%
  mutate(comtype = case_when(
    comid %in% headwaters ~ "headwater",
    comid %in% nonhw ~ "non-headwater"
  ))


# Compare all Years One Metric --------------------------------------------

var_sel <- "ppt_jan_wy"

df_all %>% filter(comtype=="headwater") %>%
  ggplot() +
  geom_point(aes(x=wa_yr, y=.data[[var_sel]], group=wa_yr, color=as.factor(comid), shape=datatype), size=2.7) +
  scale_color_discrete("Headwater COMID")+
  theme_classic()+
  labs(title="Accumulated Data Comparison")

var_sel2 <- "tav_sum1"
df_all %>% filter(comtype!="headwater") %>%
ggplot() +
  geom_point(aes(x=wa_yr, y=.data[[var_sel2]], group=wa_yr, shape=datatype, color=as.factor(comid)), size=2.7, alpha=0.9) +
  scale_color_brewer("Type", palette = "Set2")+
  scale_shape_discrete("Accumulation Data")+
  theme_classic()+
  labs(title=glue("Accumulated Data Comparison: {var_sel2}"))

# krug?
var_sel3 <- "krug_runoff"
df_all %>% filter(comtype!="headwater") %>%
  ggplot() +
  geom_point(aes(x=wa_yr, y=.data[[var_sel3]], group=wa_yr, shape=datatype, color=as.factor(comid)), size=2.7, alpha=0.9) +
  scale_color_brewer("Type", palette = "Set2")+
  scale_shape_discrete("Accumulation Data")+
  theme_classic()+
  labs(title=glue("Accumulated Data Comparison: {var_sel3}"))


# Calculations ------------------------------------------------------------

# pick headwater and calc a single year
df_all %>% filter(comid==3917136) %>%
  group_by(datatype) %>%
  select(ppt_jan_wy) %>%
  summarize(mean(ppt_jan_wy))

# summary per all years for one var
df_all %>% filter(comid==3917136 & datatype=="orig") %>%
  select(ppt_jan_wy) %>% summary()

df_all %>% filter(comid==3917136 & datatype=="revised") %>%
  select(ppt_jan_wy) %>% summary()

# if scaled what is summary?
df_all %>% filter(comid==3917136 & datatype=="revised") %>%
  select(ppt_jan_wy) %>% summarize(scale(ppt_jan_wy)) %>% summary()

df_all %>% filter(comid==3917136 & datatype=="revised") %>%
  group_by(datatype) %>%
  select(ppt_jan_wy) %>%
  summarize(scale(ppt_jan_wy))
