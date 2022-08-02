# make comparison between accumulated metrics for COMID 3917198

# Get Libraries -----------------

library(tidyverse)
library(janitor)
library(glue)

# Set Version -------------------------------------------------------------

vrsn <- "_v4" # _v4 or ""

# Get Data ----------------------------------------------------------------

# get xwalk for names
xwalk <- read_csv("data_input/08_accumulated_final_xwalk.csv")

# files w orig data from Daren
library(readxl)
orig_df <- readxl::read_xlsx("data_output/comidpreds4ryan_rev.xlsx", sheet = 1) %>%
  # select and order properly
  select(xwalk$mod_input_raw)
names(orig_df) <- xwalk$mod_input_final

# add info
orig_df <- orig_df %>%
  mutate(datatype = "orig", .after=comid)

# revised
rev <- read_csv(glue("data_output/accumulated_raw_metrics_north{vrsn}.csv")) %>%
  filter(comid %in% c(unique(orig_df$comid))) %>%
  select(xwalk$mod_input_final) %>%
  mutate(datatype="revised", .after=comid)

# check names and bind:
names(orig_df)[220:263] == names(rev)[220:263]

# set names as same in both:
compare_df_cols(rev, orig_df)

# bind
df_all <- bind_rows(orig_df, rev) %>%
  arrange(comid, wa_yr)

# write out
write_csv(df_all, file = glue("data_output/accum_comparison_north{vrsn}.csv"))

# Add Labels --------------------------------------------------------------

headwaters <- c(3917136, 3917138, 3917194)
loi <- 3917198
nonhw <- c(3917198, 3917914)
df_all <- df_all %>%
  mutate(comtype = case_when(
    comid %in% headwaters ~ "headwater",
    comid %in% nonhw ~ "non-headwater"
  ), .after=comid)



# Compare all Years One Metric --------------------------------------------

#
# var_sel <- "ppt_jan_wy"
#
# df_all2 %>% filter(comtype=="headwater") %>%
#   ggplot() +
#   geom_point(aes(x=wa_yr, y=.data[[var_sel]], group=wa_yr, color=as.factor(comid), shape=datatype, alpha=datatype), size=3) +
#   scale_color_discrete("Headwater COMID")+
#   theme_classic()+
#   labs(title="Accumulated Data Comparison")
#
# var_sel2 <- "ppt_jun_wy"
# df_all2 %>% filter(comtype!="headwater") %>%
# ggplot() +
#   geom_point(aes(x=wa_yr, y=.data[[var_sel2]], group=wa_yr, shape=datatype, color=as.factor(comid)), size=2.7, alpha=0.9) +
#   scale_color_brewer("Type", palette = "Set2")+
#   scale_shape_discrete("Accumulation Data")+
#   theme_classic()+
#   labs(title=glue("Accumulated Data Comparison: {var_sel2}"))
#
# # krug?
# var_sel3 <- "krug_runoff"
# df_all2 %>% filter(comtype!="headwater") %>%
#   ggplot() +
#   geom_point(aes(x=wa_yr, y=.data[[var_sel3]], group=wa_yr, shape=datatype, color=as.factor(comid)), size=2.7, alpha=0.9) +
#   scale_color_brewer("Type", palette = "Set2")+
#   scale_shape_discrete("Accumulation Data")+
#   theme_classic()+
#   labs(title=glue("Accumulated Data Comparison: {var_sel3}"))
#
#
# # Calculations ------------------------------------------------------------
#
# # pick headwater and calc a single year
# df_all2 %>% filter(comid==3917136) %>%
#   group_by(datatype) %>%
#   select(ppt_jan_wy) %>%
#   summarize(mean(ppt_jan_wy))
#
# # summary per all years for one var
# df_all2 %>% filter(comid==3917136 & datatype=="orig_usgs") %>%
#   select(ppt_jan_wy) %>% summary()
#
# df_all2 %>% filter(comid==3917136 & datatype=="revised") %>%
#   select(ppt_jan_wy) %>% summary()
#
# # if scaled what is summary?
# df_all2 %>% filter(comid==3917136, datatype=="revised") %>%
#   select(ppt_jan_wy) %>% summarize(scale(ppt_jan_wy)) %>% summary()
#
# df_all2 %>% filter(comid==3917136, datatype=="orig_usgs") %>%
#   select(ppt_jan_wy) %>% summarize(scale(ppt_jan_wy)) %>% summary()
#
# df_all2 %>% filter(comid==3917136, datatype=="orig") %>%
#   select(ppt_jan_wy) %>% summary()
#
# df_all2 %>% filter(comid==3917136 & datatype=="orig") %>%
#   group_by(datatype) %>%
#   select(ppt_jan_wy) %>%
#   summarize(scale(ppt_jan_wy))
