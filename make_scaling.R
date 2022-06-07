
# tst
orig <- c(0.846889284,0.248925403,0.501705486,0.166294114,0.094901113,0.206849681,7.60E-04,0.060263293,0.150875397,1.428891536,0.362611291,0.591286056)
new <- c(155.99,45.85,92.41,30.63,17.48,38.1,0.14,11.1,27.79,263.19,66.79,108.91)

scale(new)


library(tidyverse)

# get accum data
dat <- read_csv("data_output/accum_comparison_north_v2.csv")

# scale for a given comid
dat1 <- dat %>%
  filter(comid == 3917136, datatype=="revised") %>%
  select(wa_yr, starts_with("ppt"))

dat1 %>%
  mutate(across(ppt_jan_wy:ppt_dec_wy, ~scale(.x))) %>% View()
