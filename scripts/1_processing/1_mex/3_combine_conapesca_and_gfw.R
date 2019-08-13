######################################
#   Combine conapesca and gfw data   #
######################################

# Load packages
library(here)
library(tidyverse)

# Load data
# Effort and characteristics from GFW
mex_ps <- readRDS(here("data", "mexican_purse_seiners_monthly_raster_by_vessel.rds"))

# Landings data from conapesca
conapesca_bq <- readRDS(here("data", "conapesca_bq.rds")) %>% 
  filter(category == "Atun")

# Combine the datasets
data <- full_join(mex_ps, conapesca_bq, by = c("year", "month", "shipname")) %>% 
  drop_na()

# export data
saveRDS(object = data,
        file = here("data", "mexican_purse_seines.rds"))

# END OF SCRIPT