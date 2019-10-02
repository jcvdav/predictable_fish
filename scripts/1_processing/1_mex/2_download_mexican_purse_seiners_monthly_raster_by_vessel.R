##############################################################
## download_mexican_purse_seiners_monthly_raster_by_vessel ##
##############################################################

# Downloads the table from GBQ, performs a filter,
# and then output it to the data folder

# Load packages
library(startR)
library(here)
library(tidyverse)

# Get the table from GBQ
mex_ps <- get_table(project = "ucsb-gfw",
                    dataset = "jc_predictable_fish",
                    table = "mexican_purse_seiners_monthly_raster_by_vessel") %>%
  # filter(between(lat_min, -30, 35),
         # between(lon_min, -160, 0)) %>% 
  mutate(lat_range = abs(lat_max - lat_min),
         lon_range = abs(lon_max - lon_min)) %>% 
  select(-c(lat_max, lon_max))

# Export the table as an RDS object
saveRDS(object = mex_ps,
        file = here("data", "mexican_purse_seiners_monthly_raster_by_vessel.rds"))

# END OF SCRIPT