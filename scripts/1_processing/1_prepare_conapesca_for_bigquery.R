
###################################
# prepare_conapesca_for_bigquery  #
###################################


# Load packages
library(lubridate)
library(here)
library(startR)
library(furrr)
library(tidyverse)

# Function to normalize strings
normalize_string <- function(x) {
  x %>%
    toupper() %>%
    str_remove_all("[^[:alnum:]]") %>%
    str_trim()
}

# Function to convert factors into characters
remove_factors <- function(x){
  char <- as.character(x)
  if(!is.numeric(x)){
    return(char)
  } else {
    num <- as.numeric(char)
    return(num)
  }
}

# Read the data and convert all to character and numeric
conapesca <- readRDS(here("raw_data", "conapesca.rds")) %>% 
  mutate_all(.funs = remove_factors)

# List of species of interest
spp_interest <- c("Atun", "Barrilete", "Bonito", "Sardina")

# Filter species of interest
conapesca_bq <- conapesca %>%
  filter(NombrePrincipal %in% spp_interest)

# Get shipnames to normalize
shipnames <- unique(conapesca_bq$NombreActivo) %>% 
  sort()

plan(multiprocess)
shipnames_norm <- future_map_chr(shipnames, normalize_shipname)

# create a couple of dictionaries
# Shipnames
shipname_dictionary <- tibble(NombreActivo = shipnames,
                              ShipnameNorm = shipnames_norm)

# Months Esp - Eng - num
months <- tibble(Mes = c("Enero",
                         "Febrero",
                         "Marzo",
                         "Abril",
                         "Mayo",
                         "Junio",
                         "Julio",
                         "Agosto",
                         "Septiembre",
                         "Octubre",
                         "Noviembre",
                         "Diciembre"),
                 month = c(1)
                 )

# Join to the dictionaries and order columns
conapesca_bq <- conapesca_bq %>% 
  left_join(shipname_dictionary, by = "NombreActivo") %>% 
  left_join(months, by = "Mes") %>% 
  rename(shipname = ShipnameNorm,
         owner = UnidadEconomica,
         state = Estado,
         reporting_office = Oficina,
         year = Ano,
         category = NombrePrincipal,
         commodity = NombreComun,
         species = NombreCientifico,
         landings = PesoDesembarcado,
         catches = PesoVivo,
         price = Precio,
         value = Valor
         ) %>% 
  mutate(date = date(paste(year, month, 1, sep = "-"))) %>%
  select(year,
         month,
         date,
         shipname,
         owner,
         category,
         commodity,
         species,
         landings,
         catches,
         price,
         value)

# Export data
saveRDS(object = conapesca_bq,
        file = here("data", "conapesca_bq.rds"))
  

# END