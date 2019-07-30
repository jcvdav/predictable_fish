
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

# Function to get percentiles of interest
get_out <- function(x) {
  q <- quantile(x, probs = 0.95)
  return(q)
}

# Read the data and convert all to character and numeric
conapesca <- readRDS(here("raw_data", "conapesca.rds")) %>% 
  mutate_all(.funs = remove_factors)

# List of species of interest
spp_interest <- c("Atun",
                  "Barrilete", 
                  "Bonito")

# List of states in the Pacific Ocean
pac_states <- c("Baja california",
                "Baja california sur",
                "Chiapas",
                "Colima",
                "Guerrero",
                "Jalisco",
                "Michoacan",
                "Nayarit",
                "Oaxaca",
                "Sinaloa",
                "Sonora")

# Apply filters for spp of interest and states
conapesca_bq <- conapesca %>%
  filter(NombrePrincipal %in% spp_interest) %>% 
  filter(Estado %in% pac_states)

# Get shipnames to normalize
shipnames <- unique(conapesca_bq$NombreActivo) %>% 
  sort()

plan(multiprocess)
shipnames_norm <- future_map_chr(shipnames, normalize_shipname)

# create a couple of dictionaries
# Shipnames
shipname_dictionary <- tibble(NombreActivo = shipnames,
                              ShipnameNorm = shipnames_norm)

# Manual modifications to names in the dictionary
shipname_dictionary <- shipname_dictionary %>% 
  mutate(ShipnameNorm = case_when(ShipnameNorm == "CLIPEPERTON" ~ "CLIPPERTON",
                                  ShipnameNorm == "MARIAANTONIETA" ~ "MAANTONIETA",
                                  T ~ ShipnameNorm))

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
                 month = c(1:12))

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
         value = Valor) %>% 
  mutate(date = date(paste(year, month, 1, sep = "-"))) %>%
  group_by(year, month, date, shipname, owner, category, commodity, species) %>% 
  summarize(landings = sum(landings, na.rm = T),
            catches = sum(catches, na.rm = T),
            value = sum(value, na.rm = T),
            price = round(value / landings,)) %>% 
  ungroup()

# Get percentiles
landings_pct <- get_out(conapesca_bq$landings)
catches_pct <- get_out(conapesca_bq$catches)
value_pct <- get_out(conapesca_bq$value)
price_pct <- get_out(conapesca_bq$price)

# Continue filtering
conapesca_bq <- conapesca_bq %>% 
  filter(between(landings, 0, landings_pct),
         between(catches, 0, catches_pct),
         between(value, 0, value_pct),
         between(price, 0, price_pct)) %>% 
  select(year,
         month,
         date,
         shipname,
         category,
         commodity,
         species,
         landings,
         catches,
         value,
         price) %>% 
  arrange(year,
          month,
          date,
          shipname,
          category,
          commodity,
          species,
          landings,
          catches,
          value,
          price)

# Export data
saveRDS(object = conapesca_bq,
        file = here("data", "conapesca_bq.rds"))

write.csv(x = conapesca_bq,
          file = here("data", "conapesca_bq.csv"),
          row.names = F)

# END