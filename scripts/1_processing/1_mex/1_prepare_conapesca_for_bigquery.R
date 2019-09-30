
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
conapesca <- read.csv(here("raw_data", "conapesca_2012_2019.csv"),
                      stringsAsFactors = FALSE, skip = 1) %>% 
  clean_names() %>% 
  mutate_at(vars(nombre_oficina, nombre_principal, nombre_estado, nombre_especie, mes_corte, nombre_sitio_desembarque), tolower) %>% 
  mutate(ano_corte = ifelse(ano_corte == "2019/P", "2019", ano_corte),
         ano_corte = as.numeric(ano_corte))

# List of states in the Pacific Ocean
pac_states <- tolower(c("Baja california",
                        "Baja california sur",
                        "Chiapas",
                        "Colima",
                        "Guerrero",
                        "Jalisco",
                        "Michoacan",
                        "Nayarit",
                        "Oaxaca",
                        "Sinaloa",
                        "Sonora"))

# Apply filters for spp of interest and states
conapesca_bq <- conapesca %>%
  filter(nombre_estado %in% pac_states)

# Get shipnames to normalize
shipnames <- unique(conapesca_bq$nombre_activo) %>% 
  sort()

plan(multiprocess)
shipnames_norm <- future_map_chr(shipnames, normalize_shipname)

# create a couple of dictionaries
# Shipnames
shipname_dictionary <- tibble(nombre_activo = shipnames,
                              shipname_norm = shipnames_norm)

# Months Esp - Eng - num
months <- tibble(mes_corte = c("enero",
                               "febrero",
                               "marzo",
                               "abril",
                               "mayo",
                               "junio",
                               "julio",
                               "agosto",
                               "septiembre",
                               "octubre",
                               "noviembre",
                               "diciembre"),
                 month = c(1:12))

# Join to the dictionaries and order columns
conapesca_bq <- conapesca_bq %>% 
  left_join(shipname_dictionary, by = "nombre_activo") %>% 
  left_join(months, by = "mes_corte") %>% 
  rename(shipname = shipname_norm,
         state = nombre_estado,
         reporting_office = nombre_oficina,
         year = ano_corte,
         category = nombre_principal,
         commodity = nombre_especie,
         landings = peso_desembarcado_kilogramos,
         catches = peso_vivo_kilogramos,
         price = precio_pesos,
         value = valor_pesos) %>% 
  mutate(date = date(paste(year, month, 15, sep = "-"))) %>%
  group_by(year, month, date, shipname, category, commodity) %>% 
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

# END OF SCRIPT