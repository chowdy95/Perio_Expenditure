#---------------------------------------------------------------------------------------------------------

# This is to prep and combine data on GDP per capita, population and prevalence from GBD with procedure costs

# Load packages
library(tidyverse)
library(countrycode)

gdp_per_capita <- read_csv("data/GBD_gdp_per_capita.csv") %>% # Taken from https://ghdx.healthdata.org/record/ihme-data/global-gdp-per-capita-1960-2050-fgh-2023
  filter(level == "Country", year == 2021) %>%
  select(location_name, iso3, gdp_ppp_mean, gdp_ppp_upper, gdp_ppp_lower) %>%
  rename(GDP_per_capita_PPP_2021 = gdp_ppp_mean)

population <- read_csv("data/GBD_population_2021.csv") %>% # Taken from https://vizhub.healthdata.org/gbd-results/
  select(location_name, val, upper, lower) %>%
  rename(Pop = val) %>%
  mutate(Pop_sd = (upper - Pop) / 2) %>%
  select(-upper, -lower)

prevalence <- read_csv("data/GBD_prev_2021.csv") %>% # Taken from https://vizhub.healthdata.org/gbd-results/
  select(location_name, cause_name, val, upper, lower)

edent <- prevalence %>%
  filter(cause_name == "Edentulism") %>%
  rename(Edent_prev = val) %>%
  mutate(Edent_prev_sd = (upper - Edent_prev) / 2) %>%
  select(-upper, -lower, -cause_name)

perio <- prevalence %>%
  filter(cause_name == "Periodontal diseases") %>%
  rename(Perio_prev = val) %>%
  mutate(Perio_prev_sd = (upper - Perio_prev) / 2) %>%
  select(-upper, -lower, -cause_name)

full_dataset <- population %>%
  left_join(edent, by = "location_name") %>%
  left_join(perio, by = "location_name") %>%
  left_join(gdp_per_capita, by = "location_name")

# write_csv(gdp_per_capita, "data/gdp_ppp_manually_cleaned.csv")
# write_csv(full_dataset, "data/gbd_prevalence_population.csv")

# country_input <- read_csv("data/country_input.csv") %>%   # To ensure no transcription mistakes as this smaller dataset had been transcribed by hand previously
#   select(-(Perio_prev:Pop_sd)) %>%        # Run only once as it overwrites its original file
#   left_join(full_dataset, by = c("Country" = "location_name"))
#
# write_csv(country_input, "data/country_input.csv")

dental_expenditure <- read_csv("./data/gbd_dental_expenditure.csv") %>%
  mutate(Country = ifelse(Country == "Micronesia", # Micronesia is the only country that is not captured by the countrycodes package
    "Micronesia (Federated States of)",
    Country
  )) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  select(-Country) %>%
  full_join(full_dataset, by = c("iso3c" = "iso3")) %>%
  rename(Country = location_name)

write_csv(dental_expenditure, "data/2021_prevalence_pop_dentexp_GDP.csv") # For use as an input in Procedure Cost Extrapolation Ratio R file

# See Procedure Cost Extrapolation_ratio_approach_GDP_PPP for further data prep involving prediction of other countries' procedure costs
