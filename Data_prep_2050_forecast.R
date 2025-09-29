# ==============================================================================
# Extracting data from IHME population forecast, perio and edentulism prevalence data
# ==============================================================================

library(tidyverse)
library(countrycode)

population_forecast <- read_csv("data/IHME_population_forecast.csv") %>% # Data taken from IHME (https://ghdx.healthdata.org/record/ihme-data/global-population-forecasts-2017-2100)
  filter(scenario == 0, year_id %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  select(starts_with(c("location", "year", "measure", "val", "upper", "lower"))) %>%
  mutate(iso3c = countrycode(location_name, origin = "country.name", destination = "iso3c")) %>% # matching with ISO code for country names
  filter(!is.na(iso3c)) %>% # drop all locations not corresponding to countries (largely regions and super regions)
  rename(Pop = val) %>%
  mutate(Pop_sd = (upper - Pop) / 2) %>%
  select(!(upper:lower)) %>%
  select(!(measure_id:measure_name)) # Noted that this dataset is missing Nauru, Palau, Saint Kitts and Nevis, Tuvalu and San Marino, which had to be taken from another dataset

UN_population_forecast <- read_csv("data/UN_world_population_prospects.csv") %>% # Data taken from UN World Population Prospects 2024 using the medium scenario (https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used)
  filter(iso3c %in% c("NRU", "TUV", "PLW", "KNA", "SMR", "MCO")) %>%
  filter(Year %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  select(iso3c, Year, Pop) %>%
  mutate(Pop = as.double(Pop)) %>%
  mutate(Pop = Pop * 1000) %>%
  rename(year_id = Year)

population_forecast <- bind_rows(population_forecast, UN_population_forecast) %>%
  select(!(location_id:location_name))

perio_prevalence_forecast <- read_csv("data/GBD_prev_perio.csv") %>% # Data taken from GBD forecasts
  filter(year_id %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  filter(age_group_name == "All Ages", sex == "Both") %>%
  mutate(iso3c = countrycode(location_name, origin = "country.name", destination = "iso3c")) %>% # matching with ISO code for country names
  filter(!is.na(iso3c)) %>%
  rename(Perio_prev = mean) %>%
  mutate(Perio_prev_sd = (upper - Perio_prev) / 2) %>%
  select(!(upper:lower)) %>%
  select(!(sex:age_group_name)) %>%
  select(!(location_id:location_name))


edent_prevalence_forecast <- read_csv("data/GBD_prev_edent.csv") %>% # Data taken from GBD forecasts
  filter(year_id %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  filter(age_group_name == "All Ages", sex == "Both") %>%
  mutate(iso3c = countrycode(location_name, origin = "country.name", destination = "iso3c")) %>% # matching with ISO code for country names
  filter(!is.na(iso3c)) %>%
  rename(Edent_prev = mean) %>%
  mutate(Edent_prev_sd = (upper - Edent_prev) / 2) %>%
  select(!(upper:lower)) %>%
  select(!(sex:age_group_name)) %>%
  select(!(location_id:location_name))


joined_list <- list(perio_prevalence_forecast, edent_prevalence_forecast, population_forecast)

joined_forecast <- reduce(joined_list, left_join, by = c("year_id", "iso3c"))


# ==============================================================================
# Creating datasets for each year, joining by iso3c, and adjusting procedure costs for medical inflation
# ==============================================================================

existing_input <- read_csv("data/combined_country_input.csv")

existing_input_stripped <- existing_input %>%
  select(!(Pop:Perio_prev_sd)) %>%
  mutate(
    Country = ifelse(Country == "Micronesia", # Micronesia is the only country that is not captured by the countrycodes package
      "Micronesia (Federated States of)",
      Country
    ),
    iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")
  )

# Prepare 2025 file, joining by iso3c

combined_country_input_2025 <- joined_forecast %>%
  filter(year_id == 2025) %>%
  select(!(year_id)) %>%
  right_join(existing_input_stripped) %>%
  write_csv("data/combined_country_input_2025.csv")

# Regarding estimate for medical inflation, would refer to Dunn et al. (2016) (https://pmc.ncbi.nlm.nih.gov/articles/PMC5785315/)
# Based on CPI-U vs the Medical Care component of CPI, the medical care component has been broadly in line with real inflation
# In fact in 2021 and 2022 it was outstripped by overall CPI by 3-4%
# Therefore, no further adjustments were made for medical inflation

# Prepare 2030 file

combined_country_input_2030 <- joined_forecast %>%
  filter(year_id == 2030) %>%
  select(!(year_id)) %>%
  right_join(existing_input_stripped) %>%
  write_csv("data/combined_country_input_2030.csv")

# Prepare 2035 file

combined_country_input_2035 <- joined_forecast %>%
  filter(year_id == 2035) %>%
  select(!(year_id)) %>%
  right_join(existing_input_stripped) %>%
  write_csv("data/combined_country_input_2035.csv")

# Prepare 2040 file

combined_country_input_2040 <- joined_forecast %>%
  filter(year_id == 2040) %>%
  select(!(year_id)) %>%
  right_join(existing_input_stripped) %>%
  write_csv("data/combined_country_input_2040.csv")

# Prepare 2045 file

combined_country_input_2045 <- joined_forecast %>%
  filter(year_id == 2045) %>%
  select(!(year_id)) %>%
  right_join(existing_input_stripped) %>%
  write_csv("data/combined_country_input_2045.csv")


# Prepare 2050 file

combined_country_input_2050 <- joined_forecast %>%
  filter(year_id == 2050) %>%
  select(!(year_id)) %>%
  right_join(existing_input_stripped) %>%
  write_csv("data/combined_country_input_2050.csv")



# ==============================================================================
# Converting the GBD super-region, region and location hierarchy into a wide format
# ==============================================================================

library(tidyverse)

# Step 1: Read the CSV
df <- read_csv("data/GBD_location_hierarchy.csv")

# Step 2 & 3: Join hierarchy
df_wide <- df %>%
  select(
    Location_ID = `Location ID`,
    Parent_ID = `Parent ID`,
    Level,
    Location_Name = `Location Name`
  ) %>%
  mutate(Level = as.integer(Level)) %>%
  {
    countries <- filter(., Level == 3)
    regions <- filter(., Level == 2)
    supers <- filter(., Level == 1)

    # Join countries to regions
    countries %>%
      left_join(regions,
        by = c("Parent_ID" = "Location_ID"),
        suffix = c("_country", "_region")
      ) %>%
      rename(
        Country = Location_Name_country,
        Region = Location_Name_region,
        Region_Parent_ID = Parent_ID_region
      ) %>%
      # Join regions to superregions
      left_join(supers,
        by = c("Region_Parent_ID" = "Location_ID")
      ) %>%
      rename(Superregion = Location_Name)
  } %>%
  select(Country, Region, Superregion) %>%
  arrange(Superregion, Region, Country)

# Step 4: Save to CSV
write_csv(df_wide, "data/GBD_location_hierarchy_wide.csv")

# Step 5: Quick check
df_wide %>%
  group_by(Superregion, Region) %>%
  slice_head(n = 3) %>%
  print(n = 20)
