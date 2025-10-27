
#------------

# To assess countries for expenditure per capita

library(tidyverse)
library(patchwork)

perio_expenditure <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
  rename(location_name = LocationHeader) %>%
  filter(Year ==2021) %>%
  mutate(
    base_exppc = 1e9*selected_Mean_total_billions/Pop)

# Top 5 expenditure per capita

perio_expenditure_countries <- perio_expenditure %>%
  filter(Level == 3)

top_5_exppc <- perio_expenditure_countries %>%
  filter(Level == 3, Year == 2021) %>%
  arrange(desc(base_exppc)) %>%
  slice(1:5) %>%
  select(iso3c)

top_5_exppc_df <- perio_expenditure_countries %>%
  inner_join(top_5_exppc, by = "iso3c") %>%
  mutate(location_name = fct_reorder(location_name, base_exppc)) %>%
  mutate(location_name = fct_rev(location_name)) %>%
  filter (Year == 2021) %>%
  select (base_exppc, everything())


# Bottom 5 expenditure per capita

perio_expenditure_countries <- perio_expenditure %>%
  filter(Level == 3)

bottom_5_exppc <- perio_expenditure_countries %>%
  filter(Level == 3, Year == 2021) %>%
  arrange(base_exppc) %>%
  slice(1:5) %>%
  select(iso3c)

bottom_5_exppc_df <- perio_expenditure_countries %>%
  inner_join(bottom_5_exppc, by = "iso3c") %>%
  mutate(location_name = fct_reorder(location_name, base_exppc)) %>%
  mutate(location_name = fct_rev(location_name)) %>%
  filter (Year == 2021) %>%
  select (base_exppc, everything())

bottom_5_exppc_df <- perio_expenditure_countries %>%
  arrange(base_exppc) %>%
  filter (Year == 2021, base_exppc<1) %>%
  select (base_exppc, everything())


#------------

# To assess raw numbers for expenditure

perio_expenditure_wide <- read_csv("outputs_forecast/expenditure_summary_forecast_wide.csv") %>%
  rename(location_name = LocationHeader) %>%
  select(location_name, ends_with("2021") & contains ("total"), ends_with("2050") & contains ("total"), Region, Superregion, iso3c)
  # mutate(
  #   gap_WHO_mean = WHO_selected_Mean_total_billions_2050 - selected_Mean_total_billions_2021,
  #   gap_base_mean = selected_Mean_total_billions_2050 - selected_Mean_total_billions_2021,
  #   gap_WHO_mean_pct = 100*gap_WHO_mean/selected_Mean_total_billions_2021,
  #   gap_base_mean_pct = 100*gap_base_mean/selected_Mean_total_billions_2021
  # )

superregion_wide <- perio_expenditure_wide %>%
  filter(is.na(Region)) %>%
  select(-c(Region, Superregion, iso3c, WHO_selected_Mean_total_billions_2021, WHO_selected_SD_total_billions_2021)) %>%
  mutate(
    base_2021_upper = selected_Mean_total_billions_2021 + 1.96 * selected_SD_total_billions_2021,
    base_2021_lower = selected_Mean_total_billions_2021 - 1.96 * selected_SD_total_billions_2021,
    base_2050_upper = selected_Mean_total_billions_2050 + 1.96 * selected_SD_total_billions_2050,
    base_2050_lower = selected_Mean_total_billions_2050 - 1.96 * selected_SD_total_billions_2050,
    WHO_2050_upper = WHO_selected_Mean_total_billions_2050 + 1.96 * WHO_selected_SD_total_billions_2050,
    WHO_2050_lower = WHO_selected_Mean_total_billions_2050 - 1.96 * WHO_selected_SD_total_billions_2050) %>%
  mutate(
    across(where(is.numeric), round, 2),
    across(where(is.numeric), ~ ifelse(. < 0, 0, .)),
    "2021 Expenditure" = paste0(selected_Mean_total_billions_2021, " (", base_2021_lower, "-", base_2021_upper, ")"),
    "2050 Base Expenditure" = paste0(selected_Mean_total_billions_2050, " (", base_2050_lower, "-", base_2050_upper, ")"),
    "2050 WHO Expenditure" = paste0(WHO_selected_Mean_total_billions_2050, " (", WHO_2050_lower, "-", WHO_2050_upper, ")")
  ) %>%
  select(location_name, "2021 Expenditure":"2050 WHO Expenditure")



  
  
perio_expenditure_countries <- perio_expenditure_wide %>%
  filter (!is.na(iso3c)) %>%
  filter(!is.na(gap_WHO_mean))

median(perio_expenditure_countries$gap_WHO_mean)
median(perio_expenditure_countries$gap_WHO_mean_pct)
