# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# Individual year forecasts 2025-2030
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# 1. Forecast for 2025
# -----------------------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)
library(countrycode)
library(stringi)

prediction_high <- read_csv("outputs_2025/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country)

prediction_mid <- read_csv("outputs_2025/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country)

prediction_low <- read_csv("outputs_2025/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country)

other_predictors <- read_csv("outputs/predict_countries.csv") %>%
  dplyr::select(-Country_df2) %>%
  rename(Country = Country_df1)

# <<< NEW: Load the prior selection
previous_selection <- read_csv("outputs/final_selected_output.csv") %>%
  dplyr::select(Country, selected_model) %>%
  mutate(Country = ifelse(Country == "Micronesia",             # Micronesia is the only country that is not captured by the countrycodes package
                          "Micronesia (Federated States of)", 
                          Country))

# ------------------------------------------------------------------------
# 2. Joining all the tibbles together
# ------------------------------------------------------------------------

prediction_combined <- prediction_high %>%
  left_join(prediction_mid, by = "Country") %>%
  left_join(prediction_low, by = "Country") %>%
  left_join(other_predictors, by = "Country") %>%
  left_join(previous_selection, by = "Country") # <<< CHANGED

# ------------------------------------------------------------------------
# 3. Selection of scenario based on previous run
# ------------------------------------------------------------------------

prediction_selection <- prediction_combined %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when( # <<< REPLACED logic
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_perio_billions = case_when(
      selected_model == "high" ~ Mean_perio_billions_high,
      selected_model == "mid" ~ Mean_perio_billions_mid,
      selected_model == "low" ~ Mean_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_perio_billions = case_when(
      selected_model == "high" ~ SD_perio_billions_high,
      selected_model == "mid" ~ SD_perio_billions_mid,
      selected_model == "low" ~ SD_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_replace_billions = case_when(
      selected_model == "high" ~ Mean_replace_billions_high,
      selected_model == "mid" ~ Mean_replace_billions_mid,
      selected_model == "low" ~ Mean_replace_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_replace_billions = case_when(
      selected_model == "high" ~ SD_replace_billions_high,
      selected_model == "mid" ~ SD_replace_billions_mid,
      selected_model == "low" ~ SD_replace_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Add global totals row
global_row <- prediction_selection %>%
  summarise(
    Country = "Global",
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
    selected_model = NA_character_,
    Dent_exp_usd = NA_real_,
    Dent_exppc_usd = NA_real_,
    GDP_per_capita_PPP_2021 = NA_real_
  )

# Bind to main
prediction_selection <- prediction_selection %>%
  bind_rows(global_row)

prediction_selection_short <- prediction_selection %>%
  dplyr::select(
    Country,
    selected_Mean_total_billions,
    selected_SD_total_billions,
    selected_Mean_perio_billions,
    selected_SD_perio_billions,
    selected_Mean_replace_billions,
    selected_SD_replace_billions,
    selected_model,
    Dent_exp_usd,
    Dent_exppc_usd,
    GDP_per_capita_PPP_2021
  ) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

prediction_selection_short_joined <- hier %>%
  full_join(prediction_selection_short, by = "iso3c") %>%
  filter(!is.na(Country.y)) %>%
  mutate(Country = coalesce(Country.y, Country.x)) %>%
  select(-Country.x, -Country.y)
  
regions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Region) %>%
  summarise(
    Superregion = first(Superregion),
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

superregions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Superregion) %>%
  summarise(
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

prediction_selection_short_hier <- bind_rows(prediction_selection_short_joined, regions, superregions) %>%
  mutate(
    Level = case_when(
      Country == "Global" ~ 0,
      is.na(Country) & is.na(Region) ~ 1,
      is.na(Country) & !is.na(Region) ~ 2,
      TRUE ~ 3
    ),
    # Create Location Header with indentation
    LocationHeader = case_when(
      Level == 0 ~ paste(Country),               # no indent
      Level == 1 ~ paste0(Superregion),             # no indent
      Level == 2 ~ paste0("  ", Region),           # 2 spaces
      Level == 3 ~ paste0("    ", Country)         # 4 spaces
    )
  ) %>%
  select(LocationHeader, everything()) %>%
  arrange(desc(LocationHeader == "Global"), Superregion, !is.na(Region), Region, !is.na(Country))

write_csv(prediction_selection_short_hier, "outputs_2025/short_final_selected_output.csv")
write_csv(prediction_selection, "outputs_2025/final_selected_output.csv")


# ---------------------------------------------------------
# 4. Load all severity CSVs
# ---------------------------------------------------------
sev_high <- read_csv("outputs_2025/country_sev_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Severity))

sev_mid <- read_csv("outputs_2025/country_sev_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Severity))

sev_low <- read_csv("outputs_2025/country_sev_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Severity))

# ---------------------------------------------------------
# 5. Combine severity data
# ---------------------------------------------------------
sev_combined <- sev_high %>%
  left_join(sev_mid, by = c("Country", "Severity")) %>%
  left_join(sev_low, by = c("Country", "Severity")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 6. Load all procedure CSVs
# ---------------------------------------------------------
proc_high <- read_csv("outputs_2025/procedure_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))

proc_mid <- read_csv("outputs_2025/procedure_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))

proc_low <- read_csv("outputs_2025/procedure_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Procedure))

# ---------------------------------------------------------
# 7. Combine procedure data
# ---------------------------------------------------------
proc_combined <- proc_high %>%
  left_join(proc_mid, by = c("Country", "Procedure")) %>%
  left_join(proc_low, by = c("Country", "Procedure")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 8. Compute global severity summary
# ---------------------------------------------------------
global_severity <- sev_combined %>%
  group_by(Severity) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 9. Compute global procedure summary
# ---------------------------------------------------------
global_procedure <- proc_combined %>%
  group_by(Procedure) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 10. Write outputs
# ---------------------------------------------------------
write_csv(
  sev_combined %>% dplyr::select(Country, Severity, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2025/country_sev_selected.csv"
)

write_csv(
  proc_combined %>% dplyr::select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2025/procedure_selected.csv"
)

write_csv(global_severity, "outputs_2025/global_severity.csv")
write_csv(global_procedure, "outputs_2025/global_procedure.csv")




# -----------------------------------------------------------------------------------------
# 1. Forecast for 2030
# -----------------------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)

prediction_high <- read_csv("outputs_2030/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country)

prediction_mid <- read_csv("outputs_2030/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country)

prediction_low <- read_csv("outputs_2030/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country)

other_predictors <- read_csv("outputs/predict_countries.csv") %>%
  dplyr::select(-Country_df2) %>%
  rename(Country = Country_df1)

# <<< NEW: Load the prior selection
previous_selection <- read_csv("outputs/final_selected_output.csv") %>%
  dplyr::select(Country, selected_model) %>%
  mutate(Country = ifelse(Country == "Micronesia",             # Micronesia is the only country that is not captured by the countrycodes package
                          "Micronesia (Federated States of)", 
                          Country))

# ------------------------------------------------------------------------
# 2. Joining all the tibbles together
# ------------------------------------------------------------------------

prediction_combined <- prediction_high %>%
  left_join(prediction_mid, by = "Country") %>%
  left_join(prediction_low, by = "Country") %>%
  left_join(other_predictors, by = "Country") %>%
  left_join(previous_selection, by = "Country") # <<< CHANGED

# ------------------------------------------------------------------------
# 3. Selection of scenario based on previous run
# ------------------------------------------------------------------------

prediction_selection <- prediction_combined %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when( # <<< REPLACED logic
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_perio_billions = case_when(
      selected_model == "high" ~ Mean_perio_billions_high,
      selected_model == "mid" ~ Mean_perio_billions_mid,
      selected_model == "low" ~ Mean_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_perio_billions = case_when(
      selected_model == "high" ~ SD_perio_billions_high,
      selected_model == "mid" ~ SD_perio_billions_mid,
      selected_model == "low" ~ SD_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_replace_billions = case_when(
      selected_model == "high" ~ Mean_replace_billions_high,
      selected_model == "mid" ~ Mean_replace_billions_mid,
      selected_model == "low" ~ Mean_replace_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_replace_billions = case_when(
      selected_model == "high" ~ SD_replace_billions_high,
      selected_model == "mid" ~ SD_replace_billions_mid,
      selected_model == "low" ~ SD_replace_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Add global totals row
global_row <- prediction_selection %>%
  summarise(
    Country = "Global",
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
    selected_model = NA_character_,
    Dent_exp_usd = NA_real_,
    Dent_exppc_usd = NA_real_,
    GDP_per_capita_PPP_2021 = NA_real_
  )

# Bind to main
prediction_selection <- prediction_selection %>%
  bind_rows(global_row)

prediction_selection_short <- prediction_selection %>%
  dplyr::select(
    Country,
    selected_Mean_total_billions,
    selected_SD_total_billions,
    selected_Mean_perio_billions,
    selected_SD_perio_billions,
    selected_Mean_replace_billions,
    selected_SD_replace_billions,
    selected_model,
    Dent_exp_usd,
    Dent_exppc_usd,
    GDP_per_capita_PPP_2021
  ) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

prediction_selection_short_joined <- hier %>%
  full_join(prediction_selection_short, by = "iso3c") %>%
  filter(!is.na(Country.y)) %>%
  mutate(Country = coalesce(Country.y, Country.x)) %>%
  select(-Country.x, -Country.y)

regions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Region) %>%
  summarise(
    Superregion = first(Superregion),
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

superregions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Superregion) %>%
  summarise(
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

prediction_selection_short_hier <- bind_rows(prediction_selection_short_joined, regions, superregions) %>%
  mutate(
    Level = case_when(
      Country == "Global" ~ 0,
      is.na(Country) & is.na(Region) ~ 1,
      is.na(Country) & !is.na(Region) ~ 2,
      TRUE ~ 3
    ),
    # Create Location Header with indentation
    LocationHeader = case_when(
      Level == 0 ~ paste(Country),               # no indent
      Level == 1 ~ paste0(Superregion),             # no indent
      Level == 2 ~ paste0("  ", Region),           # 2 spaces
      Level == 3 ~ paste0("    ", Country)         # 4 spaces
    )
  ) %>%
  select(LocationHeader, everything()) %>%
  arrange(desc(LocationHeader == "Global"), Superregion, !is.na(Region), Region, !is.na(Country))

write_csv(prediction_selection_short_hier, "outputs_2030/short_final_selected_output.csv")
write_csv(prediction_selection, "outputs_2030/final_selected_output.csv")

# ---------------------------------------------------------
# 4. Load all severity CSVs
# ---------------------------------------------------------
sev_high <- read_csv("outputs_2030/country_sev_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Severity))

sev_mid <- read_csv("outputs_2030/country_sev_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Severity))

sev_low <- read_csv("outputs_2030/country_sev_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Severity))

# ---------------------------------------------------------
# 5. Combine severity data
# ---------------------------------------------------------
sev_combined <- sev_high %>%
  left_join(sev_mid, by = c("Country", "Severity")) %>%
  left_join(sev_low, by = c("Country", "Severity")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 6. Load all procedure CSVs
# ---------------------------------------------------------
proc_high <- read_csv("outputs_2030/procedure_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))

proc_mid <- read_csv("outputs_2030/procedure_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))

proc_low <- read_csv("outputs_2030/procedure_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Procedure))

# ---------------------------------------------------------
# 7. Combine procedure data
# ---------------------------------------------------------
proc_combined <- proc_high %>%
  left_join(proc_mid, by = c("Country", "Procedure")) %>%
  left_join(proc_low, by = c("Country", "Procedure")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 8. Compute global severity summary
# ---------------------------------------------------------
global_severity <- sev_combined %>%
  group_by(Severity) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 9. Compute global procedure summary
# ---------------------------------------------------------
global_procedure <- proc_combined %>%
  group_by(Procedure) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 10. Write outputs
# ---------------------------------------------------------
write_csv(
  sev_combined %>% dplyr::select(Country, Severity, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2030/country_sev_selected.csv"
)

write_csv(
  proc_combined %>% dplyr::select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2030/procedure_selected.csv"
)

write_csv(global_severity, "outputs_2030/global_severity.csv")
write_csv(global_procedure, "outputs_2030/global_procedure.csv")




# -----------------------------------------------------------------------------------------
# 1. Forecast for 2035
# -----------------------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)

prediction_high <- read_csv("outputs_2035/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country)

prediction_mid <- read_csv("outputs_2035/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country)

prediction_low <- read_csv("outputs_2035/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country)

other_predictors <- read_csv("outputs/predict_countries.csv") %>%
  dplyr::select(-Country_df2) %>%
  rename(Country = Country_df1)

# <<< NEW: Load the prior selection
previous_selection <- read_csv("outputs/final_selected_output.csv") %>%
  dplyr::select(Country, selected_model) %>%
  mutate(Country = ifelse(Country == "Micronesia",             # Micronesia is the only country that is not captured by the countrycodes package
                          "Micronesia (Federated States of)", 
                          Country))

# ------------------------------------------------------------------------
# 2. Joining all the tibbles together
# ------------------------------------------------------------------------

prediction_combined <- prediction_high %>%
  left_join(prediction_mid, by = "Country") %>%
  left_join(prediction_low, by = "Country") %>%
  left_join(other_predictors, by = "Country") %>%
  left_join(previous_selection, by = "Country") # <<< CHANGED

# ------------------------------------------------------------------------
# 3. Selection of scenario based on previous run
# ------------------------------------------------------------------------

prediction_selection <- prediction_combined %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when( # <<< REPLACED logic
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_perio_billions = case_when(
      selected_model == "high" ~ Mean_perio_billions_high,
      selected_model == "mid" ~ Mean_perio_billions_mid,
      selected_model == "low" ~ Mean_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_perio_billions = case_when(
      selected_model == "high" ~ SD_perio_billions_high,
      selected_model == "mid" ~ SD_perio_billions_mid,
      selected_model == "low" ~ SD_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_replace_billions = case_when(
      selected_model == "high" ~ Mean_replace_billions_high,
      selected_model == "mid" ~ Mean_replace_billions_mid,
      selected_model == "low" ~ Mean_replace_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_replace_billions = case_when(
      selected_model == "high" ~ SD_replace_billions_high,
      selected_model == "mid" ~ SD_replace_billions_mid,
      selected_model == "low" ~ SD_replace_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Add global totals row
global_row <- prediction_selection %>%
  summarise(
    Country = "Global",
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
    selected_model = NA_character_,
    Dent_exp_usd = NA_real_,
    Dent_exppc_usd = NA_real_,
    GDP_per_capita_PPP_2021 = NA_real_
  )

# Bind to main
prediction_selection <- prediction_selection %>%
  bind_rows(global_row)

prediction_selection_short <- prediction_selection %>%
  dplyr::select(
    Country,
    selected_Mean_total_billions,
    selected_SD_total_billions,
    selected_Mean_perio_billions,
    selected_SD_perio_billions,
    selected_Mean_replace_billions,
    selected_SD_replace_billions,
    selected_model,
    Dent_exp_usd,
    Dent_exppc_usd,
    GDP_per_capita_PPP_2021
  ) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

prediction_selection_short_joined <- hier %>%
  full_join(prediction_selection_short, by = "iso3c") %>%
  filter(!is.na(Country.y)) %>%
  mutate(Country = coalesce(Country.y, Country.x)) %>%
  select(-Country.x, -Country.y)

regions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Region) %>%
  summarise(
    Superregion = first(Superregion),
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

superregions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Superregion) %>%
  summarise(
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

prediction_selection_short_hier <- bind_rows(prediction_selection_short_joined, regions, superregions) %>%
  mutate(
    Level = case_when(
      Country == "Global" ~ 0,
      is.na(Country) & is.na(Region) ~ 1,
      is.na(Country) & !is.na(Region) ~ 2,
      TRUE ~ 3
    ),
    # Create Location Header with indentation
    LocationHeader = case_when(
      Level == 0 ~ paste(Country),               # no indent
      Level == 1 ~ paste0(Superregion),             # no indent
      Level == 2 ~ paste0("  ", Region),           # 2 spaces
      Level == 3 ~ paste0("    ", Country)         # 4 spaces
    )
  ) %>%
  select(LocationHeader, everything()) %>%
  arrange(desc(LocationHeader == "Global"), Superregion, !is.na(Region), Region, !is.na(Country))

write_csv(prediction_selection_short_hier, "outputs_2035/short_final_selected_output.csv")
write_csv(prediction_selection, "outputs_2035/final_selected_output.csv")
# ---------------------------------------------------------
# 4. Load all severity CSVs
# ---------------------------------------------------------
sev_high <- read_csv("outputs_2035/country_sev_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Severity))

sev_mid <- read_csv("outputs_2035/country_sev_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Severity))

sev_low <- read_csv("outputs_2035/country_sev_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Severity))

# ---------------------------------------------------------
# 5. Combine severity data
# ---------------------------------------------------------
sev_combined <- sev_high %>%
  left_join(sev_mid, by = c("Country", "Severity")) %>%
  left_join(sev_low, by = c("Country", "Severity")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 6. Load all procedure CSVs
# ---------------------------------------------------------
proc_high <- read_csv("outputs_2035/procedure_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))

proc_mid <- read_csv("outputs_2035/procedure_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))

proc_low <- read_csv("outputs_2035/procedure_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Procedure))

# ---------------------------------------------------------
# 7. Combine procedure data
# ---------------------------------------------------------
proc_combined <- proc_high %>%
  left_join(proc_mid, by = c("Country", "Procedure")) %>%
  left_join(proc_low, by = c("Country", "Procedure")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 8. Compute global severity summary
# ---------------------------------------------------------
global_severity <- sev_combined %>%
  group_by(Severity) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 9. Compute global procedure summary
# ---------------------------------------------------------
global_procedure <- proc_combined %>%
  group_by(Procedure) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 10. Write outputs
# ---------------------------------------------------------
write_csv(
  sev_combined %>% dplyr::select(Country, Severity, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2035/country_sev_selected.csv"
)

write_csv(
  proc_combined %>% dplyr::select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2035/procedure_selected.csv"
)

write_csv(global_severity, "outputs_2035/global_severity.csv")
write_csv(global_procedure, "outputs_2035/global_procedure.csv")




# -----------------------------------------------------------------------------------------
# 1. Forecast for 2040
# -----------------------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)

prediction_high <- read_csv("outputs_2040/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country)

prediction_mid <- read_csv("outputs_2040/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country)

prediction_low <- read_csv("outputs_2040/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country)

other_predictors <- read_csv("outputs/predict_countries.csv") %>%
  dplyr::select(-Country_df2) %>%
  rename(Country = Country_df1)

# <<< NEW: Load the prior selection
previous_selection <- read_csv("outputs/final_selected_output.csv") %>%
  dplyr::select(Country, selected_model) %>%
  mutate(Country = ifelse(Country == "Micronesia",             # Micronesia is the only country that is not captured by the countrycodes package
                          "Micronesia (Federated States of)", 
                          Country))

# ------------------------------------------------------------------------
# 2. Joining all the tibbles together
# ------------------------------------------------------------------------

prediction_combined <- prediction_high %>%
  left_join(prediction_mid, by = "Country") %>%
  left_join(prediction_low, by = "Country") %>%
  left_join(other_predictors, by = "Country") %>%
  left_join(previous_selection, by = "Country") # <<< CHANGED

# ------------------------------------------------------------------------
# 3. Selection of scenario based on previous run
# ------------------------------------------------------------------------

prediction_selection <- prediction_combined %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when( # <<< REPLACED logic
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_perio_billions = case_when(
      selected_model == "high" ~ Mean_perio_billions_high,
      selected_model == "mid" ~ Mean_perio_billions_mid,
      selected_model == "low" ~ Mean_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_perio_billions = case_when(
      selected_model == "high" ~ SD_perio_billions_high,
      selected_model == "mid" ~ SD_perio_billions_mid,
      selected_model == "low" ~ SD_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_replace_billions = case_when(
      selected_model == "high" ~ Mean_replace_billions_high,
      selected_model == "mid" ~ Mean_replace_billions_mid,
      selected_model == "low" ~ Mean_replace_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_replace_billions = case_when(
      selected_model == "high" ~ SD_replace_billions_high,
      selected_model == "mid" ~ SD_replace_billions_mid,
      selected_model == "low" ~ SD_replace_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Add global totals row
global_row <- prediction_selection %>%
  summarise(
    Country = "Global",
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
    selected_model = NA_character_,
    Dent_exp_usd = NA_real_,
    Dent_exppc_usd = NA_real_,
    GDP_per_capita_PPP_2021 = NA_real_
  )

# Bind to main
prediction_selection <- prediction_selection %>%
  bind_rows(global_row)

prediction_selection_short <- prediction_selection %>%
  dplyr::select(
    Country,
    selected_Mean_total_billions,
    selected_SD_total_billions,
    selected_Mean_perio_billions,
    selected_SD_perio_billions,
    selected_Mean_replace_billions,
    selected_SD_replace_billions,
    selected_model,
    Dent_exp_usd,
    Dent_exppc_usd,
    GDP_per_capita_PPP_2021
  ) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

prediction_selection_short_joined <- hier %>%
  full_join(prediction_selection_short, by = "iso3c") %>%
  filter(!is.na(Country.y)) %>%
  mutate(Country = coalesce(Country.y, Country.x)) %>%
  select(-Country.x, -Country.y)

regions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Region) %>%
  summarise(
    Superregion = first(Superregion),
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

superregions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Superregion) %>%
  summarise(
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

prediction_selection_short_hier <- bind_rows(prediction_selection_short_joined, regions, superregions) %>%
  mutate(
    Level = case_when(
      Country == "Global" ~ 0,
      is.na(Country) & is.na(Region) ~ 1,
      is.na(Country) & !is.na(Region) ~ 2,
      TRUE ~ 3
    ),
    # Create Location Header with indentation
    LocationHeader = case_when(
      Level == 0 ~ paste(Country),               # no indent
      Level == 1 ~ paste0(Superregion),             # no indent
      Level == 2 ~ paste0("  ", Region),           # 2 spaces
      Level == 3 ~ paste0("    ", Country)         # 4 spaces
    )
  ) %>%
  select(LocationHeader, everything()) %>%
  arrange(desc(LocationHeader == "Global"), Superregion, !is.na(Region), Region, !is.na(Country))

write_csv(prediction_selection_short_hier, "outputs_2040/short_final_selected_output.csv")
write_csv(prediction_selection, "outputs_2040/final_selected_output.csv")
# ---------------------------------------------------------
# 4. Load all severity CSVs
# ---------------------------------------------------------
sev_high <- read_csv("outputs_2040/country_sev_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Severity))

sev_mid <- read_csv("outputs_2040/country_sev_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Severity))

sev_low <- read_csv("outputs_2040/country_sev_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Severity))

# ---------------------------------------------------------
# 5. Combine severity data
# ---------------------------------------------------------
sev_combined <- sev_high %>%
  left_join(sev_mid, by = c("Country", "Severity")) %>%
  left_join(sev_low, by = c("Country", "Severity")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 6. Load all procedure CSVs
# ---------------------------------------------------------
proc_high <- read_csv("outputs_2040/procedure_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))

proc_mid <- read_csv("outputs_2040/procedure_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))

proc_low <- read_csv("outputs_2040/procedure_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Procedure))

# ---------------------------------------------------------
# 7. Combine procedure data
# ---------------------------------------------------------
proc_combined <- proc_high %>%
  left_join(proc_mid, by = c("Country", "Procedure")) %>%
  left_join(proc_low, by = c("Country", "Procedure")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 8. Compute global severity summary
# ---------------------------------------------------------
global_severity <- sev_combined %>%
  group_by(Severity) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 9. Compute global procedure summary
# ---------------------------------------------------------
global_procedure <- proc_combined %>%
  group_by(Procedure) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 10. Write outputs
# ---------------------------------------------------------
write_csv(
  sev_combined %>% dplyr::select(Country, Severity, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2040/country_sev_selected.csv"
)

write_csv(
  proc_combined %>% dplyr::select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2040/procedure_selected.csv"
)

write_csv(global_severity, "outputs_2040/global_severity.csv")
write_csv(global_procedure, "outputs_2040/global_procedure.csv")




# -----------------------------------------------------------------------------------------
# 1. Forecast for 2045
# -----------------------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)

prediction_high <- read_csv("outputs_2045/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country)

prediction_mid <- read_csv("outputs_2045/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country)

prediction_low <- read_csv("outputs_2045/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country)

other_predictors <- read_csv("outputs/predict_countries.csv") %>%
  dplyr::select(-Country_df2) %>%
  rename(Country = Country_df1)

# <<< NEW: Load the prior selection
previous_selection <- read_csv("outputs/final_selected_output.csv") %>%
  dplyr::select(Country, selected_model) %>%
  mutate(Country = ifelse(Country == "Micronesia",             # Micronesia is the only country that is not captured by the countrycodes package
                          "Micronesia (Federated States of)", 
                          Country))

# ------------------------------------------------------------------------
# 2. Joining all the tibbles together
# ------------------------------------------------------------------------

prediction_combined <- prediction_high %>%
  left_join(prediction_mid, by = "Country") %>%
  left_join(prediction_low, by = "Country") %>%
  left_join(other_predictors, by = "Country") %>%
  left_join(previous_selection, by = "Country") # <<< CHANGED

# ------------------------------------------------------------------------
# 3. Selection of scenario based on previous run
# ------------------------------------------------------------------------

prediction_selection <- prediction_combined %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when( # <<< REPLACED logic
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_perio_billions = case_when(
      selected_model == "high" ~ Mean_perio_billions_high,
      selected_model == "mid" ~ Mean_perio_billions_mid,
      selected_model == "low" ~ Mean_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_perio_billions = case_when(
      selected_model == "high" ~ SD_perio_billions_high,
      selected_model == "mid" ~ SD_perio_billions_mid,
      selected_model == "low" ~ SD_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_replace_billions = case_when(
      selected_model == "high" ~ Mean_replace_billions_high,
      selected_model == "mid" ~ Mean_replace_billions_mid,
      selected_model == "low" ~ Mean_replace_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_replace_billions = case_when(
      selected_model == "high" ~ SD_replace_billions_high,
      selected_model == "mid" ~ SD_replace_billions_mid,
      selected_model == "low" ~ SD_replace_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Add global totals row
global_row <- prediction_selection %>%
  summarise(
    Country = "Global",
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
    selected_model = NA_character_,
    Dent_exp_usd = NA_real_,
    Dent_exppc_usd = NA_real_,
    GDP_per_capita_PPP_2021 = NA_real_
  )

# Bind to main
prediction_selection <- prediction_selection %>%
  bind_rows(global_row)

prediction_selection_short <- prediction_selection %>%
  dplyr::select(
    Country,
    selected_Mean_total_billions,
    selected_SD_total_billions,
    selected_Mean_perio_billions,
    selected_SD_perio_billions,
    selected_Mean_replace_billions,
    selected_SD_replace_billions,
    selected_model,
    Dent_exp_usd,
    Dent_exppc_usd,
    GDP_per_capita_PPP_2021
  ) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

prediction_selection_short_joined <- hier %>%
  full_join(prediction_selection_short, by = "iso3c") %>%
  filter(!is.na(Country.y)) %>%
  mutate(Country = coalesce(Country.y, Country.x)) %>%
  select(-Country.x, -Country.y)

regions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Region) %>%
  summarise(
    Superregion = first(Superregion),
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

superregions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Superregion) %>%
  summarise(
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

prediction_selection_short_hier <- bind_rows(prediction_selection_short_joined, regions, superregions) %>%
  mutate(
    Level = case_when(
      Country == "Global" ~ 0,
      is.na(Country) & is.na(Region) ~ 1,
      is.na(Country) & !is.na(Region) ~ 2,
      TRUE ~ 3
    ),
    # Create Location Header with indentation
    LocationHeader = case_when(
      Level == 0 ~ paste(Country),               # no indent
      Level == 1 ~ paste0(Superregion),             # no indent
      Level == 2 ~ paste0("  ", Region),           # 2 spaces
      Level == 3 ~ paste0("    ", Country)         # 4 spaces
    )
  ) %>%
  select(LocationHeader, everything()) %>%
  arrange(desc(LocationHeader == "Global"), Superregion, !is.na(Region), Region, !is.na(Country))

write_csv(prediction_selection_short_hier, "outputs_2045/short_final_selected_output.csv")
write_csv(prediction_selection, "outputs_2045/final_selected_output.csv")

# ---------------------------------------------------------
# 4. Load all severity CSVs
# ---------------------------------------------------------
sev_high <- read_csv("outputs_2045/country_sev_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Severity))

sev_mid <- read_csv("outputs_2045/country_sev_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Severity))

sev_low <- read_csv("outputs_2045/country_sev_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Severity))

# ---------------------------------------------------------
# 5. Combine severity data
# ---------------------------------------------------------
sev_combined <- sev_high %>%
  left_join(sev_mid, by = c("Country", "Severity")) %>%
  left_join(sev_low, by = c("Country", "Severity")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 6. Load all procedure CSVs
# ---------------------------------------------------------
proc_high <- read_csv("outputs_2045/procedure_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))

proc_mid <- read_csv("outputs_2045/procedure_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))

proc_low <- read_csv("outputs_2045/procedure_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Procedure))

# ---------------------------------------------------------
# 7. Combine procedure data
# ---------------------------------------------------------
proc_combined <- proc_high %>%
  left_join(proc_mid, by = c("Country", "Procedure")) %>%
  left_join(proc_low, by = c("Country", "Procedure")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 8. Compute global severity summary
# ---------------------------------------------------------
global_severity <- sev_combined %>%
  group_by(Severity) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 9. Compute global procedure summary
# ---------------------------------------------------------
global_procedure <- proc_combined %>%
  group_by(Procedure) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 10. Write outputs
# ---------------------------------------------------------
write_csv(
  sev_combined %>% dplyr::select(Country, Severity, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2045/country_sev_selected.csv"
)

write_csv(
  proc_combined %>% dplyr::select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2045/procedure_selected.csv"
)

write_csv(global_severity, "outputs_2045/global_severity.csv")
write_csv(global_procedure, "outputs_2045/global_procedure.csv")




# -----------------------------------------------------------------------------------------
# 1. Forecast for 2050
# -----------------------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)

prediction_high <- read_csv("outputs_2050/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country)

prediction_mid <- read_csv("outputs_2050/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country)

prediction_low <- read_csv("outputs_2050/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country)

other_predictors <- read_csv("outputs/predict_countries.csv") %>%
  dplyr::select(-Country_df2) %>%
  rename(Country = Country_df1)

# <<< NEW: Load the prior selection
previous_selection <- read_csv("outputs/final_selected_output.csv") %>%
  dplyr::select(Country, selected_model) %>%
  mutate(Country = ifelse(Country == "Micronesia",             # Micronesia is the only country that is not captured by the countrycodes package
                          "Micronesia (Federated States of)", 
                          Country))

# ------------------------------------------------------------------------
# 2. Joining all the tibbles together
# ------------------------------------------------------------------------

prediction_combined <- prediction_high %>%
  left_join(prediction_mid, by = "Country") %>%
  left_join(prediction_low, by = "Country") %>%
  left_join(other_predictors, by = "Country") %>%
  left_join(previous_selection, by = "Country") # <<< CHANGED

# ------------------------------------------------------------------------
# 3. Selection of scenario based on previous run
# ------------------------------------------------------------------------

prediction_selection <- prediction_combined %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when( # <<< REPLACED logic
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_perio_billions = case_when(
      selected_model == "high" ~ Mean_perio_billions_high,
      selected_model == "mid" ~ Mean_perio_billions_mid,
      selected_model == "low" ~ Mean_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_perio_billions = case_when(
      selected_model == "high" ~ SD_perio_billions_high,
      selected_model == "mid" ~ SD_perio_billions_mid,
      selected_model == "low" ~ SD_perio_billions_low,
      TRUE ~ NA_real_
    ),
    selected_Mean_replace_billions = case_when(
      selected_model == "high" ~ Mean_replace_billions_high,
      selected_model == "mid" ~ Mean_replace_billions_mid,
      selected_model == "low" ~ Mean_replace_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_replace_billions = case_when(
      selected_model == "high" ~ SD_replace_billions_high,
      selected_model == "mid" ~ SD_replace_billions_mid,
      selected_model == "low" ~ SD_replace_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# Add global totals row
global_row <- prediction_selection %>%
  summarise(
    Country = "Global",
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
    selected_model = NA_character_,
    Dent_exp_usd = NA_real_,
    Dent_exppc_usd = NA_real_,
    GDP_per_capita_PPP_2021 = NA_real_
  )

# Bind to main
prediction_selection <- prediction_selection %>%
  bind_rows(global_row)

prediction_selection_short <- prediction_selection %>%
  dplyr::select(
    Country,
    selected_Mean_total_billions,
    selected_SD_total_billions,
    selected_Mean_perio_billions,
    selected_SD_perio_billions,
    selected_Mean_replace_billions,
    selected_SD_replace_billions,
    selected_model,
    Dent_exp_usd,
    Dent_exppc_usd,
    GDP_per_capita_PPP_2021
  ) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names

prediction_selection_short_joined <- hier %>%
  full_join(prediction_selection_short, by = "iso3c") %>%
  filter(!is.na(Country.y)) %>%
  mutate(Country = coalesce(Country.y, Country.x)) %>%
  select(-Country.x, -Country.y)

regions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Region) %>%
  summarise(
    Superregion = first(Superregion),
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

superregions <- prediction_selection_short_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Superregion) %>%
  summarise(
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
  )

prediction_selection_short_hier <- bind_rows(prediction_selection_short_joined, regions, superregions) %>%
  mutate(
    Level = case_when(
      Country == "Global" ~ 0,
      is.na(Country) & is.na(Region) ~ 1,
      is.na(Country) & !is.na(Region) ~ 2,
      TRUE ~ 3
    ),
    # Create Location Header with indentation
    LocationHeader = case_when(
      Level == 0 ~ paste(Country),               # no indent
      Level == 1 ~ paste0(Superregion),             # no indent
      Level == 2 ~ paste0("  ", Region),           # 2 spaces
      Level == 3 ~ paste0("    ", Country)         # 4 spaces
    )
  ) %>%
  select(LocationHeader, everything()) %>%
  arrange(desc(LocationHeader == "Global"), Superregion, !is.na(Region), Region, !is.na(Country))

write_csv(prediction_selection_short_hier, "outputs_2050/short_final_selected_output.csv")
write_csv(prediction_selection, "outputs_2050/final_selected_output.csv")

# ---------------------------------------------------------
# 4. Load all severity CSVs
# ---------------------------------------------------------
sev_high <- read_csv("outputs_2050/country_sev_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Severity))

sev_mid <- read_csv("outputs_2050/country_sev_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Severity))

sev_low <- read_csv("outputs_2050/country_sev_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Severity))

# ---------------------------------------------------------
# 5. Combine severity data
# ---------------------------------------------------------
sev_combined <- sev_high %>%
  left_join(sev_mid, by = c("Country", "Severity")) %>%
  left_join(sev_low, by = c("Country", "Severity")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 6. Load all procedure CSVs
# ---------------------------------------------------------
proc_high <- read_csv("outputs_2050/procedure_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))

proc_mid <- read_csv("outputs_2050/procedure_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))

proc_low <- read_csv("outputs_2050/procedure_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Procedure))

# ---------------------------------------------------------
# 7. Combine procedure data
# ---------------------------------------------------------
proc_combined <- proc_high %>%
  left_join(proc_mid, by = c("Country", "Procedure")) %>%
  left_join(proc_low, by = c("Country", "Procedure")) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
  rowwise() %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid" ~ Mean_total_billions_mid,
      selected_model == "low" ~ Mean_total_billions_low,
      TRUE ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_low,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 8. Compute global severity summary
# ---------------------------------------------------------
global_severity <- sev_combined %>%
  group_by(Severity) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 9. Compute global procedure summary
# ---------------------------------------------------------
global_procedure <- proc_combined %>%
  group_by(Procedure) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 10. Write outputs
# ---------------------------------------------------------
write_csv(
  sev_combined %>% dplyr::select(Country, Severity, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2050/country_sev_selected.csv"
)

write_csv(
  proc_combined %>% dplyr::select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs_2050/procedure_selected.csv"
)

write_csv(global_severity, "outputs_2050/global_severity.csv")
write_csv(global_procedure, "outputs_2050/global_procedure.csv")





# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# 1. Bind all rows for forecast for perio expenditure/population charts
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. For short selected output (including GBD regions, superregions and countries)
# ------------------------------------------------------------------------

library(tidyverse)

summary_2021 <- read_csv("outputs/short_final_selected_output.csv") %>%
  mutate(Year = 2021)
summary_2025 <- read_csv("outputs_2025/short_final_selected_output.csv") %>%
  mutate(Year = 2025)
summary_2030 <- read_csv("outputs_2030/short_final_selected_output.csv") %>%
  mutate(Year = 2030)
summary_2035 <- read_csv("outputs_2035/short_final_selected_output.csv") %>%
  mutate(Year = 2035)
summary_2040 <- read_csv("outputs_2040/short_final_selected_output.csv") %>%
  mutate(Year = 2040)
summary_2045 <- read_csv("outputs_2045/short_final_selected_output.csv") %>%
  mutate(Year = 2045)
summary_2050 <- read_csv("outputs_2050/short_final_selected_output.csv") %>%
  mutate(Year = 2050)

full_forecast <- bind_rows(summary_2021, summary_2025, summary_2030, summary_2035, summary_2040,
                           summary_2045, summary_2050)

full_forecast_wide <- full_forecast %>%
  pivot_wider (
    names_from = Year,
    values_from = c(selected_Mean_total_billions, selected_SD_total_billions,
                     selected_Mean_perio_billions, selected_SD_perio_billions,
                     selected_Mean_replace_billions, selected_SD_replace_billions)
) %>%
  relocate(starts_with("selected"), .after = Superregion)

write_csv(full_forecast, "outputs_forecast/expenditure_summary_forecast.csv")
write_csv(full_forecast_wide, "outputs_forecast/expenditure_summary_forecast_wide.csv")


# ------------------------------------------------------------------------
# 2. For assembling full output including various dental utilisation scenarios
# ------------------------------------------------------------------------

library(tidyverse)

long_summary_2021 <- read_csv("outputs/final_selected_output.csv") %>%
  mutate(Year = 2021)
long_summary_2025 <- read_csv("outputs_2025/final_selected_output.csv") %>%
  mutate(Year = 2025)
long_summary_2030 <- read_csv("outputs_2030/final_selected_output.csv") %>%
  mutate(Year = 2030)
long_summary_2035 <- read_csv("outputs_2035/final_selected_output.csv") %>%
  mutate(Year = 2035)
long_summary_2040 <- read_csv("outputs_2040/final_selected_output.csv") %>%
  mutate(Year = 2040)
long_summary_2045 <- read_csv("outputs_2045/final_selected_output.csv") %>%
  mutate(Year = 2045)
long_summary_2050 <- read_csv("outputs_2050/final_selected_output.csv") %>%
  mutate(Year = 2050)

long_full_forecast <- bind_rows(long_summary_2021, long_summary_2025, long_summary_2030, long_summary_2035, long_summary_2040,
                           long_summary_2045, long_summary_2050)

long_full_forecast_wide <- full_forecast %>%
  pivot_wider (
    names_from = Year,
    values_from = c(selected_Mean_total_billions, selected_SD_total_billions,
                    selected_Mean_perio_billions, selected_SD_perio_billions,
                    selected_Mean_replace_billions, selected_SD_replace_billions)
  ) %>%
  relocate(starts_with("selected"), .after = Superregion)

 write_csv(full_forecast, "outputs_forecast/base_scenario.csv")
# write_csv(full_forecast_wide, "outputs_forecast/long_expenditure_summary_forecast_wide.csv")

# ------------------------------------------------------------------------
# 2. For assembling full output including various dental utilisation scenarios
# ------------------------------------------------------------------------

mid_scenario <- long_full_forecast %>%
  filter(Year > 2025) %>%
  mutate(
    selected_Mean_total_billions = case_when(
      selected_model == "high" ~ Mean_total_billions_high,
      selected_model == "mid"  ~ Mean_total_billions_mid,
      selected_model == "low"  ~ Mean_total_billions_mid,
      TRUE                     ~ NA_real_
    ),
    selected_SD_total_billions = case_when(
      selected_model == "high" ~ SD_total_billions_high,
      selected_model == "mid" ~ SD_total_billions_mid,
      selected_model == "low" ~ SD_total_billions_mid,
      TRUE ~ NA_real_
    ),
    selected_Mean_perio_billions = case_when(
      selected_model == "high" ~ Mean_perio_billions_high,
      selected_model == "mid" ~ Mean_perio_billions_mid,
      selected_model == "low" ~ Mean_perio_billions_mid,
      TRUE ~ NA_real_
    ),
    selected_SD_perio_billions = case_when(
      selected_model == "high" ~ SD_perio_billions_high,
      selected_model == "mid" ~ SD_perio_billions_mid,
      selected_model == "low" ~ SD_perio_billions_mid,
      TRUE ~ NA_real_
    ),
    selected_Mean_replace_billions = case_when(
      selected_model == "high" ~ Mean_replace_billions_high,
      selected_model == "mid" ~ Mean_replace_billions_mid,
      selected_model == "low" ~ Mean_replace_billions_mid,
      TRUE ~ NA_real_
    ),
    selected_SD_replace_billions = case_when(
      selected_model == "high" ~ SD_replace_billions_high,
      selected_model == "mid" ~ SD_replace_billions_mid,
      selected_model == "low" ~ SD_replace_billions_mid,
      TRUE ~ NA_real_
    )
  ) %>%
  bind_rows(long_summary_2021, long_summary_2025) %>%
  filter(!Country == "Global") 

global_mid_scenario <- mid_scenario%>%
  group_by(Year) %>%
  summarise(
    Country = "Global",
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
    selected_model = NA_character_,
    Dent_exp_usd = NA_real_,
    Dent_exppc_usd = NA_real_,
    GDP_per_capita_PPP_2021 = NA_real_
  )

mid_scenario_full <- mid_scenario %>%
  bind_rows(global_mid_scenario)

write_csv(mid_scenario_full, "outputs_forecast/mid_scenario.csv")

high_scenario <- long_full_forecast %>%
  filter(Year>2025) %>%
  mutate(
    selected_Mean_total_billions = Mean_total_billions_high,
    selected_SD_total_billions = SD_total_billions_high,
    selected_Mean_perio_billions = Mean_perio_billions_high,
    selected_SD_perio_billions = SD_perio_billions_high,
    selected_Mean_replace_billions = Mean_replace_billions_high,
    selected_SD_replace_billions = SD_replace_billions_high
  ) %>%
  bind_rows(long_summary_2021, long_summary_2025) %>%
  filter(!Country == "Global") 

global_high_scenario <- high_scenario%>%
  group_by(Year) %>%
  summarise(
    Country = "Global",
    selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
    selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
    selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
    selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE)),
    selected_model = NA_character_,
    Dent_exp_usd = NA_real_,
    Dent_exppc_usd = NA_real_,
    GDP_per_capita_PPP_2021 = NA_real_
  )

high_scenario_full <- high_scenario %>%
  bind_rows(global_high_scenario)

write_csv(high_scenario_full, "outputs_forecast/high_scenario.csv")
