# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)
library(countrycode)

prediction_high <- read_csv("outputs/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country) # to better identify the source

prediction_mid <- read_csv("outputs/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country) # to better identify the source

prediction_low <- read_csv("outputs/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country) # to better identify the source

prediction_WHO_target <- read_csv("outputs/country_combined_WHO_target.csv") %>%
  rename_with(~ paste0(.x, "_WHO_target"), -Country) # to better identify the source

other_predictors <- read_csv("data/2021_prevalence_pop_dentexp_GDP.csv")


# ------------------------------------------------------------------------
# 2. Joining all the tibbles together
# ------------------------------------------------------------------------

prediction_list <- list(
  prediction_high,
  prediction_mid,
  prediction_low,
  prediction_WHO_target,
  other_predictors
)

prediction_combined <- reduce(
  prediction_list,
  left_join,
  by = "Country"
) %>%
  mutate(
    Country = ifelse(
      Country == "Micronesia",
      "Micronesia (Federated States of)",
      Country
    )
  )

# ------------------------------------------------------------------------
# 3. Selection of model based on known dental expenditure
# ------------------------------------------------------------------------

# Define base variable names
base_vars <- c(
  "Mean_total_billions", "SD_total_billions",
  "Mean_perio_billions", "SD_perio_billions",
  "Mean_replace_billions", "SD_replace_billions"
)

prediction_selection <- prediction_combined %>%
  # First, pick the selected model based on your logic
  mutate(
    selected_model = case_when(
      Mean_total_billions_high < 0.75 * Dent_exp_usd ~ "high",
      Mean_total_billions_mid < 0.75 * Dent_exp_usd ~ "mid",
      TRUE ~ "low"
    )
  ) %>%
  mutate(
    Mean_total_billions = NA_real_,
    Mean_perio_billions = NA_real_,
    Mean_replace_billions = NA_real_,
    SD_total_billions = NA_real_,
    SD_perio_billions = NA_real_,
    SD_replace_billions = NA_real_
  ) %>%
  # Then, for each base variable, create the "selected_" version
  mutate(across(
    all_of(base_vars),
    ~ case_when(
      selected_model == "high" ~ get(paste0(cur_column(), "_high")),
      selected_model == "mid" ~ get(paste0(cur_column(), "_mid")),
      selected_model == "low" ~ get(paste0(cur_column(), "_low"))
    ),
    .names = "selected_{.col}"
  )) %>%
  mutate(across(
    all_of(base_vars),
    ~ case_when(
      selected_model == "high" ~ get(paste0(cur_column(), "_high")),
      selected_model == "mid" ~ get(paste0(cur_column(), "_WHO_target")),
      selected_model == "low" ~ get(paste0(cur_column(), "_WHO_target"))
    ),
    .names = "WHO_selected_{.col}"
  )) %>%
  select(-(Mean_total_billions:SD_replace_billions))

global_row <- prediction_selection %>%
  summarise(
    Country = "Global",
    across(contains("selected_Mean"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
    across(contains("selected_SD"), ~ sqrt(sum(.x^2, na.rm = TRUE)), .names = "{.col}")
  )

prediction_selection <- prediction_selection %>%
  bind_rows(global_row) %>%
  select(
    Country,
    selected_model:WHO_selected_SD_replace_billions,
    everything()
  ) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% # read GBD hierarchy
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) # matching with ISO code for country names

prediction_selection_joined <- hier %>%
  full_join(prediction_selection, by = "iso3c") %>%
  filter(!is.na(Country.y)) %>%
  mutate(Country = coalesce(Country.y, Country.x)) %>%
  select(-Country.x, -Country.y)

regions <- prediction_selection_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Region) %>%
  summarise(
    Superregion = first(Superregion),
    across(contains("selected_Mean"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
    across(contains("selected_SD"), ~ sqrt(sum(.x^2, na.rm = TRUE)), .names = "{.col}")
  )

superregions <- prediction_selection_joined %>%
  filter(!is.na(Superregion)) %>%
  group_by(Superregion) %>%
  summarise(
    across(contains("selected_Mean"), ~ sum(.x, na.rm = TRUE), .names = "{.col}"),
    across(contains("selected_SD"), ~ sqrt(sum(.x^2, na.rm = TRUE)), .names = "{.col}")
  )

prediction_selection_hier <- bind_rows(prediction_selection_joined, regions, superregions) %>%
  mutate(
    Level = case_when(
      Country == "Global" ~ 0,
      is.na(Country) & is.na(Region) ~ 1,
      is.na(Country) & !is.na(Region) ~ 2,
      TRUE ~ 3
    ),
    # Create Location Header with indentation
    LocationHeader = case_when(
      Level == 0 ~ paste(Country), # no indent
      Level == 1 ~ paste0(Superregion), # no indent
      Level == 2 ~ paste0("  ", Region), # 2 spaces
      Level == 3 ~ paste0("    ", Country) # 4 spaces
    )
  ) %>%
  select(LocationHeader, everything()) %>%
  arrange(desc(LocationHeader == "Global"), Superregion, !is.na(Region), Region, !is.na(Country))

write_csv(prediction_selection_hier, "outputs/short_final_selected_output.csv")
write_csv(prediction_selection, "outputs/final_selected_output.csv")

# ---------------------------------------------------------
# 4. Load all severity CSVs
# ---------------------------------------------------------
# sev_high <- read_csv("outputs/country_sev_combined_high.csv") %>%
#   rename_with(~ paste0(.x, "_high"), -c(Country, Severity))
#
# sev_mid <- read_csv("outputs/country_sev_combined_mid.csv") %>%
#   rename_with(~ paste0(.x, "_mid"), -c(Country, Severity))
#
# sev_low <- read_csv("outputs/country_sev_combined_low.csv") %>%
#   rename_with(~ paste0(.x, "_low"), -c(Country, Severity))

# ---------------------------------------------------------
# 5. Combine severity data
# ---------------------------------------------------------
# sev_combined <- sev_high %>% # TRY PURRR::REDUCE()
#   left_join(sev_mid, by = c("Country", "Severity")) %>%
#   left_join(sev_low, by = c("Country", "Severity")) %>%
#   left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country") %>%
#   rowwise() %>%
#   mutate(
#     selected_Mean_total_billions = case_when(
#       selected_model == "high" ~ Mean_total_billions_high,
#       selected_model == "mid" ~ Mean_total_billions_mid,
#       selected_model == "low" ~ Mean_total_billions_low,
#       TRUE ~ NA_real_
#     ),
#     selected_SD_total_billions = case_when(
#       selected_model == "high" ~ SD_total_billions_high,
#       selected_model == "mid" ~ SD_total_billions_mid,
#       selected_model == "low" ~ SD_total_billions_low,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   ungroup()

# ---------------------------------------------------------
# 6. Load all procedure CSVs
# ---------------------------------------------------------
proc_high <- read_csv("outputs/procedure_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))

proc_mid <- read_csv("outputs/procedure_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))

proc_low <- read_csv("outputs/procedure_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -c(Country, Procedure))

proc_WHO_target <- read_csv("outputs/procedure_combined_WHO_target.csv") %>%
  rename_with(~ paste0(.x, "_WHO_target"), -c(Country, Procedure))

# ---------------------------------------------------------
# 7. Combine procedure data
# ---------------------------------------------------------

selected_model <- prediction_selection %>% select(Country, selected_model)

proc_list <- list(
  proc_high, proc_mid, proc_low, proc_WHO_target
)

prediction_combined <- reduce(
  proc_list,
  left_join,
  by = c("Country", "Procedure")
) %>%
  mutate(
    Country = ifelse(
      Country == "Micronesia",
      "Micronesia (Federated States of)",
      Country
    )
  ) %>%
  mutate(
    Mean_total_billions = NA_real_,
    SD_total_billions   = NA_real_
  ) %>%
  left_join(prediction_selection %>% dplyr::select(Country, selected_model), by = "Country")

base_vars <- c("Mean_total_billions", "SD_total_billions")

proc_combined <- prediction_combined %>%
  mutate(across(
    all_of(base_vars),
    ~ case_when(
      selected_model == "high" ~ get(paste0(cur_column(), "_high")),
      selected_model == "mid" ~ get(paste0(cur_column(), "_mid")),
      selected_model == "low" ~ get(paste0(cur_column(), "_low"))
    ),
    .names = "selected_{.col}"
  )) %>%
  mutate(across(
    all_of(base_vars),
    ~ case_when(
      selected_model == "high" ~ get(paste0(cur_column(), "_high")),
      selected_model == "mid" ~ get(paste0(cur_column(), "_WHO_target")),
      selected_model == "low" ~ get(paste0(cur_column(), "_WHO_target"))
    ),
    .names = "WHO_selected_{.col}"
  )) %>%
  select(-Mean_total_billions, -SD_total_billions)


# ---------------------------------------------------------
# 8. Compute global severity summary
# ---------------------------------------------------------
# global_severity <- sev_combined %>%
#   group_by(Severity) %>%
#   summarise(
#     global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
#     global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
#     .groups = "drop"
#   )

# ---------------------------------------------------------
# 9. Compute global procedure summary
# ---------------------------------------------------------
global_procedure <- proc_combined %>%
  group_by(Procedure) %>%
  summarise(
    global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
    global_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
    global_WHO_Mean_total_billions = sum(WHO_selected_Mean_total_billions, na.rm = TRUE),
    global_WHO_SD_total_billions = sqrt(sum(WHO_selected_SD_total_billions^2, na.rm = TRUE)),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 10. Write outputs
# ---------------------------------------------------------
# write_csv(
#   sev_combined %>% dplyr::select(Country, Severity, selected_Mean_total_billions, selected_SD_total_billions),
#   "outputs/country_sev_selected.csv"
# )
#
write_csv(
  proc_combined %>% dplyr::select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs/procedure_selected.csv"
)
#
# write_csv(global_severity, "outputs/global_severity.csv")
write_csv(global_procedure, "outputs/global_procedure.csv")
