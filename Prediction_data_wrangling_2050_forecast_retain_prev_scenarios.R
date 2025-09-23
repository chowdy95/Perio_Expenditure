# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# Individual year forecasts 2025-2030
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)
library(countrycode)

process_year <- function(year) {
  
  input_dir  <- paste0("outputs_", year, "/")
  output_dir <- paste0("outputs_", year, "/")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # ------------------------------------------------------------------------
  # 1. Load Packages and Data
  # ------------------------------------------------------------------------
  
  prediction_high <- read_csv(paste0(input_dir, "country_combined_high.csv")) %>%
    rename_with(~ paste0(.x, "_high"), -Country)
  
  prediction_mid <- read_csv(paste0(input_dir, "country_combined_mid.csv")) %>%
    rename_with(~ paste0(.x, "_mid"), -Country)
  
  prediction_low <- read_csv(paste0(input_dir, "country_combined_low.csv")) %>%
    rename_with(~ paste0(.x, "_low"), -Country)
  
  prediction_WHO_target <- read_csv(paste0(input_dir, "country_combined_WHO_target.csv")) %>%
    rename_with(~ paste0(.x, "_WHO_target"), -Country)
  
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
    mutate(Country = ifelse(Country == "Micronesia",
                            "Micronesia (Federated States of)", Country))
  
  # ------------------------------------------------------------------------
  # 3. Model selection
  # ------------------------------------------------------------------------
  
  base_vars <- c("Mean_total_billions", "SD_total_billions",
                 "Mean_perio_billions", "SD_perio_billions",
                 "Mean_replace_billions", "SD_replace_billions")
  
  prediction_selection <- prediction_combined %>%
    mutate(
      selected_model = case_when(
        Mean_total_billions_high < 0.6 * Dent_exp_usd ~ "high",
        Mean_total_billions_mid  < 0.6 * Dent_exp_usd ~ "mid",
        TRUE ~ "low"
      )
    ) %>%
    mutate(
      Mean_total_billions = NA_real_,
      Mean_perio_billions  = NA_real_,
      Mean_replace_billions  = NA_real_,
      SD_total_billions   = NA_real_,
      SD_perio_billions    = NA_real_,
      SD_replace_billions    = NA_real_
    ) %>%
    mutate(across(
      all_of(base_vars),
      ~ case_when(
        selected_model == "high" ~ get(paste0(cur_column(), "_high")),
        selected_model == "mid"  ~ get(paste0(cur_column(), "_mid")),
        selected_model == "low"  ~ get(paste0(cur_column(), "_low"))
      ),
      .names = "selected_{.col}"
    )) %>%
    mutate(across(
      all_of(base_vars),
      ~ case_when(
        selected_model == "high" ~ get(paste0(cur_column(), "_high")),
        selected_model == "mid"  ~ get(paste0(cur_column(), "_WHO_target")),
        selected_model == "low"  ~ get(paste0(cur_column(), "_WHO_target"))
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
    mutate(iso3c = countrycode(Country, "country.name", "iso3c"))
  
  hier <- read_csv("data/GBD_location_hierarchy_wide.csv", 
                   locale = locale(encoding = "Latin1")) %>%
    mutate(iso3c = countrycode(Country, "country.name", "iso3c"))
  
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
      LocationHeader = case_when(
        Level == 0 ~ Country,
        Level == 1 ~ Superregion,
        Level == 2 ~ paste0("  ", Region),
        Level == 3 ~ paste0("    ", Country)
      )
    ) %>%
    select(LocationHeader, everything())
  
  write_csv(prediction_selection_hier, paste0(output_dir, "short_final_selected_output.csv"))
  write_csv(prediction_selection,       paste0(output_dir, "final_selected_output.csv"))
  
  # ------------------------------------------------------------------------
  # 6–9. Procedures
  # ------------------------------------------------------------------------
  
  proc_high <- read_csv(paste0(input_dir, "procedure_combined_high.csv")) %>%
    rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))
  
  proc_mid <- read_csv(paste0(input_dir, "procedure_combined_mid.csv")) %>%
    rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))
  
  proc_low <- read_csv(paste0(input_dir, "procedure_combined_low.csv")) %>%
    rename_with(~ paste0(.x, "_low"), -c(Country, Procedure))
  
  proc_WHO_target <- read_csv(paste0(input_dir, "procedure_combined_WHO_target.csv")) %>%
    rename_with(~ paste0(.x, "_WHO_target"), -c(Country, Procedure))
  
  proc_list <- list(proc_high, proc_mid, proc_low, proc_WHO_target)
  
  prediction_combined <- reduce(proc_list, left_join, by = c("Country", "Procedure")) %>%
    mutate(Country = ifelse(Country == "Micronesia",
                            "Micronesia (Federated States of)", Country)) %>%
    mutate(
      Mean_total_billions = NA_real_,
      SD_total_billions   = NA_real_
    ) %>%
    left_join(prediction_selection %>% select(Country, selected_model), by = "Country")
  
  base_vars <- c("Mean_total_billions", "SD_total_billions")
  
  proc_combined <- prediction_combined %>%
    mutate(across(
      all_of(base_vars),
      ~ case_when(
        selected_model == "high" ~ get(paste0(cur_column(), "_high")),
        selected_model == "mid"  ~ get(paste0(cur_column(), "_mid")),
        selected_model == "low"  ~ get(paste0(cur_column(), "_low"))
      ),
      .names = "selected_{.col}"
    )) %>%
    mutate(across(
      all_of(base_vars),
      ~ case_when(
        selected_model == "high" ~ get(paste0(cur_column(), "_high")),
        selected_model %in% c("mid","low") ~ get(paste0(cur_column(), "_WHO_target"))
      ),
      .names = "WHO_selected_{.col}"
    )) %>%
    select(-Mean_total_billions, -SD_total_billions)
  
  global_procedure <- proc_combined %>%
    group_by(Procedure) %>%
    summarise(
      global_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
      global_SD_total_billions   = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
      global_WHO_Mean_total_billions = sum(WHO_selected_Mean_total_billions, na.rm = TRUE),
      global_WHO_SD_total_billions   = sqrt(sum(WHO_selected_SD_total_billions^2, na.rm = TRUE)),
      .groups = "drop"
    )
  
  write_csv(proc_combined %>% select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
            paste0(output_dir, "procedure_selected.csv"))
  
  write_csv(global_procedure, paste0(output_dir, "global_procedure.csv"))
}

# ------------------------------------------------------------------------
# Run for all years
# ------------------------------------------------------------------------
years <- seq(2025, 2050, by = 5)
walk(years, process_year)









# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# 1. Bind all rows for forecast for perio expenditure/population charts
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------
# 1. For short selected output (including GBD regions, superregions and countries)
# ------------------------------------------------------------------------

library(tidyverse)

# Define years
years <- c(2021, seq(2025, 2050, by = 5))

# Read all files + add Year column
full_forecast <- map_dfr(years, function(y) {
  path <- if (y == 2021) {
    "outputs/short_final_selected_output.csv"
  } else {
    paste0("outputs_", y, "/short_final_selected_output.csv")
  }
  read_csv(path) %>% mutate(Year = y)
})

# Pivot wide
full_forecast_wide <- full_forecast %>%
  pivot_wider(
    names_from = Year,
    values_from = c(
      selected_Mean_total_billions, selected_SD_total_billions,
      selected_Mean_perio_billions, selected_SD_perio_billions,
      selected_Mean_replace_billions, selected_SD_replace_billions
    )
  ) %>%
  relocate(starts_with("selected"), .after = Superregion)

# Save outputs
if (!dir.exists("outputs_forecast")) dir.create("outputs_forecast", recursive = TRUE)

write_csv(full_forecast,       "outputs_forecast/expenditure_summary_forecast.csv")
write_csv(full_forecast_wide,  "outputs_forecast/expenditure_summary_forecast_wide.csv")


full_forecast <- bind_rows(summary_2021, summary_2025, summary_2030, summary_2035, summary_2040,
                           summary_2045, summary_2050)

full_forecast_wide <- full_forecast %>%
  pivot_wider (
    names_from = Year,
    values_from = c(selected_Mean_total_billions:WHO_selected_SD_replace_billions)
  ) %>%
  relocate(starts_with("selected"), .after = Superregion)

write_csv(full_forecast, "outputs_forecast/expenditure_summary_forecast.csv")
write_csv(full_forecast_wide, "outputs_forecast/expenditure_summary_forecast_wide.csv")


# # ------------------------------------------------------------------------
# # 2. For assembling full output including various dental utilisation scenarios
# # ------------------------------------------------------------------------
# 
# library(tidyverse)
# library(countrycode)
# 
# long_summary_2021 <- read_csv("outputs/final_selected_output.csv") %>%
#   mutate(Year = 2021)
# long_summary_2025 <- read_csv("outputs_2025/final_selected_output.csv") %>%
#   mutate(Year = 2025)
# long_summary_2030 <- read_csv("outputs_2030/final_selected_output.csv") %>%
#   mutate(Year = 2030)
# long_summary_2035 <- read_csv("outputs_2035/final_selected_output.csv") %>%
#   mutate(Year = 2035)
# long_summary_2040 <- read_csv("outputs_2040/final_selected_output.csv") %>%
#   mutate(Year = 2040)
# long_summary_2045 <- read_csv("outputs_2045/final_selected_output.csv") %>%
#   mutate(Year = 2045)
# long_summary_2050 <- read_csv("outputs_2050/final_selected_output.csv") %>%
#   mutate(Year = 2050)
# 
# long_full_forecast <- bind_rows(long_summary_2021, long_summary_2025, long_summary_2030, long_summary_2035, long_summary_2040,
#                            long_summary_2045, long_summary_2050)
# 
# base_scenario <- long_full_forecast %>%
#   filter(!Country == "Global") %>%
#   mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
#   mutate (Level = 3)
# 
# hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
#   mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names
# 
# base_scenario_joined <- hier %>%
#   full_join(base_scenario, by = "iso3c") %>%
#   filter(!is.na(Country.y)) %>%
#   mutate(Country = coalesce(Country.y, Country.x)) %>%
#   select(-Country.x, -Country.y)
# 
# global_base_scenario <- base_scenario%>%
#   group_by(Year) %>%
#   summarise(
#     Country = "Global",
#     selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
#     selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
#     selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
#     selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
#     selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
#     selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE))
#   ) %>%
#   mutate(Level = 0)
# 
# superregion_base_scenario <- base_scenario_joined%>%
#   group_by(Year,Superregion) %>%
#   summarise(
#     selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
#     selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
#     selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
#     selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
#     selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
#     selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE))
#   ) %>%
#   mutate(Level = 1) %>%
#   mutate(Country = Superregion)
# 
# base_scenario_full <- base_scenario_joined %>%
#   bind_rows(global_base_scenario, superregion_base_scenario)
# 
# write_csv(base_scenario_full, "outputs_forecast/base_scenario.csv")
#  
# 
# # ------------------------------------------------------------------------
# # 2. For assembling full output including various dental utilisation scenarios
# # ------------------------------------------------------------------------
# 
# mid_scenario <- long_full_forecast %>%
#   filter(Year > 2025) %>%
#   mutate(
#     selected_Mean_total_billions = case_when(
#       selected_model == "high" ~ Mean_total_billions_high,
#       selected_model == "mid"  ~ Mean_total_billions_mid,
#       selected_model == "low"  ~ Mean_total_billions_mid,
#       TRUE                     ~ NA_real_
#     ),
#     selected_SD_total_billions = case_when(
#       selected_model == "high" ~ SD_total_billions_high,
#       selected_model == "mid" ~ SD_total_billions_mid,
#       selected_model == "low" ~ SD_total_billions_mid,
#       TRUE ~ NA_real_
#     ),
#     selected_Mean_perio_billions = case_when(
#       selected_model == "high" ~ Mean_perio_billions_high,
#       selected_model == "mid" ~ Mean_perio_billions_mid,
#       selected_model == "low" ~ Mean_perio_billions_mid,
#       TRUE ~ NA_real_
#     ),
#     selected_SD_perio_billions = case_when(
#       selected_model == "high" ~ SD_perio_billions_high,
#       selected_model == "mid" ~ SD_perio_billions_mid,
#       selected_model == "low" ~ SD_perio_billions_mid,
#       TRUE ~ NA_real_
#     ),
#     selected_Mean_replace_billions = case_when(
#       selected_model == "high" ~ Mean_replace_billions_high,
#       selected_model == "mid" ~ Mean_replace_billions_mid,
#       selected_model == "low" ~ Mean_replace_billions_mid,
#       TRUE ~ NA_real_
#     ),
#     selected_SD_replace_billions = case_when(
#       selected_model == "high" ~ SD_replace_billions_high,
#       selected_model == "mid" ~ SD_replace_billions_mid,
#       selected_model == "low" ~ SD_replace_billions_mid,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#    bind_rows(long_summary_2021, long_summary_2025) %>%
#    filter(!Country == "Global") %>%
#    mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
#    mutate (Level = 3)
#  
# hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
#   mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names
# 
# mid_scenario_joined <- hier %>%
#  full_join(mid_scenario, by = "iso3c") %>%
#  filter(!is.na(Country.y)) %>%
#  mutate(Country = coalesce(Country.y, Country.x)) %>%
#  select(-Country.x, -Country.y)
#  
# global_mid_scenario <- mid_scenario%>%
#   group_by(Year) %>%
#   summarise(
#     Country = "Global",
#     selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
#     selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
#     selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
#     selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
#     selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
#     selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE))
#   ) %>%
#   mutate(Level = 0)
# 
# superregion_mid_scenario <- mid_scenario_joined%>%
#   group_by(Year,Superregion) %>%
#   summarise(
#     selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
#     selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
#     selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
#     selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
#     selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
#     selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE))
#   ) %>%
#   mutate(Level = 1) %>%
#   mutate(Country = Superregion)
# 
# mid_scenario_full <- mid_scenario_joined %>%
#    bind_rows(global_mid_scenario, superregion_mid_scenario)
# 
# write_csv(mid_scenario_full, "outputs_forecast/mid_scenario.csv")
# 
# high_scenario <- long_full_forecast %>%
#   filter(Year>2025) %>%
#   mutate(
#     selected_Mean_total_billions = Mean_total_billions_high,
#     selected_SD_total_billions = SD_total_billions_high,
#     selected_Mean_perio_billions = Mean_perio_billions_high,
#     selected_SD_perio_billions = SD_perio_billions_high,
#     selected_Mean_replace_billions = Mean_replace_billions_high,
#     selected_SD_replace_billions = SD_replace_billions_high
#   ) %>%
#   bind_rows(long_summary_2021, long_summary_2025) %>%
#   filter(!Country == "Global") %>%
#   mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
#   mutate (Level = 3)
# 
# hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>% #read GBD hierarchy
#   mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))  #matching with ISO code for country names
# 
# high_scenario_joined <- hier %>%
#   full_join(high_scenario, by = "iso3c") %>%
#   filter(!is.na(Country.y)) %>%
#   mutate(Country = coalesce(Country.y, Country.x)) %>%
#   select(-Country.x, -Country.y)
# 
# superregion_high_scenario <- high_scenario_joined%>%
#   group_by(Year,Superregion) %>%
#   summarise(
#     selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
#     selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
#     selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
#     selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
#     selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
#     selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE))
#   ) %>%
#   mutate(Level = 1) %>%
#   mutate(Country = Superregion)
# 
# global_high_scenario <- high_scenario %>%
#   group_by(Year) %>%
#   summarise(
#     Country = "Global",
#     selected_Mean_total_billions = sum(selected_Mean_total_billions, na.rm = TRUE),
#     selected_SD_total_billions = sqrt(sum(selected_SD_total_billions^2, na.rm = TRUE)),
#     selected_Mean_perio_billions = sum(selected_Mean_perio_billions, na.rm = TRUE),
#     selected_SD_perio_billions = sqrt(sum(selected_SD_perio_billions^2, na.rm = TRUE)),
#     selected_Mean_replace_billions = sum(selected_Mean_replace_billions, na.rm = TRUE),
#     selected_SD_replace_billions = sqrt(sum(selected_SD_replace_billions^2, na.rm = TRUE))
#   ) %>%
#   mutate(Level = 0)
# 
# high_scenario_full <- high_scenario %>%
#   bind_rows(global_high_scenario, superregion_mid_scenario)
# 
# write_csv(high_scenario_full, "outputs_forecast/high_scenario.csv")
