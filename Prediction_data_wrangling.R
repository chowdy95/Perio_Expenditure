# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(readr)
library(fuzzyjoin)
library(stringr)
library(dplyr)

prediction_high <- read_csv("outputs/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country) # to better identify the source

prediction_mid <- read_csv("outputs/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country) # to better identify the source

prediction_low <- read_csv("outputs/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country) # to better identify the source

other_predictors <- read_csv("outputs/predict_countries.csv") %>%
  dplyr::select(-Country_df2) %>%
  rename(Country = Country_df1)


# ------------------------------------------------------------------------
# 2. Joining all the tibbles together
# ------------------------------------------------------------------------

prediction_combined <- prediction_high %>%
  left_join(prediction_mid, by = "Country") %>%
  left_join(prediction_low, by = "Country") %>%
  left_join(other_predictors, by = "Country")


# ------------------------------------------------------------------------
# 3. Selection of model based on known dental expenditure
# ------------------------------------------------------------------------

library(dplyr)

prediction_selection <- prediction_combined %>%
  rowwise() %>%
  mutate(
    fits_high = Mean_total_billions_high < 0.6 * Dent_exp_usd,
    fits_mid = Mean_total_billions_mid < 0.6 * Dent_exp_usd,
    fits_low = Mean_total_billions_low < 0.6 * Dent_exp_usd,
    selected_Mean_total_billions = case_when(
      fits_high ~ Mean_total_billions_high,
      fits_mid ~ Mean_total_billions_mid,
      fits_low ~ Mean_total_billions_low,
      TRUE ~ Mean_total_billions_low
    ),
    selected_SD_total_billions = case_when(
      selected_Mean_total_billions == Mean_total_billions_high ~ SD_total_billions_high,
      selected_Mean_total_billions == Mean_total_billions_mid ~ SD_total_billions_mid,
      selected_Mean_total_billions == Mean_total_billions_low ~ SD_total_billions_low
    ),
    selected_Mean_perio_billions = case_when(
      selected_Mean_total_billions == Mean_total_billions_high ~ Mean_perio_billions_high,
      selected_Mean_total_billions == Mean_total_billions_mid ~ Mean_perio_billions_mid,
      selected_Mean_total_billions == Mean_total_billions_low ~ Mean_perio_billions_low
    ),
    selected_SD_perio_billions = case_when(
      selected_Mean_total_billions == Mean_total_billions_high ~ SD_perio_billions_high,
      selected_Mean_total_billions == Mean_total_billions_mid ~ SD_perio_billions_mid,
      selected_Mean_total_billions == Mean_total_billions_low ~ SD_perio_billions_low
    ),
    selected_Mean_replace_billions = case_when(
      selected_Mean_total_billions == Mean_total_billions_high ~ Mean_replace_billions_high,
      selected_Mean_total_billions == Mean_total_billions_mid ~ Mean_replace_billions_mid,
      selected_Mean_total_billions == Mean_total_billions_low ~ Mean_replace_billions_low
    ),
    selected_SD_replace_billions = case_when(
      selected_Mean_total_billions == Mean_total_billions_high ~ SD_replace_billions_high,
      selected_Mean_total_billions == Mean_total_billions_mid ~ SD_replace_billions_mid,
      selected_Mean_total_billions == Mean_total_billions_low ~ SD_replace_billions_low
    ),
    selected_model = case_when(
      selected_Mean_total_billions == Mean_total_billions_high ~ "high",
      selected_Mean_total_billions == Mean_total_billions_mid ~ "mid",
      selected_Mean_total_billions == Mean_total_billions_low ~ "low"
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
  )

write_csv(prediction_selection_short, "outputs/short_final_selected_output.csv")
write_csv(prediction_selection, "outputs/final_selected_output.csv")

# ---------------------------------------------------------
# 4. Load all severity CSVs
# ---------------------------------------------------------
sev_high <- read_csv("outputs/country_sev_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Severity))

sev_mid <- read_csv("outputs/country_sev_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Severity))

sev_low <- read_csv("outputs/country_sev_combined_low.csv") %>%
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
proc_high <- read_csv("outputs/procedure_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -c(Country, Procedure))

proc_mid <- read_csv("outputs/procedure_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -c(Country, Procedure))

proc_low <- read_csv("outputs/procedure_combined_low.csv") %>%
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
  "outputs/country_sev_selected.csv"
)

write_csv(
  proc_combined %>% dplyr::select(Country, Procedure, selected_Mean_total_billions, selected_SD_total_billions),
  "outputs/procedure_selected.csv"
)

write_csv(global_severity, "outputs/global_severity.csv")
write_csv(global_procedure, "outputs/global_procedure.csv")
