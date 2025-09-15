# -----------------------------------------------------------------------------------------
# 1. Forecast for 2025
# -----------------------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 1. Load Packages and Data
# ------------------------------------------------------------------------

library(tidyverse)
library(fuzzyjoin)

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
  dplyr::select(Country, selected_model) # <<< CHANGED

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
  )

write_csv(prediction_selection_short, "outputs_2025/short_final_selected_output.csv")
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
  dplyr::select(Country, selected_model) # <<< CHANGED

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
  )

write_csv(prediction_selection_short, "outputs_2030/short_final_selected_output.csv")
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
  dplyr::select(Country, selected_model) # <<< CHANGED

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
  )

write_csv(prediction_selection_short, "outputs_2035/short_final_selected_output.csv")
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
  dplyr::select(Country, selected_model) # <<< CHANGED

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
  )

write_csv(prediction_selection_short, "outputs_2040/short_final_selected_output.csv")
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
  dplyr::select(Country, selected_model) # <<< CHANGED

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
  )

write_csv(prediction_selection_short, "outputs_2045/short_final_selected_output.csv")
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
  dplyr::select(Country, selected_model) # <<< CHANGED

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
  )

write_csv(prediction_selection_short, "outputs_2050/short_final_selected_output.csv")
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