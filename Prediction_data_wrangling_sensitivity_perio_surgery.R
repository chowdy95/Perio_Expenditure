# ==============================================================================
# Sensitivity Analysis — Periodontal Surgery Allocation (corrected)
# ==============================================================================

rm(list = ls())

library(tidyverse)
library(countrycode)

# ------------------------------------------------------------------------------
# Load selected utilisation model
# ------------------------------------------------------------------------------

selected_model_df <- read_csv("outputs/final_selected_output.csv") %>%
  select(Country, selected_model)

# ------------------------------------------------------------------------------
# Helper: extract selected model output (aggregate at country level)
# ------------------------------------------------------------------------------

extract_selected_prediction <- function(output_dir, selected_df) {
  
  high <- read_csv(file.path(output_dir, "country_combined_high.csv")) %>%
    mutate(Scenario = "high")
  
  mid <- read_csv(file.path(output_dir, "country_combined_mid.csv")) %>%
    mutate(Scenario = "mid")
  
  low <- read_csv(file.path(output_dir, "country_combined_low.csv")) %>%
    mutate(Scenario = "low")
  
  bind_rows(high, mid, low) %>%
    left_join(selected_df, by = "Country") %>%
    filter(Scenario == selected_model) %>%
    group_by(Country) %>%                            # ensure one row per country
    summarise(Mean_total_billions = sum(Mean_total_billions, na.rm = TRUE), .groups = "drop")
}

# ------------------------------------------------------------------------------
# Load predictions
# ------------------------------------------------------------------------------

pred_base <- extract_selected_prediction(
  "outputs",
  selected_model_df
)

pred_flip <- extract_selected_prediction(
  "outputs_surgery_sensitivity_flipped",
  selected_model_df
)

# ------------------------------------------------------------------------------
# Load hierarchy and ensure one row per country
# ------------------------------------------------------------------------------

hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>%
  mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>%
  distinct(iso3c, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# Aggregate to Global / Superregion only (drop Regions to avoid duplication)
# ------------------------------------------------------------------------------

aggregate_levels <- function(df, label) {
  
  df2 <- df %>%
    mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>%
    left_join(hier, by = "iso3c")
  
  # Global
  global <- tibble(
    Location = "Global",
    Level = "Global",
    Mean_total_billions = sum(df2$Mean_total_billions, na.rm = TRUE)
  )
  
  # Superregions
  superregions <- df2 %>%
    filter(!is.na(Superregion)) %>%
    group_by(Superregion) %>%
    summarise(
      Location = first(Superregion),
      Level = "Superregion",
      Mean_total_billions = sum(Mean_total_billions, na.rm = TRUE),
      .groups = "drop"
    )
  
  bind_rows(global, superregions) %>%
    mutate(Scenario = label)
}

agg_base <- aggregate_levels(pred_base, "baseline")
agg_flip <- aggregate_levels(pred_flip, "surgery_flipped")

# ------------------------------------------------------------------------------
# Delta calculation
# ------------------------------------------------------------------------------

delta_surgery <- bind_rows(agg_base, agg_flip) %>%
  pivot_wider(
    names_from  = Scenario,
    values_from = Mean_total_billions
  ) %>%
  mutate(
    delta_absolute = surgery_flipped - baseline,
    delta_pct      = 100 * delta_absolute / baseline
  ) %>%
  arrange(Location)

# ------------------------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------------------------

write_csv(
  delta_surgery,
  "outputs/surgery_allocation_sensitivity_deltas_corrected.csv"
)

# ------------------------------------------------------------------------------
# Publication-ready table
# ------------------------------------------------------------------------------

publication_table_surgery <- delta_surgery %>%
  select(Location, baseline, delta_absolute, delta_pct) %>%
  rename(
    `2021 Expenditure (current assumption, US$ billion)` = baseline,
    `Change in Expenditure (increased assumption, US$ billion)` = delta_absolute,
    `Change in Expenditure (%) (alternative surgery assumption)` = delta_pct
  ) %>%
  mutate(
    across(
      contains("US$ billion"),
      ~ format(round(.x, 2), big.mark = ",", nsmall = 2)
    ),
    across(
      contains("(%)"),
      ~ paste0(round(.x, 2), "%")
    )
  )

write_csv(
  publication_table_surgery,
  "outputs/sensitivity_analysis_publication_ready_surgery_corrected.csv"
)
