# ==============================================================================
# One-way Sensitivity Analysis:
# Allocation of non-severe dentate population (gingivitis vs Stage I–II)
# ==============================================================================

rm(list = ls())

# ------------------------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------------------------

library(tidyverse)
library(countrycode)

# ------------------------------------------------------------------------------
# 2. Load selected utilisation scenario
# ------------------------------------------------------------------------------

selected_model_df <- read_csv("outputs/final_selected_output.csv") %>%
  select(Country, selected_model)

# ------------------------------------------------------------------------------
# 3. Helper function: extract selected model output
# ------------------------------------------------------------------------------

extract_selected_prediction <- function(output_dir, selected_df) {
  
  high <- read_csv(file.path(output_dir, "country_combined_high.csv")) %>%
    mutate(Scenario = "high")
  
  mid <- read_csv(file.path(output_dir, "country_combined_mid.csv")) %>%
    mutate(Scenario = "mid")
  
  low <- read_csv(file.path(output_dir, "country_combined_low.csv")) %>%
    mutate(Scenario = "low")
  
  all_preds <- bind_rows(high, mid, low)
  
  all_preds %>%
    left_join(selected_df, by = "Country") %>%
    filter(Scenario == selected_model) %>%
    select(Country, Mean_total_billions)
}

# ------------------------------------------------------------------------------
# 4. Load predictions for baseline and sensitivity scenarios
# ------------------------------------------------------------------------------

pred_base <- extract_selected_prediction(
  "outputs",
  selected_model_df
)

pred_gingi60 <- extract_selected_prediction(
  "outputs_gingi_sensitivity_gingi60",
  selected_model_df
)

pred_gingi40 <- extract_selected_prediction(
  "outputs_gingi_sensitivity_gingi40",
  selected_model_df
)

# ------------------------------------------------------------------------------
# 5. Load GBD hierarchy
# ------------------------------------------------------------------------------

hierarchy <- read_csv(
  "data/GBD_location_hierarchy_wide.csv",
  locale = locale(encoding = "Latin1")
) %>%
  mutate(iso3c = countrycode(Country, "country.name", "iso3c"))

# ------------------------------------------------------------------------------
# 6. Aggregate to Global / Superregion / Region (FIXED)
# ------------------------------------------------------------------------------

aggregate_levels <- function(df, scenario_name) {
  
  df_iso <- df %>%
    mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>%
    left_join(
      hierarchy %>% select(iso3c, Region, Superregion),
      by = "iso3c"
    )
  
  # ---- Global ----
  global <- df_iso %>%
    summarise(
      Location = "Global",
      Level = "Global",
      Mean_total_billions = sum(Mean_total_billions, na.rm = TRUE)
    )
  
  # ---- Superregions ----
  superregions <- df_iso %>%
    filter(!is.na(Superregion)) %>%
    group_by(Superregion) %>%
    summarise(
      Location = first(Superregion),
      Level = "Superregion",
      Mean_total_billions = sum(Mean_total_billions, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Regions ----
  regions <- df_iso %>%
    filter(!is.na(Region)) %>%
    group_by(Region) %>%
    summarise(
      Location = first(Region),
      Level = "Region",
      Mean_total_billions = sum(Mean_total_billions, na.rm = TRUE),
      .groups = "drop"
    )
  
  bind_rows(global, superregions, regions) %>%
    mutate(Scenario = scenario_name)
}

agg_base    <- aggregate_levels(pred_base,    "baseline")
agg_gingi60 <- aggregate_levels(pred_gingi60, "gingi60")
agg_gingi40 <- aggregate_levels(pred_gingi40, "gingi40")

agg_all <- bind_rows(agg_base, agg_gingi60, agg_gingi40)


# ------------------------------------------------------------------------------
# 7. Calculate deltas (FIXED)
# ------------------------------------------------------------------------------

delta_table <- agg_all %>%
  pivot_wider(
    names_from  = Scenario,
    values_from = Mean_total_billions
  ) %>%
  mutate(
    delta_gingi60      = gingi60 - baseline,
    delta_gingi60_pct  = 100 * delta_gingi60 / baseline,
    delta_gingi40      = gingi40 - baseline,
    delta_gingi40_pct  = 100 * delta_gingi40 / baseline,
    max_abs_pct_change = pmax(
      abs(delta_gingi60_pct),
      abs(delta_gingi40_pct),
      na.rm = TRUE
    )
  ) %>%
  arrange(Level, desc(max_abs_pct_change))


# ------------------------------------------------------------------------------
# 8. Save outputs
# ------------------------------------------------------------------------------

write_csv(delta_table, "outputs/gingivitis_allocation_sensitivity_deltas.csv")


# ------------------------------------------------------------------------
# Convert gingivitis allocation sensitivity analysis to publication-ready table
# ------------------------------------------------------------------------
publication_table_gingivitis <- delta_table %>%
  # Keep only Global and Superregion
  filter(Level %in% c("Global", "Superregion")) %>%
  
  # Select relevant columns
  select(
    Location,
    Level,
    baseline,
    delta_gingi60,
    delta_gingi60_pct,
    delta_gingi40,
    delta_gingi40_pct,
    max_abs_pct_change
  ) %>%
  
  # Rename columns with publication-ready headers
  rename(
    `2021 Expenditure (current equal-allocation assumption, US$ billion)` = baseline,
    `Change in Expenditure (60–40 allocation, US$ billion)` = delta_gingi60,
    `Change in Expenditure (%) (60–40 allocation)` = delta_gingi60_pct,
    `Change in Expenditure (40–60 allocation, US$ billion)` = delta_gingi40,
    `Change in Expenditure (%) (40–60 allocation)` = delta_gingi40_pct,
    `Maximum absolute change (%)` = max_abs_pct_change
  ) %>%
  
  # Format numeric columns
  mutate(
    across(
      contains("US$ billion"),
      ~ format(round(.x, 2), big.mark = ",", nsmall = 2)
    ),
    across(
      contains("(%)"),
      ~ paste0(round(.x, 2), "%")
    )
  ) %>%
  
  # Order rows: Global first, then Superregions alphabetically
  arrange(
    factor(Level, levels = c("Global", "Superregion")),
    Location
  ) %>%
  
  # Drop Level column for final presentation
  select(-Level)

# ------------------------------------------------------------------------
# Save publication-ready table
# ------------------------------------------------------------------------
write_csv(
  publication_table_gingivitis,
  "outputs/sensitivity_analysis_publication_ready_gingivitis_allocation.csv"
)

