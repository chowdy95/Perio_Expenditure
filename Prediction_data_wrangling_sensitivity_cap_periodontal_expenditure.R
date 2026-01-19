# ------------------------------------------------------------------------
# 0. Load Packages
# ------------------------------------------------------------------------
library(tidyverse)
library(countrycode)

# ------------------------------------------------------------------------
# 1. Load Data
# ------------------------------------------------------------------------
prediction_high <- read_csv("outputs/country_combined_high.csv") %>%
  rename_with(~ paste0(.x, "_high"), -Country)

prediction_mid <- read_csv("outputs/country_combined_mid.csv") %>%
  rename_with(~ paste0(.x, "_mid"), -Country)

prediction_low <- read_csv("outputs/country_combined_low.csv") %>%
  rename_with(~ paste0(.x, "_low"), -Country)

other_predictors <- read_csv("data/2021_prevalence_pop_dentexp_GDP.csv")

prediction_combined <- reduce(
  list(prediction_high, prediction_mid, prediction_low, other_predictors),
  left_join,
  by = "Country"
) %>%
  mutate(Country = ifelse(Country == "Micronesia",
                          "Micronesia (Federated States of)", Country))

# GBD hierarchy
hier <- read_csv("data/GBD_location_hierarchy_wide.csv", locale = locale(encoding = "Latin1")) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

# ------------------------------------------------------------------------
# 2. Function to select model and aggregate at Region / Superregion / Global
# ------------------------------------------------------------------------
run_sensitivity_region <- function(cap_pct) {
  
  sel <- prediction_combined %>%
    mutate(
      # Select total dental expenditure based on cap
      selected_total = case_when(
        Mean_total_billions_high < cap_pct * Dent_exp_usd ~ Mean_total_billions_high,
        Mean_total_billions_mid  < cap_pct * Dent_exp_usd ~ Mean_total_billions_mid,
        TRUE ~ Mean_total_billions_low
      ),
      iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")
    ) %>%
    full_join(hier, by = "iso3c")
  
  # Aggregate at Region level
  regions <- sel %>%
    group_by(Superregion, Region) %>%
    summarise(selected_total = sum(selected_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Level = "Region",
      Location = Region
    )
  
  # Aggregate at Superregion level (sum of all countries in that Superregion)
  superregions <- sel %>%
    group_by(Superregion) %>%
    summarise(selected_total = sum(selected_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Level = "Superregion",
      Location = Superregion
    )
  
  # Combine Region and Superregion, but remove redundant Region if same name as Superregion
  regions <- regions %>%
    filter(!(Location %in% superregions$Location))
  
  # Global total
  global <- tibble(
    Location = "Global",
    Level = "Global",
    selected_total = sum(sel$selected_total, na.rm = TRUE)
  )
  
  bind_rows(global, regions, superregions)
}

# ------------------------------------------------------------------------
# 3. Run for baseline 75% and reductions 60%, 50%
# ------------------------------------------------------------------------
results_75 <- run_sensitivity_region(0.75) %>% rename(total75 = selected_total)
results_60 <- run_sensitivity_region(0.60) %>% rename(total60 = selected_total)
results_50 <- run_sensitivity_region(0.50) %>% rename(total50 = selected_total)

# ------------------------------------------------------------------------
# 4. Combine results and calculate deltas
# ------------------------------------------------------------------------
sensitivity_df <- results_75 %>%
  left_join(results_60 %>% select(Location, total60), by = "Location") %>%
  left_join(results_50 %>% select(Location, total50), by = "Location") %>%
  mutate(
    baseline = total75,
    delta_60 = total60 - total75,
    delta_60_pct = 100 * delta_60 / total75,
    delta_50 = total50 - total75,
    delta_50_pct = 100 * delta_50 / total75
  ) %>%
  select(Location, Level, baseline, total60, total50,
         delta_60, delta_60_pct, delta_50, delta_50_pct) %>%
  arrange(Level, Location)

# ------------------------------------------------------------------------
# 5. Save output
# ------------------------------------------------------------------------
write_csv(sensitivity_df, "outputs/sensitivity_analysis_total_expenditure_cap.csv")

# ------------------------------------------------------------------------
# Convert sensitivity_df to publication-ready table with units and formatting
# ------------------------------------------------------------------------
publication_table <- sensitivity_df %>%
  # Keep only Global and Superregion
  filter(Level %in% c("Global", "Superregion")) %>%
  # Drop raw total columns
  select(
    Location, Level,
    baseline,
    delta_60, delta_60_pct,
    delta_50, delta_50_pct
  ) %>%
  # Rename columns with clear labels
  rename(
    `2021 Expenditure (current 75% cap assumption, US$ billion)` = baseline,
    `Change in Expenditure (60% cap, US$ billion)` = delta_60,
    `Change in Expenditure (%) (60% cap)` = delta_60_pct,
    `Change in Expenditure (50% cap, US$ billion)` = delta_50,
    `Change in Expenditure (%) (50% cap)` = delta_50_pct
  ) %>%
  # Format numeric columns
  mutate(
    across(contains("$ billion"), ~ format(round(.x, 2), big.mark = ",", nsmall = 2)),
    across(contains("(%)"), ~ paste0(round(.x, 2), "%"))
  ) %>%
  # Optional: reorder rows (Global first, then Superregions alphabetically)
  arrange(factor(Level, levels = c("Global", "Superregion")), Location) %>%
  select(-Level)

# ------------------------------------------------------------------------
# Save publication-ready table
# ------------------------------------------------------------------------
write_csv(publication_table, "outputs/sensitivity_analysis_publication_ready_cap_expenditure.csv")
