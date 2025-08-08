# ==============================================================================
# Cost Model Function — Optimised Version with Cleanup
# ==============================================================================

source("./99_functions.R")

# ============================================================================
# Load packages 
# ============================================================================

library(tidyverse) # DPLYR IS PART OF TIDYVERSE - NO NEED TO CALL BOTH
library(MCMCpack)

# ROBIN NOTE - MOVED THE MAIN FUNCTION TO 99_functions.R FILE TO MAKE IT EASIER TO DIGEST

# ==============================================================================
# Optional: cleaning and combining the dataset
# ==============================================================================

# Load both CSVs
overwrite_df <- read_csv("./data/country_input.csv", col_types = cols())
base_df <- read_csv("monte_carlo_input_from_rms.csv", col_types = cols())

# Combine: overwrite countries
combined_df <- base_df %>%
  filter(!Country %in% overwrite_df$Country) %>%
  bind_rows(overwrite_df) %>%
  arrange(Country)

# Join back to base_df to restore missing cols
final_df <- combined_df %>%
  left_join(base_df, by = "Country", suffix = c("", "_base"))

# Identify which columns are the base fallback
base_cols <- names(final_df)[grepl("_base$", names(final_df))]

# Coalesce each pair: new value if present, else fallback
for (col_base in base_cols) {
  col_orig <- sub("_base$", "", col_base)
  final_df[[col_orig]] <- coalesce(final_df[[col_orig]], final_df[[col_base]])
}

# Drop the _base helper columns
final_df <- final_df %>%
  dplyr:::select(-all_of(base_cols))

final_df_clean <- final_df %>%
  dplyr::select(
    -Pop_sd,
    -Dent_exp_usd,
    -Dent_exppc_usd,
    -GDP_per_capita_PPP_2021,
    -Conversion,
    -`Dental Expenditure Per Capita`
  )

# Save
write_csv(final_df_clean, "combined_country_input.csv")


# ==============================================================================
# Calling the function
# ==============================================================================

# Example file paths (adjust if needed)
countries_path <- "./data/combined_country_input.csv"
procedure_lookup_high_path <- "./data/procedure_lookup.csv"
procedure_lookup_mid_path <- "./data/procedure_lookup.csv"
#  procedure_lookup_high_path <- "./data/procedure_lookup_increased_maintenance.csv"   #for various scenarios
#  procedure_lookup_mid_path  <- "./data/procedure_lookup_increased_maintenance.csv"
procedure_lookup_low_path <- "./data/procedure_lookup_low_scenario.csv"
#  procedure_lookup_low_path  <- "./data/procedure_lookup_low_scenario_increased_maintenance.csv"
tx_split_high_path <- "./data/tx_split.csv"
tx_split_mid_path <- "./data/tx_split_mid_scenario.csv"
tx_split_low_path <- "./data/tx_split_low_scenario.csv"
severity_split_path <- "./data/severity_split.csv"

# Where to write outputs
output_dir <- "outputs"

# FOR TESTING
output_dir <- "test"
n_sims = 2

# Call the new scenario runner
run_cost_model_scenarios(
  countries_path = countries_path,
  procedure_lookup_high_path = procedure_lookup_high_path,
  procedure_lookup_mid_path = procedure_lookup_mid_path,
  procedure_lookup_low_path = procedure_lookup_low_path,
  tx_split_high_path = tx_split_high_path,
  tx_split_mid_path = tx_split_mid_path,
  tx_split_low_path = tx_split_low_path,
  severity_split_path = severity_split_path,
  n_sims = n_sims,
  output_dir = output_dir,
  scenarios = c("low", "mid", "high"), # same as before
  slice_size = 100 # same base slice size
)

