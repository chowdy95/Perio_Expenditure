# ==============================================================================
# Cost Model Function — Optimised Version with Cleanup
# ==============================================================================

source("./99_functions.R")

# ============================================================================
# Load packages 
# ============================================================================

library(tidyverse) # DPLYR IS PART OF TIDYVERSE - NO NEED TO CALL BOTH
library(MCMCpack)

# ROBIN NOTE - MOVED THE MAIN FUNCTION TO 99_functions.R FILE AND 
#   CLEANING/COMBINATION STEP TO PROCEDURE COST FILE TO MAKE IT EASIER TO DIGEST


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

n_sims = 30000

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
  slice_size = 999
)

