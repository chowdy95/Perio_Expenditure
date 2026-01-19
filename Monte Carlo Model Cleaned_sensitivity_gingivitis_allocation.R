# ==============================================================================
# One-way sensitivity analysis:
# Allocation of non-severe disease (gingivitis vs Stage I–II)
# ==============================================================================

source("./99_functions.R")

library(MCMCpack)
library(tidyverse)
library(data.table)

# ------------------------------------------------------------------------------
# Common paths
# ------------------------------------------------------------------------------

countries_path <- "./data/combined_country_input.csv"
procedure_lookup_high_path <- "./data/procedure_lookup.csv"
procedure_lookup_mid_path  <- "./data/procedure_lookup.csv"
procedure_lookup_low_path  <- "./data/procedure_lookup_low_scenario.csv"

tx_split_high_path <- "./data/tx_split.csv"
tx_split_mid_path  <- "./data/tx_split_mid_scenario.csv"
tx_split_low_path  <- "./data/tx_split_low_scenario.csv"

# ------------------------------------------------------------------------------
# Sensitivity scenarios
# ------------------------------------------------------------------------------

severity_paths <- list(
  base   = "./data/severity_split.csv",
  gingi60 = "./data/severity_split_60_40.csv",
  gingi40 = "./data/severity_split_40_60.csv"
)

n_sims <- 3000

# ------------------------------------------------------------------------------
# Run sensitivity analyses
# ------------------------------------------------------------------------------

for (s in names(severity_paths)) {
  
  run_cost_model_scenarios(
    countries_path = countries_path,
    procedure_lookup_high_path = procedure_lookup_high_path,
    procedure_lookup_mid_path  = procedure_lookup_mid_path,
    procedure_lookup_low_path  = procedure_lookup_low_path,
    tx_split_high_path = tx_split_high_path,
    tx_split_mid_path  = tx_split_mid_path,
    tx_split_low_path  = tx_split_low_path,
    severity_split_path = severity_paths[[s]],
    n_sims = n_sims,
    output_dir = paste0("outputs_gingi_sensitivity_", s),
    scenarios = c("low", "mid", "high", "WHO_target"),
    slice_size = 999
  )
  
}
