# ==============================================================================
# Cost Model Runner — Sensitivity: Periodontal Surgery Allocation
# ==============================================================================

rm(list = ls())

source("./99_functions.R")

library(MCMCpack)
library(tidyverse)
library(data.table)

# ------------------------------------------------------------------------------
# Input paths
# ------------------------------------------------------------------------------

countries_path <- "./data/combined_country_input.csv"

# Example file paths (adjust if needed)
countries_path <- "./data/combined_country_input.csv"
procedure_lookup_high_path <- "./data/procedure_lookup.csv"
procedure_lookup_mid_path <- "./data/procedure_lookup.csv"
#  procedure_lookup_high_path <- "./data/procedure_lookup_increased_maintenance.csv"   #for various scenarios
#  procedure_lookup_mid_path  <- "./data/procedure_lookup_increased_maintenance.csv"
procedure_lookup_low_path <- "./data/procedure_lookup_low_scenario.csv"
procedure_lookup_WHO_target_path <- "./data/procedure_lookup.csv"
#  procedure_lookup_low_path  <- "./data/procedure_lookup_low_scenario_increased_maintenance.csv"
tx_split_high_path <- "./data/tx_split.csv"
tx_split_mid_path <- "./data/tx_split_mid_scenario.csv"
tx_split_low_path <- "./data/tx_split_low_scenario.csv"
tx_split_WHO_target_path <- "./data/tx_split_WHO_target_scenario.csv"
severity_split_path <- "./data/severity_split.csv"

# Baseline procedure lookup
procedure_lookup_baseline <- "./data/procedure_lookup.csv"

# Surgery sensitivity lookup
procedure_lookup_surgsens <- "./data/procedure_lookup_sensitivity_surgery.csv"

tx_split_high_path <- "./data/tx_split.csv"
tx_split_mid_path  <- "./data/tx_split_mid_scenario.csv"
tx_split_low_path  <- "./data/tx_split_low_scenario.csv"

severity_split_path <- "./data/severity_split.csv"

# ------------------------------------------------------------------------------
# Simulation settings
# ------------------------------------------------------------------------------

n_sims     <- 3000
slice_size <- 999

# ------------------------------------------------------------------------------
# Run BASELINE
# ------------------------------------------------------------------------------

run_cost_model_scenarios(
  countries_path = countries_path,
  procedure_lookup_high_path = procedure_lookup_baseline,
  procedure_lookup_mid_path  = procedure_lookup_baseline,
  procedure_lookup_low_path  = procedure_lookup_baseline,
  tx_split_high_path = tx_split_high_path,
  tx_split_mid_path  = tx_split_mid_path,
  tx_split_low_path  = tx_split_low_path,
  severity_split_path = severity_split_path,
  n_sims = n_sims,
  output_dir = "outputs_surgery_sensitivity_base",
  scenarios = c("low", "mid", "high"),
  slice_size = slice_size
)

# ------------------------------------------------------------------------------
# Run SURGERY SENSITIVITY
# ------------------------------------------------------------------------------

run_cost_model_scenarios(
  countries_path = countries_path,
  procedure_lookup_high_path = procedure_lookup_surgsens,
  procedure_lookup_mid_path  = procedure_lookup_surgsens,
  procedure_lookup_low_path  = procedure_lookup_surgsens,
  tx_split_high_path = tx_split_high_path,
  tx_split_mid_path  = tx_split_mid_path,
  tx_split_low_path  = tx_split_low_path,
  severity_split_path = severity_split_path,
  n_sims = n_sims,
  output_dir = "outputs_surgery_sensitivity_flipped",
  scenarios = c("low", "mid", "high"),
  slice_size = slice_size
)
