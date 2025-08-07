# ==============================================================================
# Cost Model Function — Optimised Version with Cleanup
# ==============================================================================

# ============================================================================
# Load packages
# ============================================================================
library(tidyverse)
library(MCMCpack)
library(dplyr)

# ============================================================================
# Monte Carlo model function
# ============================================================================

run_cost_model <- function(
    countries_path,
    procedure_lookup_high_path,
    procedure_lookup_mid_path,
    procedure_lookup_low_path,
    tx_split_high_path,
    tx_split_mid_path,
    tx_split_low_path,
    severity_split_path,
    scenario = c("high", "mid", "low"),
    n_sims = 3000,
    output_dir = "outputs"
) {
  
  # ============================================================================
  # Validate scenario input
  # ============================================================================
  scenario <- match.arg(scenario)
  
  # ============================================================================
  # Create output folder if needed
  # ============================================================================
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # ============================================================================
  # Load inputs with explicit col_types for speed
  # ============================================================================
  message("Loading input files ...")
  countries <- read_csv(countries_path, col_types = cols())
  severity_split <- read_csv(severity_split_path, col_types = cols())
  
  procedure_lookup <- case_when(
    scenario == "high" ~ read_csv(procedure_lookup_high_path, col_types = cols()),
    scenario == "mid"  ~ read_csv(procedure_lookup_mid_path, col_types = cols()),
    scenario == "low"  ~ read_csv(procedure_lookup_low_path, col_types = cols())
  )
  
  tx_split <- case_when(
    scenario == "high" ~ read_csv(tx_split_high_path, col_types = cols()),
    scenario == "mid"  ~ read_csv(tx_split_mid_path, col_types = cols()),
    scenario == "low"  ~ read_csv(tx_split_low_path, col_types = cols())
  )
  
  # ============================================================================
  # Reshape country costs to long format
  # ============================================================================
  message("Reshaping country cost data ...")
  countries_long <- countries %>%
    pivot_longer(
      cols = matches("Consult_simple|Consult_perio|OPG|PA|Prophy|RootDeb|OHI|Extraction|OFD|GTR|Single_implant|Implant_surgery|Full_fixed|Denture|Denture_repair|Maintenance_simp|Maintenance_perio"),
      names_to = "Var",
      values_to = "Value"
    ) %>%
    mutate(
      Metric = if_else(str_ends(Var, "_sd"), "SD_cost", "Mean_cost"),
      Procedure = str_remove(Var, "_sd$")
    ) %>%
    dplyr::select(Country, Procedure, Metric, Value) %>%
    pivot_wider(names_from = Metric, values_from = Value)
  
  # ============================================================================
  # Simulate base population and prevalence
  # ============================================================================
  message("Simulating base population and prevalence ...")
  sim_base <- crossing(
    countries,
    sim = seq_len(n_sims)
  ) %>%
    mutate(
      Pop = rnorm(n(), Pop, 0),
      Perio_prev = rnorm(n(), Perio_prev, Perio_prev_sd),
      Edent_prev = rnorm(n(), Edent_prev, Edent_prev_sd),
      Perio_pop = Pop * Perio_prev,
      Healthy_pop = Pop * (1 - Perio_prev - Edent_prev)
    ) %>%
    dplyr::select(Country, sim, Pop, Perio_pop, Healthy_pop) # Drop input prev and sd
  
  # ============================================================================
  # Dirichlet severity splits
  # ============================================================================
  message("Drawing Dirichlet severity splits ...")
  alpha_perio <- severity_split %>% filter(Periofrac_mean > 0) %>% pull(Periofrac_mean)
  alpha_healthy <- severity_split %>% filter(Healthyfrac_mean > 0) %>% pull(Healthyfrac_mean)
  
  severity_draws <- tibble(sim = seq_len(n_sims)) %>%
    mutate(
      Perio = map(sim, ~ MCMCpack::rdirichlet(1, alpha_perio) %>% as.numeric()),
      Healthy = map(sim, ~ MCMCpack::rdirichlet(1, alpha_healthy) %>% as.numeric())
    ) %>%
    mutate(
      Perio_tbl = map(Perio, ~ tibble(
        Severity = c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4"),
        Severity_frac = .
      )),
      Healthy_tbl = map(Healthy, ~ tibble(
        Severity = c("Health", "Stage_I_II"),
        Severity_frac = .
      ))
    ) %>%
    pivot_longer(
      cols = c(Perio_tbl, Healthy_tbl),
      names_to = "Group",
      values_to = "tbl"
    ) %>%
    unnest(tbl) %>%
    mutate(Group = if_else(Group == "Perio_tbl", "Perio", "Healthy")) %>%
    dplyr::select(sim, Group, Severity, Severity_frac) # drop raw vectors
  
  severity_groups <- tibble(
    Group = c(rep("Perio", 3), rep("Healthy", 2)),
    Severity = c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4", "Health", "Stage_I_II")
  )
  
  sim_severity <- sim_base %>%
    crossing(severity_groups) %>%
    left_join(severity_draws, by = c("sim", "Group", "Severity")) %>%
    mutate(
      Patients = case_when(
        Group == "Perio" ~ Perio_pop * Severity_frac,
        Group == "Healthy" ~ Healthy_pop * Severity_frac
      )
    ) %>%
    filter(Patients > 0) %>%
    dplyr::select(Country, sim, Severity, Group, Patients) # drop frac, pops
  
  # ============================================================================
  # Dirichlet treatment splits within severity
  # ============================================================================
  message("Drawing Dirichlet treatment splits (vectorised) ...")
  treatment_draws <- sim_severity %>%
    distinct(sim, Severity, Group) %>%
    left_join(tx_split, by = "Group") %>%
    mutate(
      alpha = pmap(list(Untreated, Initial, Maintenance), c),
      draw = map(alpha, ~ MCMCpack::rdirichlet(1, .x) %>% as.numeric()),
      # Only keep Initial & Maintenance fractions
      Treat_tbl = map(draw, ~ tibble(
        Treatment = c("Untreated", "Initial", "Maintenance"),
        Treat_frac = .x
      )) %>%
        map(~ filter(.x, Treatment != "Untreated")) # drop untreated rows to minimise memory
    ) %>%
    dplyr::select(sim, Severity, Treat_tbl) %>%
    unnest(Treat_tbl)
  
  # Cross only Initial & Maintenance
  sim_treatment <- sim_severity %>%
    crossing(Treatment = c("Initial", "Maintenance")) %>%  # no Untreated rows to minimise memory
    left_join(treatment_draws, by = c("sim", "Severity", "Treatment")) %>%
    mutate(Patients = Patients * Treat_frac) %>%
    filter(Patients > 0) %>%
    dplyr::select(Country, sim, Severity, Treatment, Patients) # drop Treat_frac
  
  # ============================================================================
  # Join procedures and simulate costs
  # ============================================================================
  message("Joining procedure lookups ...")
  sim_procedures <- sim_treatment %>%
    inner_join(procedure_lookup, by = c("Severity", "Treatment"), relationship = "many-to-many") %>%
    left_join(countries_long, by = c("Country", "Procedure"))
  
  # ============================================================================
  # Vectorised simulation of costs
  # ============================================================================
  message("Simulating procedure costs (vectorised) ...")
  sim_costed <- sim_procedures %>%
    mutate(
      cost_per_unit = ifelse(SD_cost == 0 | is.na(SD_cost),
                             Mean_cost,
                             rnorm(n(), Mean_cost, SD_cost)),
      unit_count = ifelse(Number_sd == 0 | is.na(Number_sd),
                          Number_mean,
                          rnorm(n(), Number_mean, Number_sd)),
      Total_cost = cost_per_unit * unit_count * Patients
    ) %>%
    dplyr::select(Country, sim, Severity, Treatment, Procedure, Total_cost) # drop input means/sds
  
  # ============================================================================
  # Summarise costs by different slices
  # ============================================================================
  message("Summarising costs ...")
  results <- sim_costed %>%
    group_by(Country, sim, Severity, Treatment) %>%
    summarise(Cost = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
    group_by(Country, sim) %>%
    mutate(Total_cost = sum(Cost, na.rm = TRUE))
  
  # ============================================================================
  # Country-level total, perio, replacement
  # ============================================================================
  perio_procs <- c("RootDeb", "OHI", "OFD", "GTR", "Maintenance_perio")
  perio_sev <- c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4")
  replace_procs <- c("Single_implant", "Implant_surgery", "Full_fixed", "Denture", "Denture_repair")
  
  country_total <- results %>%
    group_by(Country, sim) %>%
    summarise(Total_cost = sum(Cost, na.rm = TRUE), .groups = "drop")
  
  country_perio <- sim_costed %>%
    filter(Procedure %in% perio_procs, Severity %in% perio_sev) %>%
    group_by(Country, sim) %>%
    summarise(Perio_cost = sum(Total_cost, na.rm = TRUE), .groups = "drop")
  
  country_replace <- sim_costed %>%
    filter(Procedure %in% replace_procs) %>%
    group_by(Country, sim) %>%
    summarise(Replace_cost = sum(Total_cost, na.rm = TRUE), .groups = "drop")
  
  country_combined <- country_total %>%
    left_join(country_perio, by = c("Country", "sim")) %>%
    left_join(country_replace, by = c("Country", "sim")) %>%
    group_by(Country) %>%
    summarise(
      Mean_total_billions = mean(Total_cost) / 1e9,
      SD_total_billions = sd(Total_cost) / 1e9,
      Mean_perio_billions = mean(Perio_cost, na.rm = TRUE) / 1e9,
      SD_perio_billions = sd(Perio_cost, na.rm = TRUE) / 1e9,
      Mean_replace_billions = mean(Replace_cost, na.rm = TRUE) / 1e9,
      SD_replace_billions = sd(Replace_cost, na.rm = TRUE) / 1e9
    )
  
  country_sev_combined <- sim_costed %>%
    group_by(Country, Severity, sim) %>%
    summarise(Total_cost = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
    group_by(Country, Severity) %>%
    summarise(
      Mean_total_billions = mean(Total_cost) / 1e9,
      SD_total_billions = sd(Total_cost) / 1e9
    )
  
  procedure_combined <- sim_costed %>%
    group_by(Country, Procedure, sim) %>%
    summarise(Procedure_cost = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
    group_by(Country, Procedure) %>%
    summarise(
      Mean_total_billions = mean(Procedure_cost) / 1e9,
      SD_total_billions   = sd(Procedure_cost) / 1e9,
      .groups = "drop"
    )
  
  # ============================================================================
  # Write outputs
  # ============================================================================
  message("Writing output CSVs ...")
  write_csv(country_combined, file.path(output_dir, paste0("country_combined_", scenario, ".csv")))
  write_csv(country_sev_combined, file.path(output_dir, paste0("country_sev_combined_", scenario, ".csv")))
  write_csv(procedure_combined, file.path(output_dir, paste0("procedure_combined_", scenario, ".csv")))
  
  invisible(list(
    country_combined = country_combined,
    country_sev_combined = country_sev_combined,
    procedure_combined = procedure_combined
  ))
}


# ==============================================================================
# A separate function to permit chunking into slices to minimise memory space, and loop low/mid/high scenarios
# ==============================================================================


run_cost_model_scenarios <- function(
    countries_path,
    procedure_lookup_high_path,
    procedure_lookup_mid_path,
    procedure_lookup_low_path,
    tx_split_high_path,
    tx_split_mid_path,
    tx_split_low_path,
    severity_split_path,
    n_sims = 3000,
    output_dir = "outputs",
    scenarios = c("low", "mid", "high"),
    slice_size = 100  # ✅ configurable, default 100
) {
  library(tidyverse)
  
  # -----------------------------------------------------------
  # Load the countries data to check total count
  # -----------------------------------------------------------
  all_countries <- read_csv(countries_path, col_types = cols())
  n_countries <- length(unique(all_countries$Country))
  
  # -----------------------------------------------------------
  # Loop over scenarios
  # -----------------------------------------------------------
  for (sc in scenarios) {
    message("Running scenario: ", sc)
    
    if (n_countries > slice_size) {
      
      # -----------------------------------------------------------
      # Split countries into slices of max 'slice_size'
      # -----------------------------------------------------------
      country_groups <- split(
        unique(all_countries$Country),
        ceiling(seq_along(unique(all_countries$Country)) / slice_size)
      )
      
      slice_outputs <- vector("list", length(country_groups))
      
      for (i in seq_along(country_groups)) {
        message("  Running slice ", i, " of ", length(country_groups))
        
        # Filter countries in this slice
        slice_countries <- all_countries %>%
          filter(Country %in% country_groups[[i]])
        
        # Write temporary file for this slice
        slice_path <- paste0("temp_slice_", i, ".csv")
        write_csv(slice_countries, slice_path)
        
        # -----------------------------------------------------------
        # Run the core model for this slice
        # -----------------------------------------------------------
        run_cost_model(
          countries_path = slice_path,
          procedure_lookup_high_path = procedure_lookup_high_path,
          procedure_lookup_mid_path  = procedure_lookup_mid_path,
          procedure_lookup_low_path  = procedure_lookup_low_path,
          tx_split_high_path = tx_split_high_path,
          tx_split_mid_path  = tx_split_mid_path,
          tx_split_low_path  = tx_split_low_path,
          severity_split_path = severity_split_path,
          scenario = sc,
          n_sims = n_sims,
          output_dir = output_dir
        )
        
        # Read slice results
        country_combined <- read_csv(file.path(output_dir, paste0("country_combined_", sc, ".csv")))
        country_sev_combined <- read_csv(file.path(output_dir, paste0("country_sev_combined_", sc, ".csv")))
        procedure_combined <- read_csv(file.path(output_dir, paste0("procedure_combined_", sc, ".csv")))
        
        slice_outputs[[i]] <- list(
          country_combined = country_combined,
          country_sev_combined = country_sev_combined,
          procedure_combined = procedure_combined
        )
        
        # Remove temp slice file
        file.remove(slice_path)
      }
      
      # -----------------------------------------------------------
      # Combine all slices for this scenario
      # -----------------------------------------------------------
      country_combined_all <- bind_rows(map(slice_outputs, "country_combined"))
      country_sev_combined_all <- bind_rows(map(slice_outputs, "country_sev_combined"))
      procedure_combined_all <- bind_rows(map(slice_outputs, "procedure_combined"))
      
      # -----------------------------------------------------------
      # Overwrite final scenario files
      # -----------------------------------------------------------
      write_csv(country_combined_all, file.path(output_dir, paste0("country_combined_", sc, ".csv")))
      write_csv(country_sev_combined_all, file.path(output_dir, paste0("country_sev_combined_", sc, ".csv")))
      write_csv(procedure_combined_all, file.path(output_dir, paste0("procedure_combined_", sc, ".csv")))
      
    } else {
      # -----------------------------------------------------------
      # Fewer than slice_size — run directly, overwrite output
      # -----------------------------------------------------------
      run_cost_model(
        countries_path = countries_path,
        procedure_lookup_high_path = procedure_lookup_high_path,
        procedure_lookup_mid_path  = procedure_lookup_mid_path,
        procedure_lookup_low_path  = procedure_lookup_low_path,
        tx_split_high_path = tx_split_high_path,
        tx_split_mid_path  = tx_split_mid_path,
        tx_split_low_path  = tx_split_low_path,
        severity_split_path = severity_split_path,
        scenario = sc,
        n_sims = n_sims,
        output_dir = output_dir
      )
    }
  }
  
  message("All scenarios complete.")
  invisible(NULL)
}


# ==============================================================================
# Optional: cleaning and combining the dataset
# ==============================================================================


  library(tidyverse)
  
  # Load both CSVs
  overwrite_df <- read_csv("country_input.csv", col_types = cols())
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


library(tidyverse)

  # Example file paths (adjust if needed)
  countries_path <- "combined_country_input.csv"
  procedure_lookup_high_path <- "procedure_lookup.csv"
  procedure_lookup_mid_path  <- "procedure_lookup.csv"
#  procedure_lookup_high_path <- "procedure_lookup_increased_maintenance.csv"   #for various scenarios
#  procedure_lookup_mid_path  <- "procedure_lookup_increased_maintenance.csv"
  procedure_lookup_low_path  <- "procedure_lookup_low_scenario.csv"
#  procedure_lookup_low_path  <- "procedure_lookup_low_scenario_increased_maintenance.csv"
    tx_split_high_path <- "tx_split.csv"
  tx_split_mid_path  <- "tx_split_mid_scenario.csv"
  tx_split_low_path  <- "tx_split_low_scenario.csv"
  severity_split_path <- "severity_split.csv"
  
  # Where to write outputs
  output_dir <- "outputs"
  
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
    n_sims = 3000,
    output_dir = output_dir,
    scenarios = c("low", "mid", "high"),  # same as before
    slice_size = 100                         # same base slice size
  )
  
