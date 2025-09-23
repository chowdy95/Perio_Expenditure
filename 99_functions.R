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
    scenario = c("high", "mid", "low", "WHO_target"),
    n_sims = 3000,
    output_dir = "outputs") {
  
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
    scenario == "mid" ~ read_csv(procedure_lookup_mid_path, col_types = cols()),
    scenario == "low" ~ read_csv(procedure_lookup_low_path, col_types = cols()),
    scenario == "WHO_target" ~ read_csv(procedure_lookup_WHO_target_path, col_types = cols())
  )
  
  tx_split <- case_when(
    scenario == "high" ~ read_csv(tx_split_high_path, col_types = cols()),
    scenario == "mid" ~ read_csv(tx_split_mid_path, col_types = cols()),
    scenario == "low" ~ read_csv(tx_split_low_path, col_types = cols()),
    scenario == "WHO_target" ~ read_csv(tx_split_WHO_target_path, col_types = cols())
  )
  
  # ============================================================================
  # Reshape country costs to long format
  # ============================================================================
  message("Reshaping country cost data ...")

  procedures <- c(
    "Consult_simple", "Consult_perio", "OPG", "PA", "Prophy", "RootDeb", "OHI",
    "Extraction", "OFD", "GTR", "Single_implant", "Implant_surgery",
    "Full_fixed", "Denture", "Denture_repair", "Maintenance_simp",
    "Maintenance_perio"
  )
  
  pattern <- paste0(procedures, collapse = "|")
  
  countries_long <- countries %>%
    pivot_longer(
      cols = matches(paste0(pattern, "_shape|", pattern, "_rate")),
      names_to = "Var",
      values_to = "Value"
    ) %>%
    mutate(
      Metric    = if_else(str_ends(Var, "_shape"), "Shape", "Rate"),
      Procedure = str_remove(Var, "_shape$|_rate$")
    ) %>%
    select(Country, Procedure, Metric, Value) %>%
    pivot_wider(names_from = Metric, values_from = Value)
  
  # ============================================================================
  # Simulate base population and prevalence
  # ============================================================================
  message("Simulating base population and prevalence ...")
  sim_base <- crossing(
    countries,
    sim = seq_len(n_sims)
  ) %>%
    transmute(          # Swapped from mutate
      Country, sim,
      Pop = rnorm(n(), Pop, Pop_sd),
      Perio_prev = rnorm(n(), Perio_prev, Perio_prev_sd),
      Edent_prev = rnorm(n(), Edent_prev, Edent_prev_sd),
      Perio_pop = Pop * Perio_prev,
      Healthy_pop = Pop * (1 - Perio_prev - Edent_prev)
    ) %>%
    dplyr::select(-Pop, -Perio_prev, -Edent_prev) %>%  # Drop input prev and sd
    # dplyr::select(Country, sim, Pop, Perio_pop, Healthy_pop) # Drop input prev and sd
    mutate(row_id = row_number())
  
  # ============================================================================
  # Dirichlet severity splits
  # ============================================================================
  message("Drawing Dirichlet severity splits ...")
  alpha_perio <- severity_split %>%
    filter(Periofrac_mean > 0) %>%
    pull(Periofrac_mean)
  alpha_healthy <- severity_split %>%
    filter(Healthyfrac_mean > 0) %>%
    pull(Healthyfrac_mean)
  
  # --- 1. Define severity labels and alpha vectors ---
  perio_severity <- c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4")
  healthy_severity <- c("Health", "Stage_I_II")
  
  # --- 2. Perio: vectorised Dirichlet draws ---
  n_rows <- nrow(sim_base)
  n_sev_perio <- length(perio_severity)
  
  # generate all gamma draws in one go
  gamma_perio <- matrix(rgamma(n_rows * n_sev_perio, shape = rep(alpha_perio, each = n_rows)),
                        nrow = n_rows, ncol = n_sev_perio, byrow = FALSE)
  dirichlet_perio <- gamma_perio / rowSums(gamma_perio)
  
  # attach to sim_base and reshape long
  perio_tbl <- sim_base %>%
    bind_cols(as_tibble(dirichlet_perio)) %>%
    setNames(c(names(sim_base), perio_severity)) %>%
    pivot_longer(
      cols = all_of(perio_severity),
      names_to = "Severity",
      values_to = "Frac"
    ) %>%
    transmute(
      Country, sim, Severity, 
      Group = "Perio",
      Patients = Perio_pop * Frac
    )
  
  # --- 3. Healthy: same approach ---
  n_sev_healthy <- length(healthy_severity)
  gamma_healthy <- matrix(rgamma(n_rows * n_sev_healthy, shape = rep(alpha_healthy, each = n_rows)),
                          nrow = n_rows, ncol = n_sev_healthy, byrow = FALSE)
  dirichlet_healthy <- gamma_healthy / rowSums(gamma_healthy)
  
  healthy_tbl <- sim_base %>%
    bind_cols(as_tibble(dirichlet_healthy)) %>%
    setNames(c(names(sim_base), healthy_severity)) %>%
    pivot_longer(
      cols = all_of(healthy_severity),
      names_to = "Severity",
      values_to = "Frac"
    ) %>%
    mutate(
      Group = "Healthy",
      Patients = Healthy_pop * Frac
    ) %>%
    select(Country, sim, Group, Severity, Patients)
  
  # --- 4. Combine ---
  sim_severity <- bind_rows(perio_tbl, healthy_tbl) %>%
    arrange(Country, sim, Group, Severity)
  
  
  #--------------------------------------------------------------------
  
  # Superseded by gamma approach to make one huge draw all at once and avoid row-wise map and unnest
  
  # perio_severity <- c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4")
  # healthy_severity <- c("Health", "Stage_I_II")
  # 
  # # Perio
  # n_sims <- nrow(sim_base)
  # perio_frac <- rdirichlet(n_sims, alpha_perio)  # matrix n_sims x 3
  # 
  # perio_tbl <- sim_base %>%
  #   mutate(Perio_frac = split(perio_frac, row(perio_frac))) %>%  # one list per row
  #   unnest_wider(Perio_frac, names_sep = "_") %>%
  #   pivot_longer(
  #     cols = starts_with("Perio_frac_"),
  #     names_to = "Severity",
  #     values_to = "Frac"
  #   ) %>%
  #   mutate(
  #     Group = "Perio",
  #     Severity = factor(Severity, labels = perio_severity),
  #     Patients = Perio_pop * Frac
  #   ) %>%
  #   dplyr::select(Country, sim, Group, Severity, Patients)
  # 
  # # Healthy
  # healthy_frac <- rdirichlet(n_sims, alpha_healthy)  # matrix n_sims x 2
  # 
  # healthy_tbl <- sim_base %>%
  #   mutate(Healthy_frac = split(healthy_frac, row(healthy_frac))) %>%
  #   unnest_wider(Healthy_frac, names_sep = "_") %>%
  #   pivot_longer(
  #     cols = starts_with("Healthy_frac_"),
  #     names_to = "Severity",
  #     values_to = "Frac"
  #   ) %>%
  #   mutate(
  #     Group = "Healthy",
  #     Severity = factor(Severity, labels = healthy_severity),
  #     Patients = Healthy_pop * Frac
  #   ) %>%
  #   dplyr::select(Country, sim, Group, Severity, Patients)
  # 
  # # Combine
  # sim_severity <- bind_rows(perio_tbl, healthy_tbl) %>%
  #   arrange(Country, sim, Group, Severity)

  
  #-----------------------------------------------------------------------------------------
  
  # severity_draws <- tibble( # TIBBLE CAN HANDLE OPERATIONS SEQUENTIALLY, MUTATE IS SLOWER
  #   sim = seq_len(n_sims),
  #   Perio = map(sim, ~ MCMCpack::rdirichlet(1, alpha_perio) %>% as.numeric()),
  #   Healthy = map(sim, ~ MCMCpack::rdirichlet(1, alpha_healthy) %>% as.numeric()),
  #   Perio_tbl = map(Perio, ~ tibble(
  #     Severity = c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4"),
  #     Severity_frac = .
  #   )),
  #   Healthy_tbl = map(Healthy, ~ tibble(
  #     Severity = c("Health", "Stage_I_II"),
  #     Severity_frac = .
  #   ))
  #   )  %>% # NOTE THAT THIS PROCESS DOES A LOT OF MANIPULATION TO DATA; VECTORISING WOULD BE FASTER
  #   pivot_longer(
  #     cols = c(Perio_tbl, Healthy_tbl),
  #     names_to = "Group",
  #     values_to = "tbl"
  #   ) %>%
  #   unnest(tbl) %>%
  #   mutate(Group = if_else(Group == "Perio_tbl", "Perio", "Healthy")) %>%
  #   dplyr::select(sim, Group, Severity, Severity_frac) # drop raw vectors
  # 
  # severity_groups <- tibble(
  #   Group = c(rep("Perio", 3), rep("Healthy", 2)),
  #   Severity = c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4", "Health", "Stage_I_II")
  # )
  # 
  # sim_severity <- sim_base %>%
  #   crossing(severity_groups) %>%
  #   left_join(severity_draws, by = c("sim", "Group", "Severity")) %>%
  #   mutate(
  #     Patients = case_when(
  #       Group == "Perio" ~ Perio_pop * Severity_frac,
  #       Group == "Healthy" ~ Healthy_pop * Severity_frac
  #     )
  #   ) %>%
  #   filter(Patients > 0) %>%
  #   dplyr::select(Country, sim, Severity, Group, Patients) # drop frac, pops
  
  # ============================================================================
  # Dirichlet treatment splits within severity
  # ============================================================================
  message("Drawing Dirichlet treatment splits ...")
  
  # --- 1. Prepare labels ---
  treat_labels <- c("Untreated", "Initial", "Maintenance")
  
  # Base table (only groups with treatment splits)
  sim_tx_base <- sim_severity %>%
    filter(Group %in% tx_split$Group)
  
  n_rows <- nrow(sim_tx_base)
  n_treat <- length(treat_labels)
  
  # --- 2. Match alpha vectors per row ---
  alpha_mat <- tx_split %>%
    select(Group, Untreated, Initial, Maintenance) %>%
    column_to_rownames("Group") %>%
    as.matrix()
  
  alpha_full <- alpha_mat[sim_tx_base$Group, , drop = FALSE]  # n_rows x 3
  
  # --- 3. Draw all Dirichlet samples in one go ---
  # Generate n_rows * n_treat draws from Gamma, then normalize row sums
  gamma_draws <- matrix(rgamma(n_rows * n_treat, shape = as.vector(alpha_full)), 
                        nrow = n_rows, ncol = n_treat)
  
  dirichlet_draws <- gamma_draws / rowSums(gamma_draws)  # n_rows x 3
  colnames(dirichlet_draws) <- treat_labels
  
  # --- 4. Combine and reshape ---
  sim_treatment <- sim_tx_base %>%
    bind_cols(as_tibble(dirichlet_draws)) %>%
    mutate(
      Patients_Initial = Patients * Initial,
      Patients_Maintenance = Patients * Maintenance
    ) %>%
    select(-Untreated, -Initial, -Maintenance, -Patients) %>%
    pivot_longer(
      cols = starts_with("Patients_"),
      names_to = "Treatment",
      values_to = "Patients"
    ) %>%
    mutate(Treatment = recode(Treatment,
                              "Patients_Initial" = "Initial",
                              "Patients_Maintenance" = "Maintenance")) %>%
    filter(Patients > 0) %>%
    select(Country, sim, Severity, Treatment, Patients)
  
  #-----------------------------------------------------------------------------------
  
  # Replaced by fully vectorised dirichlet draw using gamma distribution
  
  # treatment_draws <- sim_severity %>%
  #   distinct(sim, Severity, Group) %>%
  #   left_join(tx_split, by = "Group") %>%
  #   mutate(
  #     alpha = pmap(list(Untreated, Initial, Maintenance), c),
  #     draw = map(alpha, ~ MCMCpack::rdirichlet(1, .x) %>% as.numeric()),
  #     # Only keep Initial & Maintenance fractions
  #     Treat_tbl = map(draw, ~ tibble(
  #       Treatment = c("Untreated", "Initial", "Maintenance"),
  #       Treat_frac = .x
  #     )) %>%
  #       map(~ filter(.x, Treatment != "Untreated")) # drop untreated rows to minimise memory
  #   ) %>%
  #   dplyr::select(sim, Severity, Treat_tbl) %>%
  #   unnest(Treat_tbl)
  # 
  # # Cross only Initial & Maintenance
  # sim_treatment <- sim_severity %>%
  #   crossing(Treatment = c("Initial", "Maintenance")) %>% # no Untreated rows to minimise memory
  #   left_join(treatment_draws, by = c("sim", "Severity", "Treatment")) %>%
  #   mutate(Patients = Patients * Treat_frac) %>%
  #   filter(Patients > 0) %>%
  #   dplyr::select(Country, sim, Severity, Treatment, Patients) # drop Treat_frac
  
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
  
  # --- 1. Simulate cost per unit ---
  sim_procedures <- sim_procedures %>%
    mutate(
      # fallback mean when Rate is missing or zero
      mean_cost = Shape / Rate,
      cost_per_unit = if_else(
        is.na(Rate) | Rate == 0,
        mean_cost,
        rgamma(n(), shape = Shape, rate = Rate)
      )
    )
  
  # --- 2. Simulate unit count ---
  sim_procedures <- sim_procedures %>%
    mutate(
      unit_count = if_else(
        is.na(Number_sd) | Number_sd == 0,
        Number_mean,
        abs(rnorm(n(), mean = Number_mean, sd = Number_sd))
      )
    )
  
  # --- 3. Compute total cost ---
  sim_costed <- sim_procedures %>%
    transmute(
      Total_cost = cost_per_unit * unit_count * Patients,
      Country, sim, Severity, Treatment, Procedure
    ) 
  
  #-------------------------------------------------------------------
  
  # sim_costed <- sim_procedures %>%
  #   mutate(
  #     cost_per_unit = ifelse(SD_cost == 0 | is.na(SD_cost),
  #                            Mean_cost,
  #                            rnorm(n(), Mean_cost, SD_cost) # CUBA AND ERITREA ARE NA HERE FYI
  #     ),
  #     unit_count = ifelse(Number_sd == 0 | is.na(Number_sd),
  #                         Number_mean,
  #                         rnorm(n(), Number_mean, Number_sd)
  #     ),
  #     Total_cost = cost_per_unit * unit_count * Patients
  #   ) %>%
  #   dplyr::select(Country, sim, Severity, Treatment, Procedure, Total_cost) # drop input means/sds
  
  # ============================================================================
  # Summarise costs by different slices
  # ============================================================================
  # message("Summarising costs ...")
  # results <- sim_costed %>% # THE NEGATIVE COSTS HERE SUM UP TO PRETTY BIG DECREMENTS. SHOULD CONSIDER POSITIVE RIGHT SKEW DISTS
  #   group_by(Country, sim, Severity, Treatment) %>%
  #   summarise(Cost = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
  #   group_by(Country, sim) %>%
  #   mutate(Total_cost = sum(Cost, na.rm = TRUE))
  
  # ============================================================================
  # Country-level total, perio, replacement
  # ============================================================================
  perio_procs <- c("RootDeb", "OHI", "OFD", "GTR", "Maintenance_perio")
  perio_sev <- c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4")
  replace_procs <- c("Single_implant", "Implant_surgery", "Full_fixed", "Denture", "Denture_repair")
  
  # country_total <- results %>%
  #   group_by(Country, sim) %>%
  #   summarise(Total_cost = sum(Cost, na.rm = TRUE), .groups = "drop")
  
  country_total <- sim_costed %>%
    group_by(Country, sim) %>%
    summarise(Total_cost = sum(Total_cost, na.rm = TRUE), .groups = "drop")
  
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
  
  # country_sev_combined <- sim_costed %>%
  #   group_by(Country, Severity, sim) %>%
  #   summarise(Total_cost = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
  #   group_by(Country, Severity) %>%
  #   summarise(
  #     Mean_total_billions = mean(Total_cost) / 1e9,
  #     SD_total_billions = sd(Total_cost) / 1e9
  #   )
  
  dt <- as.data.table(sim_costed)
  
  procedure_combined <- dt[, .(Procedure_cost = sum(Total_cost)), by = .(Country, Procedure, sim)
  ][, .(
    Mean_total_billions = mean(Procedure_cost) / 1e9,
    SD_total_billions   = sd(Procedure_cost) / 1e9
  ), by = .(Country, Procedure)]
  
  # procedure_combined <- sim_costed %>% 
  #   group_by(Procedure, sim) %>%
  #   summarise(Procedure_cost = sum(Total_cost, na.rm = TRUE), .groups = "drop") %>%
  #   group_by(Procedure) %>%
  #   summarise(
  #     Mean_total_billions = mean(Procedure_cost) / 1e9,
  #     SD_total_billions = sd(Procedure_cost) / 1e9,
  #     .groups = "drop"
  #   ) %>%
  #   mutate(Country = "Global")
  
  # ============================================================================
  # Write outputs
  # ============================================================================
  message("Writing output CSVs ...")
  write_csv(country_combined, file.path(output_dir, paste0("country_combined_", scenario, ".csv")))
  # write_csv(country_sev_combined, file.path(output_dir, paste0("country_sev_combined_", scenario, ".csv")))
  write_csv(procedure_combined, file.path(output_dir, paste0("procedure_combined_", scenario, ".csv")))
  
  invisible(list(
    country_combined = country_combined,
    # country_sev_combined = country_sev_combined,
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
    scenarios = c("low", "mid", "high","WHO_target"),
    slice_size = 100 # ✅ configurable, default 100
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
          procedure_lookup_mid_path = procedure_lookup_mid_path,
          procedure_lookup_low_path = procedure_lookup_low_path,
          tx_split_high_path = tx_split_high_path,
          tx_split_mid_path = tx_split_mid_path,
          tx_split_low_path = tx_split_low_path,
          severity_split_path = severity_split_path,
          scenario = sc,
          n_sims = n_sims,
          output_dir = output_dir
        )
        
        # Read slice results
        country_combined <- read_csv(file.path(output_dir, paste0("country_combined_", sc, ".csv")))
        # country_sev_combined <- read_csv(file.path(output_dir, paste0("country_sev_combined_", sc, ".csv")))
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
      # country_sev_combined_all <- bind_rows(map(slice_outputs, "country_sev_combined"))
      procedure_combined_all <- bind_rows(map(slice_outputs, "procedure_combined"))
      
      # -----------------------------------------------------------
      # Overwrite final scenario files
      # -----------------------------------------------------------
      write_csv(country_combined_all, file.path(output_dir, paste0("country_combined_", sc, ".csv")))
      # write_csv(country_sev_combined_all, file.path(output_dir, paste0("country_sev_combined_", sc, ".csv")))
      write_csv(procedure_combined_all, file.path(output_dir, paste0("procedure_combined_", sc, ".csv")))
    } else {
      # -----------------------------------------------------------
      # Fewer than slice_size — run directly, overwrite output
      # -----------------------------------------------------------
      run_cost_model(
        countries_path = countries_path,
        procedure_lookup_high_path = procedure_lookup_high_path,
        procedure_lookup_mid_path = procedure_lookup_mid_path,
        procedure_lookup_low_path = procedure_lookup_low_path,
        tx_split_high_path = tx_split_high_path,
        tx_split_mid_path = tx_split_mid_path,
        tx_split_low_path = tx_split_low_path,
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
