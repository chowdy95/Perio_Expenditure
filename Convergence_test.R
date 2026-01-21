# Testing for model convergence (2050 estimates)
library(MCMCpack)
library(tidyverse)
output_dir <- "convergence_tests"

# 2050 filepaths
countries_path <- "./data/combined_country_input_2050.csv"
procedure_lookup_high_path <- "./data/procedure_lookup.csv"
procedure_lookup_mid_path <- "./data/procedure_lookup.csv"
procedure_lookup_low_path <- "./data/procedure_lookup_low_scenario.csv"
procedure_lookup_WHO_target_path <- "./data/procedure_lookup.csv"
tx_split_high_path <- "./data/tx_split.csv"
tx_split_mid_path <- "./data/tx_split_mid_scenario.csv"
tx_split_low_path <- "./data/tx_split_low_scenario.csv"
tx_split_WHO_target_path <- "./data/tx_split_WHO_target_scenario.csv"
severity_split_path <- "./data/severity_split.csv"

# Simulations per chunk
n_sims <- 1000

# Change model to just store locally
run_cost_model <- function(
    countries_path,
    procedure_lookup_high_path,
    procedure_lookup_mid_path,
    procedure_lookup_low_path,
    tx_split_high_path,
    tx_split_mid_path,
    tx_split_low_path,
    severity_split_path,
    scenario,
    n_sims
    ) {

  countries <- read_csv(countries_path, col_types = cols())
  severity_split <- read_csv(severity_split_path, col_types = cols())
  
  procedure_lookup <- read_csv(procedure_lookup_WHO_target_path, col_types = cols())
  
  tx_split <- read_csv(tx_split_WHO_target_path, col_types = cols())
  
  procedures <- c(
    "Consult_simple", "Consult_perio", "OPG", "PA", "Prophy", "RootDeb", "OHI",
    "Extraction", "OFD", "GTR", "Single_implant", "Implant_surgery",
    "Full_fixed", "Denture", "Denture_repair", "Maintenance_simp",
    "Maintenance_perio"
  )
  
  pattern <- paste0(procedures, collapse = "|")
  
  countries_long <- countries |>
    pivot_longer(
      cols = matches(paste0(pattern, "_shape|", pattern, "_rate")),
      names_to = "Var",
      values_to = "Value"
    ) |>
    mutate(
      Metric    = if_else(str_ends(Var, "_shape"), "Shape", "Rate"),
      Procedure = str_remove(Var, "_shape$|_rate$")
    ) |>
    select(Country, Procedure, Metric, Value) |>
    pivot_wider(names_from = Metric, values_from = Value)
  
  sim_base <- crossing(
    countries,
    sim = seq_len(n_sims)
  ) |>
    transmute(
      Country, sim,
      Pop = rnorm(n(), Pop, Pop_sd),
      Perio_prev = rnorm(n(), Perio_prev, Perio_prev_sd),
      Edent_prev = rnorm(n(), Edent_prev, Edent_prev_sd),
      Perio_pop = Pop * Perio_prev,
      Healthy_pop = Pop * (1 - Perio_prev - Edent_prev)
    ) |>
    dplyr::select(-Pop, -Perio_prev, -Edent_prev) %>%
    mutate(row_id = row_number())
  
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
                        nrow = n_rows, ncol = n_sev_perio, byrow = FALSE
  )
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
                          nrow = n_rows, ncol = n_sev_healthy, byrow = FALSE
  )
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
  
  alpha_full <- alpha_mat[sim_tx_base$Group, , drop = FALSE] # n_rows x 3
  
  # --- 3. Draw all Dirichlet samples in one go ---
  # Generate n_rows * n_treat draws from Gamma, then normalize row sums
  gamma_draws <- matrix(rgamma(n_rows * n_treat, shape = as.vector(alpha_full)),
                        nrow = n_rows, ncol = n_treat
  )
  
  dirichlet_draws <- gamma_draws / rowSums(gamma_draws) # n_rows x 3
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
                              "Patients_Maintenance" = "Maintenance"
    )) %>%
    filter(Patients > 0) %>%
    select(Country, sim, Severity, Treatment, Patients)
  
 
  
  sim_procedures <- sim_treatment %>%
    inner_join(procedure_lookup, by = c("Severity", "Treatment"), relationship = "many-to-many") %>%
    left_join(countries_long, by = c("Country", "Procedure"))
  
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
  
  perio_procs <- c("RootDeb", "OHI", "OFD", "GTR", "Maintenance_perio")
  perio_sev <- c("Stage_III_IVT1_2", "Stage_IVT3", "Stage_IVT4")
  replace_procs <- c("Single_implant", "Implant_surgery", "Full_fixed", "Denture", "Denture_repair")
  
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
  
}

set.seed(160126)

library(furrr)
plan(multisession, workers = availableCores())
on.exit(plan(sequential), add = TRUE)

results <- future_map(
  1:15,
  ~ {
    output <- run_cost_model(
      countries_path = countries_path,
      procedure_lookup_high_path = procedure_lookup_high_path,
      procedure_lookup_mid_path = procedure_lookup_mid_path,
      procedure_lookup_low_path = procedure_lookup_low_path,
      tx_split_high_path = tx_split_high_path,
      tx_split_mid_path = tx_split_mid_path,
      tx_split_low_path = tx_split_low_path,
      severity_split_path = severity_split_path,
      scenario = "WHO_target",
      n_sims = .x * n_sims
      ) |>
      mutate(n_sims = .x * n_sims)
    saveRDS(output, file = paste0(output_dir, "/results_", .x*n_sims, ".rds"))
    gc()
  },
  .options = furrr_options(seed = TRUE)
)


# Test convergence
df_results <- paste0(output_dir, "/", list.files(path = output_dir, pattern = ".rds")) |>
  map_dfr(readRDS)

p <- df_results |>
  mutate(n_sims = as.numeric(n_sims)) |>
  arrange(Country, n_sims) |>
  ggplot(aes(group = Country))

p +
  geom_line(aes(x = n_sims, y = log(Mean_total_billions))) +
  theme_minimal() +
  scale_x_continuous(limits = c(1000, 12000), breaks = seq(1000, 12000, 1000))





# ==============================================================================
# Convergence diagnostics (2050) — Superregions only
# ==============================================================================

rm(list = ls())

library(tidyverse)
library(countrycode)

# ------------------------------------------------------------------------------
# 1. Load simulation outputs from RDS files
# ------------------------------------------------------------------------------

output_dir <- "convergence_tests"

df_results <- list.files(
  path = output_dir,
  pattern = "\\.rds$",
  full.names = TRUE
) %>%
  map_dfr(readRDS) %>%
  mutate(n_sims = as.numeric(n_sims))

# ------------------------------------------------------------------------------
# 2. Load GBD hierarchy
# ------------------------------------------------------------------------------

hier <- read_csv(
  "data/GBD_location_hierarchy_wide.csv",
  locale = locale(encoding = "Latin1")
) %>%
  mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>%
  select(Country, Superregion)

# ------------------------------------------------------------------------------
# 3. Aggregate directly to Superregion
# ------------------------------------------------------------------------------

superregion_results <- df_results %>%
  left_join(hier, by = "Country") %>%
  filter(!is.na(Superregion)) %>%
  group_by(Superregion, n_sims) %>%
  summarise(
    MCSE_total_billions = median(SD_total_billions/sqrt(n_sims)),
    Mean_total_billions = sum(Mean_total_billions, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 4. Plot convergence (colour by Superregion)
# ------------------------------------------------------------------------------

p2 <- ggplot(
  superregion_results,
  aes(
    x = n_sims,
    y = MCSE_total_billions,
    colour = Superregion,
    group = Superregion
  )
) +
  geom_line(linewidth = 1, alpha = 0.8) +
  theme_minimal() +
  scale_x_continuous(
    limits = c(1000, 12000),
    breaks = seq(1000, 12000, 1000)
  ) +
  labs(
    x = "Number of simulations",
    y = "Median Monte Carlo Standard Error",
    colour = "Superregion"
  ) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.7, 0.8),
        panel.grid.minor = element_blank())

p2

ggsave(
  "convergence_tests/convergence_plot_superregion.jpg",
  plot = p2,
  width = 10,
  height = 7,
  dpi = 300
)
