# ------------------------------------------------------------------------
# 1. Selection of base procedure
# ------------------------------------------------------------------------

# We will be modelling all other procedures relative to one single base procedure; this section evaluates the candidates
# for base procedures, and seeks to find the one with the lowest coefficient of variation and sd of average ratios of
# other procedures to the base procedures

library(tidyverse)

# These are the candidates for the base procedure - simple, commonly used procedures with less variance
base_candidates <- c("Extraction", "Consult_simple", "Prophy", "OPG", "PA", "Maintenance_simp")

# Drop UK as its cost structure is too idiosyncratic to extend to other countries

base_procedure_df <- read.csv("data/country_input.csv") %>%
  filter(Country != "United Kingdom") %>%
  select(-iso3)


# Get all procedure names by:
# 1. Taking all column names
# 2. Removing columns ending with '_sd'
# 3. Removing non-procedure columns (adjust if needed)

all_columns <- colnames(base_procedure_df)
procedure_names <- all_columns[!grepl("_sd$", all_columns)]
procedure_names <- setdiff(
  procedure_names,
  c("Country", "Perio_prev", "Edent_prev", "Pop", "Conversion", "Dental Expenditure Per Capita")
)

# Main loop
results <- purrr::map(base_candidates, function(base_proc) {
  base_costs <- base_procedure_df[[base_proc]]
  base_cv <- sd(base_costs) / mean(base_costs)

  others <- setdiff(procedure_names, base_proc)

  ratio_df <- base_procedure_df %>%
    dplyr::select(all_of(others)) %>%
    dplyr::mutate(across(everything(), ~ .x / base_costs))

  ratio_stats <- ratio_df %>%
    dplyr::summarise(across(everything(), list(mean_ratio = mean, sd_ratio = sd)))

  avg_sd_ratio <- ratio_stats %>%
    dplyr::select(ends_with("sd_ratio")) %>%
    unlist() %>%
    mean()

  tibble(
    base_procedure = base_proc,
    base_cv = base_cv,
    avg_sd_ratio = avg_sd_ratio
  )
})

results_df <- dplyr::bind_rows(results)

print(results_df)

# Prophy is the best candidate as it has the lowest base_cv and avg_sd_ratio


# ------------------------------------------------------------------------
# 2. Model Fitting
# ------------------------------------------------------------------------

library(rms)
library(performance)
library(see)

base_procedure_df <- read.csv("data/country_input.csv") %>%
  filter(Country != "United Kingdom")

# -------------------------------
# 1️⃣ Fit GDP → log(Prophy)
# -------------------------------

base_procedure_df <- base_procedure_df %>%
  mutate(
    log_GDP = log(GDP_per_capita_PPP_2021),
    log_prophy = log(Prophy)
  )

dd <- datadist(base_procedure_df)
options(datadist = "dd")

model_prophy <- ols(log_prophy ~ log_GDP, data = base_procedure_df)

# Diagnostics for Prophy
print(model_performance(model_prophy))
prophy_diag_plot <- plot(check_model(model_prophy)) + ggtitle("Diagnostics: log(Prophy) ~ log(GDP)")
ggsave(filename = "output_regression_diagnostics/Prophy_model_diagnostics.pdf", plot = prophy_diag_plot, width = 8, height = 6)

# -------------------------------
# 2️⃣ Fit Prophy → each Procedure (original scale)
# -------------------------------

proc_means <- c(
  "Consult_simple", "Consult_perio", "OPG", "PA", "RootDeb",
  "OHI", "Extraction", "OFD", "GTR", "Single_implant", "Implant_surgery",
  "Full_fixed", "Denture", "Denture_repair", "Maintenance_simp", "Maintenance_perio"
)

proc_models <- map(proc_means, function(proc) {
  mod <- ols(as.formula(paste0(proc, " ~ Prophy")), data = base_procedure_df)
  perf <- model_performance(mod)
  print(perf)
  diag_plot <- plot(check_model(mod)) + ggtitle(paste("Diagnostics:", proc, "~ Prophy"))
  ggsave(filename = paste0("output_regression_diagnostics/", proc, "_diagnostics.pdf"), plot = diag_plot, width = 8, height = 6)
  mod
})
names(proc_models) <- proc_means


# -------------------------------
# 3️⃣ Predict for new countries
# -------------------------------

predict_countries <- read_csv("data/2021_prevalence_pop_dentexp_GDP.csv") %>%
  mutate(log_GDP = log(GDP_per_capita_PPP_2021))

# Predict log(Prophy) + SE
log_prophy_preds <- predict(model_prophy, predict_countries, conf.int = 0.95)

predict_countries <- predict_countries %>%
  mutate(
    mu_log_prophy = log_prophy_preds$linear.predictors,
    sigma_log_prophy = (log_prophy_preds$upper - log_prophy_preds$lower) / (2 * qnorm(0.975)),
    # Log-normal back-transform
    prophy_mean = exp(mu_log_prophy + 0.5 * sigma_log_prophy^2),
    prophy_var = (exp(sigma_log_prophy^2) - 1) * exp(2 * mu_log_prophy + sigma_log_prophy^2),
    prophy_sd = sqrt(prophy_var)
  )

# -------------------------------
# 4️⃣ For each procedure: plug Prophy mean + propagate SEM
# -------------------------------

predict_proc <- function(proc_name, model, prophy_mean, prophy_var) {
  beta <- coef(model)
  beta0 <- beta[1]
  beta1 <- beta[2]
  resid_sd <- model$stats["Sigma"]

  # Predicted mean:
  pred_mean <- beta0 + beta1 * prophy_mean

  # Delta Method variance:
  pred_var <- (beta1^2) * prophy_var + resid_sd^2

  tibble(
    procedure = proc_name,
    Predicted_Cost = pred_mean,
    SD = sqrt(pred_var)
  )
}

# Do this for each procedure for each row
proc_preds <- predict_countries %>%
  mutate(procs = pmap(
    list(prophy_mean, prophy_var),
    function(pm, pv) {
      map_dfr(names(proc_models), ~ predict_proc(.x, proc_models[[.x]], pm, pv))
    }
  )) %>%
  unnest(procs)

# ------------------------------------------------------------------------
# 4. To pivot wider, so as to create the same input format for insertion to the monte carlo simulation
# ------------------------------------------------------------------------

wide_countries <- proc_preds %>%
  pivot_wider(
    names_from = procedure,
    values_from = c(Predicted_Cost, SD),
    names_glue = "{procedure}{ifelse(.value == 'SD', '_sd', '')}"
  ) %>%
  rename(Prophy = prophy_mean, Prophy_sd = prophy_sd)

# Write to CSV
write_csv(wide_countries, "./data/monte_carlo_input_from_rms.csv")

# ==============================================================================
# Optional: cleaning and combining the dataset
# ==============================================================================

base_df <- read_csv("./data/monte_carlo_input_from_rms.csv", col_types = cols())

base_df_dent_exp <- base_df %>%
  select(iso3c, Dent_exp_usd)

overwrite_df <- read_csv("./data/country_input.csv", col_types = cols()) %>%
  rename(iso3c = iso3, Dent_exppc_usd = "Dental Expenditure Per Capita") %>%
  left_join(base_df_dent_exp)

known_country_list <- overwrite_df %>%
  pull(iso3c)

final_df <- base_df %>%
  filter(!iso3c %in% known_country_list) %>%
  bind_rows(overwrite_df) %>%
  select(-mu_log_prophy, -sigma_log_prophy, -log_GDP, -prophy_var, -Conversion)

# Procedure names of interest
procedures <- c(
  "Prophy", "Consult_simple", "Consult_perio", "OPG", "PA", "RootDeb", "OHI",
  "Extraction", "OFD", "GTR", "Single_implant", "Implant_surgery",
  "Full_fixed", "Denture", "Denture_repair", "Maintenance_simp",
  "Maintenance_perio"
)

procedures_sd <- paste0(procedures, "_sd")

final_df <- final_df %>%
  mutate(across(all_of(procedures), ~ abs(.x))) %>%
  mutate(across(all_of(procedures), ~ .x * 1.21)) %>%
  mutate(across(all_of(procedures_sd), ~ .x * 1.21))
# Based on US Bureau of labor statistics, $1 in Jan 2021 has the same buying power as $1.21 in Jan 2025
# https://www.bls.gov/data/inflation_calculator.htm

# Step 2: compute *_shape and *_rate columns explicitly
for (proc in procedures) {
  mean_vec <- final_df[[proc]]
  sd_vec <- abs(final_df[[paste0(proc, "_sd")]])
  var_vec <- sd_vec^2
  denom <- ifelse(var_vec <= 0 | is.na(var_vec), NA_real_, var_vec)

  final_df[[paste0(proc, "_shape")]] <- (mean_vec^2) / denom
  final_df[[paste0(proc, "_rate")]] <- mean_vec / denom

  # safety for invalid values
  final_df[[paste0(proc, "_shape")]][!is.finite(final_df[[paste0(proc, "_shape")]]) |
    final_df[[paste0(proc, "_shape")]] <= 0] <- NA_real_
  final_df[[paste0(proc, "_rate")]][!is.finite(final_df[[paste0(proc, "_rate")]]) |
    final_df[[paste0(proc, "_rate")]] <= 0] <- NA_real_
}

write_csv(final_df, "./data/combined_country_input_for_analysis.csv") # for future data analysis

final_df_clean <- final_df %>%
  dplyr::select(
    -iso3c,
    -Dent_exp_usd,
    -Dent_exppc_usd,
    -(GDP_per_capita_PPP_2021:gdp_ppp_lower),
    -(Prophy:Maintenance_perio_sd)
  )

# Save
write_csv(final_df_clean, "./data/combined_country_input.csv")
