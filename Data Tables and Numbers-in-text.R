
#------------

# To assess countries for expenditure per capita

library(tidyverse)
library(patchwork)

perio_expenditure <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
  rename(location_name = LocationHeader) %>%
  filter(Year ==2021) %>%
  mutate(
    base_exppc = 1e9*selected_Mean_total_billions/Pop)

# Top 5 expenditure per capita

perio_expenditure_countries <- perio_expenditure %>%
  filter(Level == 3)

top_5_exppc <- perio_expenditure_countries %>%
  filter(Level == 3, Year == 2021) %>%
  arrange(desc(base_exppc)) %>%
  slice(1:5) %>%
  select(iso3c)

top_5_exppc_df <- perio_expenditure_countries %>%
  inner_join(top_5_exppc, by = "iso3c") %>%
  mutate(location_name = fct_reorder(location_name, base_exppc)) %>%
  mutate(location_name = fct_rev(location_name)) %>%
  filter (Year == 2021) %>%
  select (base_exppc, everything())


# Bottom 5 expenditure per capita

perio_expenditure_countries <- perio_expenditure %>%
  filter(Level == 3)

bottom_5_exppc <- perio_expenditure_countries %>%
  filter(Level == 3, Year == 2021) %>%
  arrange(base_exppc) %>%
  slice(1:5) %>%
  select(iso3c)

bottom_5_exppc_df <- perio_expenditure_countries %>%
  inner_join(bottom_5_exppc, by = "iso3c") %>%
  mutate(location_name = fct_reorder(location_name, base_exppc)) %>%
  mutate(location_name = fct_rev(location_name)) %>%
  filter (Year == 2021) %>%
  select (base_exppc, everything())

bottom_5_exppc_df <- perio_expenditure_countries %>%
  arrange(base_exppc) %>%
  filter (Year == 2021, base_exppc<1) %>%
  select (base_exppc, everything())


#------------

# To assess raw numbers for expenditure

perio_expenditure_wide <- read_csv("outputs_forecast/expenditure_summary_forecast_wide.csv") %>%
  rename(location_name = LocationHeader) %>%
  select(location_name, ends_with("2021") & contains ("selected"), ends_with("2050") & contains ("selected"), Region, Superregion, iso3c, Pop, selected_model)
  # mutate(
  #   gap_WHO_mean = WHO_selected_Mean_total_billions_2050 - selected_Mean_total_billions_2021,
  #   gap_base_mean = selected_Mean_total_billions_2050 - selected_Mean_total_billions_2021,
  #   gap_WHO_mean_pct = 100*gap_WHO_mean/selected_Mean_total_billions_2021,
  #   gap_base_mean_pct = 100*gap_base_mean/selected_Mean_total_billions_2021
  # )

superregion_wide <- perio_expenditure_wide %>%
  filter(is.na(Region)) %>%
  select(
    -c(
      Region, Superregion, iso3c,
      WHO_selected_Mean_total_billions_2021,
      WHO_selected_SD_total_billions_2021
    )
  ) %>%
  mutate(
    # Compute 95% CI bounds for each variable
    base_2021_upper = selected_Mean_total_billions_2021 + 1.96 * selected_SD_total_billions_2021,
    base_2021_lower = selected_Mean_total_billions_2021 - 1.96 * selected_SD_total_billions_2021,
    base_2050_upper = selected_Mean_total_billions_2050 + 1.96 * selected_SD_total_billions_2050,
    base_2050_lower = selected_Mean_total_billions_2050 - 1.96 * selected_SD_total_billions_2050,
    WHO_2050_upper  = WHO_selected_Mean_total_billions_2050 + 1.96 * WHO_selected_SD_total_billions_2050,
    WHO_2050_lower  = WHO_selected_Mean_total_billions_2050 - 1.96 * WHO_selected_SD_total_billions_2050
  ) %>%
  mutate(across(where(is.numeric), \(x) ifelse(x < 0, 0.01, x))) %>%
  
  # === Compute SEs from SDs ===
  mutate(
    se_2021 = selected_SD_total_billions_2021,
    se_base_2050 = selected_SD_total_billions_2050,
    se_WHO_2050  = WHO_selected_SD_total_billions_2050
  ) %>%
  
  # === Log-ratio (delta method) for percentage change ===
  mutate(
    # Base scenario
    ratio_base = selected_Mean_total_billions_2050 / selected_Mean_total_billions_2021,
    log_ratio_base = log(ratio_base),
    se_log_ratio_base = sqrt((se_base_2050 / selected_Mean_total_billions_2050)^2 +
                               (se_2021 / selected_Mean_total_billions_2021)^2),
    log_ratio_base_lower = log_ratio_base - 1.96 * se_log_ratio_base,
    log_ratio_base_upper = log_ratio_base + 1.96 * se_log_ratio_base,
    pctchange_base_mean = (exp(log_ratio_base) - 1) * 100,
    pctchange_base_lower = (exp(log_ratio_base_lower) - 1) * 100,
    pctchange_base_upper = (exp(log_ratio_base_upper) - 1) * 100,
    
    # WHO target scenario
    ratio_WHO = WHO_selected_Mean_total_billions_2050 / selected_Mean_total_billions_2021,
    log_ratio_WHO = log(ratio_WHO),
    se_log_ratio_WHO = sqrt((se_WHO_2050 / WHO_selected_Mean_total_billions_2050)^2 +
                              (se_2021 / selected_Mean_total_billions_2021)^2),
    log_ratio_WHO_lower = log_ratio_WHO - 1.96 * se_log_ratio_WHO,
    log_ratio_WHO_upper = log_ratio_WHO + 1.96 * se_log_ratio_WHO,
    pctchange_WHO_mean = (exp(log_ratio_WHO) - 1) * 100,
    pctchange_WHO_lower = (exp(log_ratio_WHO_lower) - 1) * 100,
    pctchange_WHO_upper = (exp(log_ratio_WHO_upper) - 1) * 100
  ) %>%
  
  # === Add components: Treatment, Rehab, Preventive (2021) ===
  mutate(
    # Means
    treat_mean_2021 = selected_Mean_perio_billions_2021,
    rehab_mean_2021 = selected_Mean_replace_billions_2021,
    prev_mean_2021  = selected_Mean_total_billions_2021 -
      selected_Mean_perio_billions_2021 -
      selected_Mean_replace_billions_2021,
    
    # SDs
    treat_sd_2021 = selected_SD_perio_billions_2021,
    rehab_sd_2021 = selected_SD_replace_billions_2021,
    prev_sd_2021 = pmax(
      sqrt(selected_SD_total_billions_2021^2 -
             selected_SD_perio_billions_2021^2 -
             selected_SD_replace_billions_2021^2),
      0.0001
    ),
    
    # 95% UI
    treat_lower_2021 = treat_mean_2021 - 1.96 * treat_sd_2021,
    treat_upper_2021 = treat_mean_2021 + 1.96 * treat_sd_2021,
    
    rehab_lower_2021 = rehab_mean_2021 - 1.96 * rehab_sd_2021,
    rehab_upper_2021 = rehab_mean_2021 + 1.96 * rehab_sd_2021,
    
    prev_lower_2021 = prev_mean_2021 - 1.96 * prev_sd_2021,
    prev_upper_2021 = prev_mean_2021 + 1.96 * prev_sd_2021
  ) %>%
  mutate(
    # Ensure everything is positive (like your earlier approach)
    across(c(prev_lower_2021, treat_lower_2021, rehab_lower_2021),
           ~ ifelse(. < 0, 0.01, .))
  ) %>%
  
  # === Add components: Treatment, Rehab, Preventive (2050 Base + WHO) ===
  mutate(
    # --- Base 2050 means ---
    treat_mean_2050_base = selected_Mean_perio_billions_2050,
    rehab_mean_2050_base = selected_Mean_replace_billions_2050,
    prev_mean_2050_base  = selected_Mean_total_billions_2050 -
      selected_Mean_perio_billions_2050 -
      selected_Mean_replace_billions_2050,
    
    # --- Base 2050 SDs ---
    treat_sd_2050_base = selected_SD_perio_billions_2050,
    rehab_sd_2050_base = selected_SD_replace_billions_2050,
    prev_sd_2050_base = pmax(
      sqrt(selected_SD_total_billions_2050^2 -
             selected_SD_perio_billions_2050^2 -
             selected_SD_replace_billions_2050^2),
      0.0001
    ),
    
    # --- Base 2050 UI ---
    treat_lower_2050_base = treat_mean_2050_base - 1.96 * treat_sd_2050_base,
    treat_upper_2050_base = treat_mean_2050_base + 1.96 * treat_sd_2050_base,
    
    rehab_lower_2050_base = rehab_mean_2050_base - 1.96 * rehab_sd_2050_base,
    rehab_upper_2050_base = rehab_mean_2050_base + 1.96 * rehab_sd_2050_base,
    
    prev_lower_2050_base = prev_mean_2050_base - 1.96 * prev_sd_2050_base,
    prev_upper_2050_base = prev_mean_2050_base + 1.96 * prev_sd_2050_base,
    
    
    # --- WHO 2050 means ---
    treat_mean_2050_who = WHO_selected_Mean_perio_billions_2050,
    rehab_mean_2050_who = WHO_selected_Mean_replace_billions_2050,
    prev_mean_2050_who  = WHO_selected_Mean_total_billions_2050 -
      WHO_selected_Mean_perio_billions_2050 -
      WHO_selected_Mean_replace_billions_2050,
    
    # --- WHO 2050 SDs ---
    treat_sd_2050_who = WHO_selected_SD_perio_billions_2050,
    rehab_sd_2050_who = WHO_selected_SD_replace_billions_2050,
    prev_sd_2050_who = pmax(
      sqrt(WHO_selected_SD_total_billions_2050^2 -
             WHO_selected_SD_perio_billions_2050^2 -
             WHO_selected_SD_replace_billions_2050^2),
      0.0001
    ),
    
    # --- WHO 2050 UI ---
    treat_lower_2050_who = treat_mean_2050_who - 1.96 * treat_sd_2050_who,
    treat_upper_2050_who = treat_mean_2050_who + 1.96 * treat_sd_2050_who,
    
    rehab_lower_2050_who = rehab_mean_2050_who - 1.96 * rehab_sd_2050_who,
    rehab_upper_2050_who = rehab_mean_2050_who + 1.96 * rehab_sd_2050_who,
    
    prev_lower_2050_who = prev_mean_2050_who - 1.96 * prev_sd_2050_who,
    prev_upper_2050_who = prev_mean_2050_who + 1.96 * prev_sd_2050_who
  ) %>%
  mutate(
    # Ensure non-negative lower bounds
    across(
      c(prev_lower_2050_base, treat_lower_2050_base, rehab_lower_2050_base,
        prev_lower_2050_who, treat_lower_2050_who, rehab_lower_2050_who),
      ~ ifelse(. < 0, 0.01, .)
    )
  ) %>%
  
  # === Round all numeric values to 2 decimals ===
  mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
  
  # === Formatting for table output ===
  mutate(
    "2021 Expenditure" = paste0(selected_Mean_total_billions_2021, " (", base_2021_lower, "-", base_2021_upper, ")"),
    "2021 Preventive Expenditure (95% UI)" =
      paste0(round(prev_mean_2021,3), " (",
             round(prev_lower_2021,3), "-",
             round(prev_upper_2021,3), ")"),
    
    "2021 Treatment Expenditure (95% UI)" =
      paste0(round(treat_mean_2021,3), " (",
             round(treat_lower_2021,3), "-",
             round(treat_upper_2021,3), ")"),
    
    "2021 Rehabilitation Expenditure (95% UI)" =
      paste0(round(rehab_mean_2021,3), " (",
             round(rehab_lower_2021,3), "-",
             round(rehab_upper_2021,3), ")"),
    "2050 Base Expenditure" = paste0(selected_Mean_total_billions_2050, " (", base_2050_lower, "-", base_2050_upper, ")"),
    "2050 WHO Expenditure" = paste0(WHO_selected_Mean_total_billions_2050, " (", WHO_2050_lower, "-", WHO_2050_upper, ")"),
    "Total % change 2021–2050 base scenario (95% CI)" =
      paste0(pctchange_base_mean, "% (",
             pctchange_base_lower, "–",
             pctchange_base_upper, "%)"),
    "Total % change 2021–2050 WHO target (95% CI)" =
      paste0(pctchange_WHO_mean, "% (",
             pctchange_WHO_lower, "–",
             pctchange_WHO_upper, "%)"),
    "2050 Base Preventive Expenditure (95% UI)" =
      paste0(prev_mean_2050_base, " (",
             prev_lower_2050_base, "-",
             prev_upper_2050_base, ")"),
    
    "2050 Base Treatment Expenditure (95% UI)" =
      paste0(treat_mean_2050_base, " (",
             treat_lower_2050_base, "-",
             treat_upper_2050_base, ")"),
    
    "2050 Base Rehabilitation Expenditure (95% UI)" =
      paste0(rehab_mean_2050_base, " (",
             rehab_lower_2050_base, "-",
             rehab_upper_2050_base, ")"),
    
    "2050 WHO Preventive Expenditure (95% UI)" =
      paste0(prev_mean_2050_who, " (",
             prev_lower_2050_who, "-",
             prev_upper_2050_who, ")"),
    
    "2050 WHO Treatment Expenditure (95% UI)" =
      paste0(treat_mean_2050_who, " (",
             treat_lower_2050_who, "-",
             treat_upper_2050_who, ")"),
    
    "2050 WHO Rehabilitation Expenditure (95% UI)" =
      paste0(rehab_mean_2050_who, " (",
             rehab_lower_2050_who, "-",
             rehab_upper_2050_who, ")")
    
  ) %>%
  select(
    location_name,
    "2021 Expenditure",
    "2021 Preventive Expenditure (95% UI)",
    "2021 Treatment Expenditure (95% UI)",
    "2021 Rehabilitation Expenditure (95% UI)",
    
    "2050 Base Expenditure",
    "Total % change 2021–2050 base scenario (95% CI)",
    
    "2050 Base Preventive Expenditure (95% UI)",
    "2050 Base Treatment Expenditure (95% UI)",
    "2050 Base Rehabilitation Expenditure (95% UI)",
    
    "2050 WHO Expenditure",
    "Total % change 2021–2050 WHO target (95% CI)",
    
    "2050 WHO Preventive Expenditure (95% UI)",
    "2050 WHO Treatment Expenditure (95% UI)",
    "2050 WHO Rehabilitation Expenditure (95% UI)"
  )

write_excel_csv(superregion_wide, "outputs_forecast/superregion_level_expenditure.csv")

country_wide <- perio_expenditure_wide %>%
  select(
    -c(
      iso3c,
      WHO_selected_Mean_total_billions_2021,
      WHO_selected_SD_total_billions_2021
    )
  ) %>%
  mutate(
    "Dental Utilisation Scenario" = selected_model
  ) %>%
  mutate(
    # Compute 95% CI bounds for each variable
    base_2021_upper = selected_Mean_total_billions_2021 + 1.96 * selected_SD_total_billions_2021,
    base_2021_lower = selected_Mean_total_billions_2021 - 1.96 * selected_SD_total_billions_2021,
    base_2050_upper = selected_Mean_total_billions_2050 + 1.96 * selected_SD_total_billions_2050,
    base_2050_lower = selected_Mean_total_billions_2050 - 1.96 * selected_SD_total_billions_2050,
    WHO_2050_upper  = WHO_selected_Mean_total_billions_2050 + 1.96 * WHO_selected_SD_total_billions_2050,
    WHO_2050_lower  = WHO_selected_Mean_total_billions_2050 - 1.96 * WHO_selected_SD_total_billions_2050
  ) %>%
  # === Add components: Treatment, Rehab, Preventive (2021) ===
  mutate(
    # Means
    treat_mean_2021 = selected_Mean_perio_billions_2021,
    rehab_mean_2021 = selected_Mean_replace_billions_2021,
    prev_mean_2021  = selected_Mean_total_billions_2021 -
      selected_Mean_perio_billions_2021 -
      selected_Mean_replace_billions_2021,
    
    # SDs
    treat_sd_2021 = selected_SD_perio_billions_2021,
    rehab_sd_2021 = selected_SD_replace_billions_2021,
    prev_sd_2021 = pmax(
      sqrt(selected_SD_total_billions_2021^2 -
             selected_SD_perio_billions_2021^2 -
             selected_SD_replace_billions_2021^2),
      0.0001
    ),
    
    # 95% UI
    treat_lower_2021 = treat_mean_2021 - 1.96 * treat_sd_2021,
    treat_upper_2021 = treat_mean_2021 + 1.96 * treat_sd_2021,
    
    rehab_lower_2021 = rehab_mean_2021 - 1.96 * rehab_sd_2021,
    rehab_upper_2021 = rehab_mean_2021 + 1.96 * rehab_sd_2021,
    
    prev_lower_2021 = prev_mean_2021 - 1.96 * prev_sd_2021,
    prev_upper_2021 = prev_mean_2021 + 1.96 * prev_sd_2021
  ) %>%
  mutate(
    # Ensure everything is positive (like your earlier approach)
    across(c(prev_lower_2021, treat_lower_2021, rehab_lower_2021),
           ~ ifelse(. < 0, 0.01, .))
  ) %>%
  
  mutate(across(where(is.numeric), \(x) ifelse(x < 0, 0.01, x))) %>%
  
  # === Compute SEs from SDs ===
  mutate(
    se_2021 = selected_SD_total_billions_2021,
    se_base_2050 = selected_SD_total_billions_2050,
    se_WHO_2050  = WHO_selected_SD_total_billions_2050
  ) %>%
  
  # === Log-ratio (delta method) for percentage change ===
  mutate(
    # Base scenario
    ratio_base = selected_Mean_total_billions_2050 / selected_Mean_total_billions_2021,
    log_ratio_base = log(ratio_base),
    se_log_ratio_base = sqrt((se_base_2050 / selected_Mean_total_billions_2050)^2 +
                               (se_2021 / selected_Mean_total_billions_2021)^2),
    log_ratio_base_lower = log_ratio_base - 1.96 * se_log_ratio_base,
    log_ratio_base_upper = log_ratio_base + 1.96 * se_log_ratio_base,
    pctchange_base_mean = (exp(log_ratio_base) - 1) * 100,
    pctchange_base_lower = (exp(log_ratio_base_lower) - 1) * 100,
    pctchange_base_upper = (exp(log_ratio_base_upper) - 1) * 100,
    
    # WHO target scenario
    ratio_WHO = WHO_selected_Mean_total_billions_2050 / selected_Mean_total_billions_2021,
    log_ratio_WHO = log(ratio_WHO),
    se_log_ratio_WHO = sqrt((se_WHO_2050 / WHO_selected_Mean_total_billions_2050)^2 +
                              (se_2021 / selected_Mean_total_billions_2021)^2),
    log_ratio_WHO_lower = log_ratio_WHO - 1.96 * se_log_ratio_WHO,
    log_ratio_WHO_upper = log_ratio_WHO + 1.96 * se_log_ratio_WHO,
    pctchange_WHO_mean = (exp(log_ratio_WHO) - 1) * 100,
    pctchange_WHO_lower = (exp(log_ratio_WHO_lower) - 1) * 100,
    pctchange_WHO_upper = (exp(log_ratio_WHO_upper) - 1) * 100
  ) %>%
  
  # Round expenditures to 3 decimals and percentages to 0 decimals
  mutate(
    across(c(selected_Mean_total_billions_2021, selected_Mean_total_billions_2050, WHO_selected_Mean_total_billions_2050,
             base_2021_lower, base_2021_upper, base_2050_lower, base_2050_upper, WHO_2050_lower, WHO_2050_upper),
           ~ round(., 3)),
    across(c(pctchange_base_mean, pctchange_base_lower, pctchange_base_upper,
             pctchange_WHO_mean, pctchange_WHO_lower, pctchange_WHO_upper),
           ~ round(., 0))
  ) %>%
  
  
  # === Formatting for table output ===
  mutate(
    "2021 Expenditure (95% UI)" = paste0(selected_Mean_total_billions_2021, " (", base_2021_lower, "-", base_2021_upper, ")"),
    "2050 Base Expenditure (95% UI)" = paste0(selected_Mean_total_billions_2050, " (", base_2050_lower, "-", base_2050_upper, ")"),
    "2050 WHO Expenditure (95% UI)" = paste0(WHO_selected_Mean_total_billions_2050, " (", WHO_2050_lower, "-", WHO_2050_upper, ")"),
    "Total % change 2021–2050 base scenario (95% UI)" =
      paste0(pctchange_base_mean, "% (",
             pctchange_base_lower, "–",
             pctchange_base_upper, "%)"),
    "Total % change 2021–2050 WHO target (95% UI)" =
      paste0(pctchange_WHO_mean, "% (",
             pctchange_WHO_lower, "–",
             pctchange_WHO_upper, "%)"),
    "2021 Preventive Expenditure (95% UI)" =
      paste0(round(prev_mean_2021,3), " (",
             round(prev_lower_2021,3), "-",
             round(prev_upper_2021,3), ")"),
    
    "2021 Treatment Expenditure (95% UI)" =
      paste0(round(treat_mean_2021,3), " (",
             round(treat_lower_2021,3), "-",
             round(treat_upper_2021,3), ")"),
    
    "2021 Rehabilitation Expenditure (95% UI)" =
      paste0(round(rehab_mean_2021,3), " (",
             round(rehab_lower_2021,3), "-",
             round(rehab_upper_2021,3), ")")
  ) %>%
  select(
    location_name,
    Region,
    Superregion,
    "2021 Expenditure (95% UI)",
    "2021 Preventive Expenditure (95% UI)",
    "2021 Treatment Expenditure (95% UI)",
    "2021 Rehabilitation Expenditure (95% UI)",
    "2050 Base Expenditure (95% UI)",
    "Total % change 2021–2050 base scenario (95% UI)",
    "2050 WHO Expenditure (95% UI)",
    "Total % change 2021–2050 WHO target (95% UI)",
    "Dental Utilisation Scenario"
  )

write_excel_csv(country_wide, "outputs_forecast/country_level_expenditure.csv")


#--------------------------------------------------------
# Median cost
#--------------------------------------------------------


perio_expenditure_countries <- perio_expenditure_wide %>%
  filter (!is.na(iso3c)) %>%
  filter(!is.na(gap_WHO_mean))

median(perio_expenditure_countries$gap_WHO_mean)
median(perio_expenditure_countries$gap_WHO_mean_pct)


#--------------------------------------------------------
# Median cost
#--------------------------------------------------------

perio_expenditure_2050 <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
  filter(Year ==2050)

perio_expenditure_2021 <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
  filter(Year ==2021)
