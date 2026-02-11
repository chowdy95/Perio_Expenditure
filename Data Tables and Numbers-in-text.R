
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
  select(location_name, ends_with("2021") & contains ("selected"), ends_with("2050") & contains ("selected"), Region, Superregion, iso3c, selected_model)
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




#-----------------------------------------------------------------------------------
# Country level estimates
#-----------------------------------------------------------------------------------

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







#-----------------------------------------------------------------------------------
# Global and super-region per capita estimates
#-----------------------------------------------------------------------------------

library(tidyverse)
library(readr)

# ------------------------------------------------------------------------------
# 1. Load datasets
# ------------------------------------------------------------------------------

perio_expenditure_wide <- read_csv("outputs_forecast/expenditure_summary_forecast_wide.csv") %>%
  rename(location_name = LocationHeader) %>%
  select(
    location_name,
    ends_with("2021") & contains("selected"),
    ends_with("2050") & contains("selected"),
    Region, Superregion, iso3c, selected_model
  )

pop_2050 <- read_csv("data/combined_country_input_2050.csv") %>%
  select(iso3c, Pop_2050 = Pop)

pop_2021 <- read_csv("data/combined_country_input.csv") %>%
  select(Country, Pop_2021 = Pop)

# ------------------------------------------------------------------------------
# 2. Merge population
# ------------------------------------------------------------------------------

perio_with_pop <- perio_expenditure_wide %>%
  left_join(pop_2050, by = "iso3c") %>%
  left_join(pop_2021, by = c("location_name" = "Country"))

# ------------------------------------------------------------------------------
# 3. Fix Global population
# ------------------------------------------------------------------------------

global_pop <- perio_with_pop %>%
  filter(!is.na(iso3c)) %>%
  summarise(
    Pop_2021 = sum(Pop_2021, na.rm = TRUE),
    Pop_2050 = sum(Pop_2050, na.rm = TRUE)
  )

perio_with_pop <- perio_with_pop %>%
  mutate(
    Pop_2021 = if_else(location_name == "Global", global_pop$Pop_2021, Pop_2021),
    Pop_2050 = if_else(location_name == "Global", global_pop$Pop_2050, Pop_2050)
  )

# ------------------------------------------------------------------------------
# 4. Fix Superregion population
# ------------------------------------------------------------------------------

superregion_pop <- perio_with_pop %>%
  filter(!is.na(iso3c)) %>%
  group_by(Superregion) %>%
  summarise(
    Pop_2021_sr = sum(Pop_2021, na.rm = TRUE),
    Pop_2050_sr = sum(Pop_2050, na.rm = TRUE),
    .groups = "drop"
  )

perio_with_pop <- perio_with_pop %>%
  left_join(superregion_pop, by = "Superregion") %>%
  mutate(
    Pop_2021 = if_else(is.na(iso3c) & is.na(Region) & !is.na(Superregion),
                       Pop_2021_sr, Pop_2021),
    Pop_2050 = if_else(is.na(iso3c) & is.na(Region) & !is.na(Superregion),
                       Pop_2050_sr, Pop_2050)
  ) %>%
  select(-Pop_2021_sr, -Pop_2050_sr)

# ------------------------------------------------------------------------------
# SUPERREGION + GLOBAL PER CAPITA TABLE
# ------------------------------------------------------------------------------

superregion_percap <- perio_with_pop %>%
  filter(is.na(Region)) %>%
  
  # ---------------------------------------------------------------------------
# Convert EVERYTHING to per-capita first
# ---------------------------------------------------------------------------
mutate(
  
  # ---- TOTAL ----
  total_pc_2021_mean = selected_Mean_total_billions_2021 * 1e9 / Pop_2021,
  total_pc_2021_sd   = selected_SD_total_billions_2021 * 1e9 / Pop_2021,
  
  total_pc_2050_base_mean = selected_Mean_total_billions_2050 * 1e9 / Pop_2050,
  total_pc_2050_base_sd   = selected_SD_total_billions_2050 * 1e9 / Pop_2050,
  
  total_pc_2050_who_mean  = WHO_selected_Mean_total_billions_2050 * 1e9 / Pop_2050,
  total_pc_2050_who_sd    = WHO_selected_SD_total_billions_2050 * 1e9 / Pop_2050,
  
  # ---- COMPONENTS 2021 ----
  treat_pc_2021_mean = selected_Mean_perio_billions_2021 * 1e9 / Pop_2021,
  treat_pc_2021_sd   = selected_SD_perio_billions_2021 * 1e9 / Pop_2021,
  
  rehab_pc_2021_mean = selected_Mean_replace_billions_2021 * 1e9 / Pop_2021,
  rehab_pc_2021_sd   = selected_SD_replace_billions_2021 * 1e9 / Pop_2021,
  
  prev_pc_2021_mean =
    total_pc_2021_mean - treat_pc_2021_mean - rehab_pc_2021_mean,
  
  prev_pc_2021_sd = pmax(
    sqrt(total_pc_2021_sd^2 - treat_pc_2021_sd^2 - rehab_pc_2021_sd^2),
    0.0001
  ),
  
  # ---- COMPONENTS 2050 BASE ----
  treat_pc_2050_base_mean = selected_Mean_perio_billions_2050 * 1e9 / Pop_2050,
  treat_pc_2050_base_sd   = selected_SD_perio_billions_2050 * 1e9 / Pop_2050,
  
  rehab_pc_2050_base_mean = selected_Mean_replace_billions_2050 * 1e9 / Pop_2050,
  rehab_pc_2050_base_sd   = selected_SD_replace_billions_2050 * 1e9 / Pop_2050,
  
  prev_pc_2050_base_mean =
    total_pc_2050_base_mean - treat_pc_2050_base_mean - rehab_pc_2050_base_mean,
  
  prev_pc_2050_base_sd = pmax(
    sqrt(total_pc_2050_base_sd^2 -
           treat_pc_2050_base_sd^2 -
           rehab_pc_2050_base_sd^2),
    0.0001
  ),
  
  # ---- COMPONENTS 2050 WHO ----
  treat_pc_2050_who_mean = WHO_selected_Mean_perio_billions_2050 * 1e9 / Pop_2050,
  treat_pc_2050_who_sd   = WHO_selected_SD_perio_billions_2050 * 1e9 / Pop_2050,
  
  rehab_pc_2050_who_mean = WHO_selected_Mean_replace_billions_2050 * 1e9 / Pop_2050,
  rehab_pc_2050_who_sd   = WHO_selected_SD_replace_billions_2050 * 1e9 / Pop_2050,
  
  prev_pc_2050_who_mean =
    total_pc_2050_who_mean - treat_pc_2050_who_mean - rehab_pc_2050_who_mean,
  
  prev_pc_2050_who_sd = pmax(
    sqrt(total_pc_2050_who_sd^2 -
           treat_pc_2050_who_sd^2 -
           rehab_pc_2050_who_sd^2),
    0.0001
  )
) %>%
  
  # ---------------------------------------------------------------------------
# 95% UI
# ---------------------------------------------------------------------------
mutate(across(ends_with("_mean"), ~ round(., 1))) %>%
  mutate(across(ends_with("_sd"), ~ round(., 1))) %>%
  
  mutate(
    
    total_2021_low  = total_pc_2021_mean - 1.96 * total_pc_2021_sd,
    total_2021_high = total_pc_2021_mean + 1.96 * total_pc_2021_sd,
    
    total_2050_base_low  = total_pc_2050_base_mean - 1.96 * total_pc_2050_base_sd,
    total_2050_base_high = total_pc_2050_base_mean + 1.96 * total_pc_2050_base_sd,
    
    total_2050_who_low  = total_pc_2050_who_mean - 1.96 * total_pc_2050_who_sd,
    total_2050_who_high = total_pc_2050_who_mean + 1.96 * total_pc_2050_who_sd
  ) %>%
  
  # ---------------------------------------------------------------------------
# % CHANGE WITH UNCERTAINTY
# ---------------------------------------------------------------------------
mutate(
  
  log_ratio_base =
    log(total_pc_2050_base_mean / total_pc_2021_mean),
  
  se_base = sqrt(
    (total_pc_2050_base_sd / total_pc_2050_base_mean)^2 +
      (total_pc_2021_sd / total_pc_2021_mean)^2
  ),
  
  pct_base_mean  = (exp(log_ratio_base) - 1) * 100,
  pct_base_low   = (exp(log_ratio_base - 1.96 * se_base) - 1) * 100,
  pct_base_high  = (exp(log_ratio_base + 1.96 * se_base) - 1) * 100,
  
  log_ratio_who =
    log(total_pc_2050_who_mean / total_pc_2021_mean),
  
  se_who = sqrt(
    (total_pc_2050_who_sd / total_pc_2050_who_mean)^2 +
      (total_pc_2021_sd / total_pc_2021_mean)^2
  ),
  
  pct_who_mean  = (exp(log_ratio_who) - 1) * 100,
  pct_who_low   = (exp(log_ratio_who - 1.96 * se_who) - 1) * 100,
  pct_who_high  = (exp(log_ratio_who + 1.96 * se_who) - 1) * 100
) %>%
  
  # ------------------------------------------------------------------------------
# 95% UI FOR ALL COMPONENTS
# ------------------------------------------------------------------------------

mutate(
  
  # ---- 2021 COMPONENTS ----
  prev_2021_low  = prev_pc_2021_mean  - 1.96 * prev_pc_2021_sd,
  prev_2021_high = prev_pc_2021_mean  + 1.96 * prev_pc_2021_sd,
  
  treat_2021_low  = treat_pc_2021_mean - 1.96 * treat_pc_2021_sd,
  treat_2021_high = treat_pc_2021_mean + 1.96 * treat_pc_2021_sd,
  
  rehab_2021_low  = rehab_pc_2021_mean - 1.96 * rehab_pc_2021_sd,
  rehab_2021_high = rehab_pc_2021_mean + 1.96 * rehab_pc_2021_sd,
  
  # ---- 2050 BASE COMPONENTS ----
  prev_2050_base_low  = prev_pc_2050_base_mean - 1.96 * prev_pc_2050_base_sd,
  prev_2050_base_high = prev_pc_2050_base_mean + 1.96 * prev_pc_2050_base_sd,
  
  treat_2050_base_low  = treat_pc_2050_base_mean - 1.96 * treat_pc_2050_base_sd,
  treat_2050_base_high = treat_pc_2050_base_mean + 1.96 * treat_pc_2050_base_sd,
  
  rehab_2050_base_low  = rehab_pc_2050_base_mean - 1.96 * rehab_pc_2050_base_sd,
  rehab_2050_base_high = rehab_pc_2050_base_mean + 1.96 * rehab_pc_2050_base_sd,
  
  # ---- 2050 WHO COMPONENTS ----
  prev_2050_who_low  = prev_pc_2050_who_mean - 1.96 * prev_pc_2050_who_sd,
  prev_2050_who_high = prev_pc_2050_who_mean + 1.96 * prev_pc_2050_who_sd,
  
  treat_2050_who_low  = treat_pc_2050_who_mean - 1.96 * treat_pc_2050_who_sd,
  treat_2050_who_high = treat_pc_2050_who_mean + 1.96 * treat_pc_2050_who_sd,
  
  rehab_2050_who_low  = rehab_pc_2050_who_mean - 1.96 * rehab_pc_2050_who_sd,
  rehab_2050_who_high = rehab_pc_2050_who_mean + 1.96 * rehab_pc_2050_who_sd
) %>%

  
  # ---------------------------------------------------------------------------
# FORMATTED TABLE
# ---------------------------------------------------------------------------
mutate(
  
  "2021 Expenditure" =
    paste0(total_pc_2021_mean, " (",
           round(total_2021_low,1), "-", round(total_2021_high,1), ")"),
  
  "2021 Preventive Expenditure (95% UI)" =
    paste0(prev_pc_2021_mean, " (",
           round(prev_2021_low,1), "-", round(prev_2021_high,1), ")"),
  
  "2021 Treatment Expenditure (95% UI)" =
    paste0(treat_pc_2021_mean, " (",
           round(treat_2021_low,1), "-", round(treat_2021_high,1), ")"),
  
  "2021 Rehabilitation Expenditure (95% UI)" =
    paste0(rehab_pc_2021_mean, " (",
           round(rehab_2021_low,1), "-", round(rehab_2021_high,1), ")"),
  
  "2050 Base Expenditure" =
    paste0(total_pc_2050_base_mean, " (",
           round(total_2050_base_low,1), "-", round(total_2050_base_high,1), ")"),
  
  "Total % change 2021–2050 base scenario (95% CI)" =
    paste0(round(pct_base_mean,1), " (",
           round(pct_base_low,1), " to ",
           round(pct_base_high,1), ")"),
  
  "2050 Base Preventive Expenditure (95% UI)" =
    paste0(prev_pc_2050_base_mean, " (",
           round(prev_2050_base_low,1), "-", round(prev_2050_base_high,1), ")"),
  
  "2050 Base Treatment Expenditure (95% UI)" =
    paste0(treat_pc_2050_base_mean, " (",
           round(treat_2050_base_low,1), "-", round(treat_2050_base_high,1), ")"),
  
  "2050 Base Rehabilitation Expenditure (95% UI)" =
    paste0(rehab_pc_2050_base_mean, " (",
           round(rehab_2050_base_low,1), "-", round(rehab_2050_base_high,1), ")"),
  
  "2050 WHO Expenditure" =
    paste0(total_pc_2050_who_mean, " (",
           round(total_2050_who_low,1), "-", round(total_2050_who_high,1), ")"),
  
  "Total % change 2021–2050 WHO target (95% CI)" =
    paste0(round(pct_who_mean,1), " (",
           round(pct_who_low,1), " to ",
           round(pct_who_high,1), ")"),
  
  "2050 WHO Preventive Expenditure (95% UI)" =
    paste0(prev_pc_2050_who_mean, " (",
           round(prev_2050_who_low,1), "-", round(prev_2050_who_high,1), ")"),
  
  "2050 WHO Treatment Expenditure (95% UI)" =
    paste0(treat_pc_2050_who_mean, " (",
           round(treat_2050_who_low,1), "-", round(treat_2050_who_high,1), ")"),
  
  "2050 WHO Rehabilitation Expenditure (95% UI)" =
    paste0(rehab_pc_2050_who_mean, " (",
           round(rehab_2050_who_low,1), "-", round(rehab_2050_who_high,1), ")")
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

# ------------------------------------------------------------------------------
# 6. Save output
# ------------------------------------------------------------------------------

write_excel_csv(
  superregion_percap,
  "outputs_forecast/superregion_level_expenditure_per_capita.csv"
)









# ------------------------------------------------------------------------------
# 1. Per capita tables
# ------------------------------------------------------------------------------




library(tidyverse)
library(readr)

# ------------------------------------------------------------------------------
# 1. Load datasets
# ------------------------------------------------------------------------------
perio_expenditure_wide <- read_csv("outputs_forecast/expenditure_summary_forecast_wide.csv") %>%
  rename(location_name = LocationHeader) %>%
  select(location_name, ends_with("2021") & contains ("selected"),
         ends_with("2050") & contains ("selected"),
         Region, Superregion, iso3c, selected_model)

pop_2050 <- read_csv("data/combined_country_input_2050.csv") %>%
  select(Country, iso3c, Pop_2050 = Pop)

pop_2021 <- read_csv("data/combined_country_input.csv") %>%
  select(Country, Pop_2021 = Pop)

# ------------------------------------------------------------------------------
# 2. Merge populations
# ------------------------------------------------------------------------------
perio_with_pop <- perio_expenditure_wide %>%
  select(-iso3c) %>%   # drop old iso3c to avoid .x/.y
  left_join(pop_2050, by = c("location_name" = "Country")) %>%
  left_join(pop_2021, by = c("location_name" = "Country"))

# ------------------------------------------------------------------------------
# 2b. Minimal patch to fix Global NA
# ------------------------------------------------------------------------------
# Sum up population for Global (only from countries)
global_pop <- perio_with_pop %>%
  filter(!is.na(iso3c)) %>%
  summarise(
    Pop_2021 = sum(Pop_2021, na.rm = TRUE),
    Pop_2050 = sum(Pop_2050, na.rm = TRUE)
  )

# Fill in Pop_2021 and Pop_2050 for Global row
perio_with_pop <- perio_with_pop %>%
  mutate(
    Pop_2021 = if_else(location_name == "Global", global_pop$Pop_2021, Pop_2021),
    Pop_2050 = if_else(location_name == "Global", global_pop$Pop_2050, Pop_2050)
  )

# ------------------------------------------------------------------------------
# 2c. Minimal patch to fix Region and Superregion population (same logic as Global)
# ------------------------------------------------------------------------------

# --- Superregion population ---
superregion_pop <- perio_with_pop %>%
  filter(!is.na(iso3c)) %>%   # countries only
  group_by(Superregion) %>%
  summarise(
    Pop_2021_sr = sum(Pop_2021, na.rm = TRUE),
    Pop_2050_sr = sum(Pop_2050, na.rm = TRUE),
    .groups = "drop"
  )

perio_with_pop <- perio_with_pop %>%
  left_join(superregion_pop, by = "Superregion") %>%
  mutate(
    Pop_2021 = if_else(is.na(iso3c) & is.na(Region) & !is.na(Superregion),
                       Pop_2021_sr, Pop_2021),
    Pop_2050 = if_else(is.na(iso3c) & is.na(Region) & !is.na(Superregion),
                       Pop_2050_sr, Pop_2050)
  ) %>%
  select(-Pop_2021_sr, -Pop_2050_sr)

# --- Region population ---
region_pop <- perio_with_pop %>%
  filter(!is.na(iso3c)) %>%   # countries only
  group_by(Region) %>%
  summarise(
    Pop_2021_r = sum(Pop_2021, na.rm = TRUE),
    Pop_2050_r = sum(Pop_2050, na.rm = TRUE),
    .groups = "drop"
  )

perio_with_pop <- perio_with_pop %>%
  left_join(region_pop, by = "Region") %>%
  mutate(
    Pop_2021 = if_else(is.na(iso3c) & !is.na(Region),
                       Pop_2021_r, Pop_2021),
    Pop_2050 = if_else(is.na(iso3c) & !is.na(Region),
                       Pop_2050_r, Pop_2050)
  ) %>%
  select(-Pop_2021_r, -Pop_2050_r)


# ------------------------------------------------------------------------------
# 3. Compute per-capita expenditures and 95% CI
# ------------------------------------------------------------------------------
perio_percapita <- perio_with_pop %>%
  mutate(
    # divide expenditures and SDs by population
    Mean_total_percap_2021 = selected_Mean_total_billions_2021 * 1e9 / Pop_2021,
    SD_total_percap_2021   = selected_SD_total_billions_2021 * 1e9 / Pop_2021,
    
    Mean_total_percap_2050 = selected_Mean_total_billions_2050 * 1e9 / Pop_2050,
    SD_total_percap_2050   = selected_SD_total_billions_2050 * 1e9 / Pop_2050,
    
    Mean_total_percap_WHO = WHO_selected_Mean_total_billions_2050 * 1e9 / Pop_2050,
    SD_total_percap_WHO   = WHO_selected_SD_total_billions_2050 * 1e9 / Pop_2050
  ) %>%
  # compute 95% UI
  mutate(
    lower_2021 = Mean_total_percap_2021 - 1.96 * SD_total_percap_2021,
    upper_2021 = Mean_total_percap_2021 + 1.96 * SD_total_percap_2021,
    
    lower_2050 = Mean_total_percap_2050 - 1.96 * SD_total_percap_2050,
    upper_2050 = Mean_total_percap_2050 + 1.96 * SD_total_percap_2050,
    
    lower_WHO = Mean_total_percap_WHO - 1.96 * SD_total_percap_WHO,
    upper_WHO = Mean_total_percap_WHO + 1.96 * SD_total_percap_WHO
  )

# ------------------------------------------------------------------------------
# 4. Add per-capita % changes
# ------------------------------------------------------------------------------
perio_percapita <- perio_percapita %>%
  mutate(
    # log-ratio method for % change
    ratio_base = Mean_total_percap_2050 / Mean_total_percap_2021,
    log_ratio_base = log(ratio_base),
    se_log_ratio_base = sqrt((SD_total_percap_2050 / Mean_total_percap_2050)^2 +
                               (SD_total_percap_2021 / Mean_total_percap_2021)^2),
    log_ratio_base_lower = log_ratio_base - 1.96 * se_log_ratio_base,
    log_ratio_base_upper = log_ratio_base + 1.96 * se_log_ratio_base,
    pctchange_base_mean = (exp(log_ratio_base) - 1) * 100,
    pctchange_base_lower = (exp(log_ratio_base_lower) - 1) * 100,
    pctchange_base_upper = (exp(log_ratio_base_upper) - 1) * 100,
    
    ratio_WHO = Mean_total_percap_WHO / Mean_total_percap_2021,
    log_ratio_WHO = log(ratio_WHO),
    se_log_ratio_WHO = sqrt((SD_total_percap_WHO / Mean_total_percap_WHO)^2 +
                              (SD_total_percap_2021 / Mean_total_percap_2021)^2),
    log_ratio_WHO_lower = log_ratio_WHO - 1.96 * se_log_ratio_WHO,
    log_ratio_WHO_upper = log_ratio_WHO + 1.96 * se_log_ratio_WHO,
    pctchange_WHO_mean = (exp(log_ratio_WHO) - 1) * 100,
    pctchange_WHO_lower = (exp(log_ratio_WHO_lower) - 1) * 100,
    pctchange_WHO_upper = (exp(log_ratio_WHO_upper) - 1) * 100
  )

# ------------------------------------------------------------------------------
# 4.5 Re-attach Global / Region / Superregion rows (ugly but safe)
# ------------------------------------------------------------------------------

# Identify aggregate rows
aggregate_rows <- perio_with_pop %>%
  filter(is.na(iso3c)) %>%     # Global, Region, Superregion
  mutate(
    Mean_total_percap_2021 = selected_Mean_total_billions_2021 * 1e9 / Pop_2021,
    Mean_total_percap_2050 = selected_Mean_total_billions_2050 * 1e9 / Pop_2050,
    Mean_total_percap_WHO  = WHO_selected_Mean_total_billions_2050 * 1e9 / Pop_2050,
    
    SD_total_percap_2021 = selected_SD_total_billions_2021 * 1e9 / Pop_2021,
    SD_total_percap_2050 = selected_SD_total_billions_2050 * 1e9 / Pop_2050,
    SD_total_percap_WHO  = WHO_selected_SD_total_billions_2050 * 1e9 / Pop_2050,
    
    lower_2021 = Mean_total_percap_2021 - 1.96 * SD_total_percap_2021,
    upper_2021 = Mean_total_percap_2021 + 1.96 * SD_total_percap_2021,
    
    lower_2050 = Mean_total_percap_2050 - 1.96 * SD_total_percap_2050,
    upper_2050 = Mean_total_percap_2050 + 1.96 * SD_total_percap_2050,
    
    lower_WHO = Mean_total_percap_WHO - 1.96 * SD_total_percap_WHO,
    upper_WHO = Mean_total_percap_WHO + 1.96 * SD_total_percap_WHO,
    
    # log-ratio % change for aggregates
    ratio_base = Mean_total_percap_2050 / Mean_total_percap_2021,
    log_ratio_base = log(ratio_base),
    se_log_ratio_base = sqrt(
      (SD_total_percap_2050 / Mean_total_percap_2050)^2 +
        (SD_total_percap_2021 / Mean_total_percap_2021)^2
    ),
    log_ratio_base_lower = log_ratio_base - 1.96 * se_log_ratio_base,
    log_ratio_base_upper = log_ratio_base + 1.96 * se_log_ratio_base,
    pctchange_base_mean  = (exp(log_ratio_base) - 1) * 100,
    pctchange_base_lower = (exp(log_ratio_base_lower) - 1) * 100,
    pctchange_base_upper = (exp(log_ratio_base_upper) - 1) * 100,
    
    ratio_WHO = Mean_total_percap_WHO / Mean_total_percap_2021,
    log_ratio_WHO = log(ratio_WHO),
    se_log_ratio_WHO = sqrt(
      (SD_total_percap_WHO / Mean_total_percap_WHO)^2 +
        (SD_total_percap_2021 / Mean_total_percap_2021)^2
    ),
    log_ratio_WHO_lower = log_ratio_WHO - 1.96 * se_log_ratio_WHO,
    log_ratio_WHO_upper = log_ratio_WHO + 1.96 * se_log_ratio_WHO,
    pctchange_WHO_mean  = (exp(log_ratio_WHO) - 1) * 100,
    pctchange_WHO_lower = (exp(log_ratio_WHO_lower) - 1) * 100,
    pctchange_WHO_upper = (exp(log_ratio_WHO_upper) - 1) * 100
    
  )

# Keep country rows only from per-capita computation
country_rows <- perio_percapita %>%
  filter(!is.na(iso3c))

# Bind back together
perio_percapita <- bind_rows(country_rows, aggregate_rows)


# ------------------------------------------------------------------------------
# 5. Format final table and add Dental Utilisation Scenario
# ------------------------------------------------------------------------------
country_wide_percap <- perio_percapita %>%
  mutate(
    "Dental Utilisation Scenario" = selected_model,
    "2021 Per-capita Expenditure (95% UI, US$)" = paste0(
      round(Mean_total_percap_2021,2), " (",
      round(lower_2021,2), "-", round(upper_2021,2), ")"
    ),
    "2050 Base Per-capita Expenditure (95% UI, US$)" = paste0(
      round(Mean_total_percap_2050,2), " (",
      round(lower_2050,2), "-", round(upper_2050,2), ")"
    ),
    "2050 WHO Per-capita Expenditure (95% UI, US$)" = paste0(
      round(Mean_total_percap_WHO,2), " (",
      round(lower_WHO,2), "-", round(upper_WHO,2), ")"
    ),
    "Total % change 2021–2050 base scenario (95% UI)" = paste0(
      round(pctchange_base_mean,0), "% (",
      round(pctchange_base_lower,0), "–",
      round(pctchange_base_upper,0), "%)"
    ),
    "Total % change 2021–2050 WHO target (95% UI)" = paste0(
      round(pctchange_WHO_mean,0), "% (",
      round(pctchange_WHO_lower,0), "–",
      round(pctchange_WHO_upper,0), "%)"
    )
  ) %>%
  select(
    location_name, Region, Superregion,
    "2021 Per-capita Expenditure (95% UI, US$)",
    "2050 Base Per-capita Expenditure (95% UI, US$)",
    "Total % change 2021–2050 base scenario (95% UI)",
    "2050 WHO Per-capita Expenditure (95% UI, US$)",
    "Total % change 2021–2050 WHO target (95% UI)",
    "Dental Utilisation Scenario"
  )

# ------------------------------------------------------------------------------
# 7. Match row order to country_level_expenditure.csv (robust to duplicate names)
# ------------------------------------------------------------------------------

ordering_ref <- read_csv("outputs_forecast/country_level_expenditure.csv") %>%
  mutate(.row_order = row_number()) %>%
  select(location_name, Region, Superregion, .row_order)

country_wide_percap <- country_wide_percap %>%
  left_join(
    ordering_ref,
    by = c("location_name", "Region", "Superregion")
  ) %>%
  arrange(.row_order) %>%
  select(-.row_order)



# ------------------------------------------------------------------------------
# 6. Save CSV
# ------------------------------------------------------------------------------
write_excel_csv(country_wide_percap, "outputs_forecast/country_level_percapita_expenditure.csv")







