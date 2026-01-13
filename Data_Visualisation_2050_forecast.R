#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------



# Plots for global, superregion and highlighted countries
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------
# Global and superregion plots
#-----------------------------------------------------------------------------------------------------------------


# Load files and data prep

library(tidyverse)
library(patchwork)
library(countrycode)

perio_expenditure <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
  rename(location_name = LocationHeader) %>%
  mutate(
    transition_WHO_selected_Mean_total_billions = if_else(Year < 2027, selected_Mean_total_billions, WHO_selected_Mean_total_billions),
    transition_WHO_selected_SD_total_billions = if_else(Year < 2027, selected_SD_total_billions, WHO_selected_SD_total_billions)
  )

#-----------------------------------------------------------------------------------------------------------------

# Assess perio expenditure changes for each superregion
superregion_cost_df <- perio_expenditure %>%
  filter(Level < 2) %>%
  mutate(location_name = fct_reorder(location_name, transition_WHO_selected_Mean_total_billions)) %>%
  mutate(location_name = fct_rev(location_name))

plot_superregion_cost <- ggplot(data = superregion_cost_df, aes(
  x = Year, y = selected_Mean_total_billions
)) +
  geom_line(aes(color = location_name, linetype = "Base, current-usage scenario"), linewidth = 1.2) +
  geom_ribbon(
    aes(
      ymin = selected_Mean_total_billions - selected_SD_total_billions,
      ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name
    ),
    alpha = 0.05
  ) +
  geom_line(aes(
    x = Year, y = transition_WHO_selected_Mean_total_billions,
    color = location_name, linetype = "WHO target scenario"
  ), linewidth = 1.2) +
  geom_ribbon(
    aes(
      ymin = transition_WHO_selected_Mean_total_billions - transition_WHO_selected_SD_total_billions,
      ymax = transition_WHO_selected_Mean_total_billions + transition_WHO_selected_SD_total_billions, fill = location_name
    ),
    alpha = 0.05
  ) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40),
        legend.position = "bottom",
        strip.text = element_text(size=10)) +
  labs(
    y = "Total Periodontitis Expenditure (billions)"
    ) +
  scale_linetype_manual(
    values = c("Base, current-usage scenario" = "solid", "WHO target scenario" = "dotted"),
    name = "Periodontitis expenditure from 2021 to 2050"
  ) +
  scale_fill_discrete(guide = "none") +
  scale_colour_discrete(guide = "none") +
  facet_wrap(~location_name, scales = "free", ncol = 2) +
  guides(
    linetype = guide_legend(
      override.aes = list(size = 1.2),     # thickness (optional)
      keywidth = unit(1.3, "cm")             # length of line in legend
    )
  )+
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +   # x-axis
  geom_vline(xintercept = 2020, 
             color = "black", linewidth = 0.6)  

plot_superregion_cost

ggsave("outputs_forecast/patchwork_global_superregion.png", width = 8, height = 11)



#-----------------------------------------------------------------------------------------------------------------
# Highlighted country plots
#-----------------------------------------------------------------------------------------------------------------

perio_expenditure_countries <- perio_expenditure %>%
  filter(Level == 3)

top_10_expenditure <- perio_expenditure_countries %>%
  filter(Level == 3, Year == 2050) %>%
  arrange(desc(transition_WHO_selected_Mean_total_billions)) %>%
  slice(1:5) %>%
  select(iso3c)

top_10_expenditure_df <- perio_expenditure %>%
  inner_join(top_10_expenditure, by = "iso3c") %>%
  mutate(location_name = fct_reorder(location_name, transition_WHO_selected_Mean_total_billions)) %>%
  mutate(location_name = fct_rev(location_name))

plot_top10_exp_cost <- ggplot(top_10_expenditure_df, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
  geom_line(data = perio_expenditure_countries, aes(x = Year, y = selected_Mean_total_billions, linetype = "Base, current-usage scenario"), alpha = 0.15) +
  geom_line(data = top_10_expenditure_df, aes(x = Year, y = selected_Mean_total_billions, color = location_name, linetype = "Base, current-usage scenario")) +
  geom_ribbon(
    aes(
      ymin = selected_Mean_total_billions - selected_SD_total_billions,
      ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name
    ),
    alpha = 0.1
  ) +
  geom_line(aes(
    x = Year, y = transition_WHO_selected_Mean_total_billions,
    color = location_name, linetype = "WHO target scenario"
  )) +
  geom_ribbon(
    aes(
      ymin = transition_WHO_selected_Mean_total_billions - transition_WHO_selected_SD_total_billions,
      ymax = transition_WHO_selected_Mean_total_billions + transition_WHO_selected_SD_total_billions, fill = location_name
    ),
    alpha = 0.05
  ) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Total Periodontitis Expenditure (millions)",
    color = "Country (top 5 for 2050 periodontitis expenditure based on WHO target) "
  ) +
  scale_linetype_manual(
    values = c("Base, current-usage scenario" = "solid", "WHO target scenario" = "dashed"),
    name = "Periodontitis expenditure from 2021 to 2050"
  ) +
  guides(fill = "none", alpha = "none")

plot_top10_exp_cost

perio_expenditure_countries <- perio_expenditure_countries %>%
  mutate(
    # Current scenario per capita in USD
    selected_exppc_usd_mean = (selected_Mean_total_billions * 1e9) / Pop,
    selected_exppc_usd_sd = sqrt(
      (selected_SD_total_billions * 1e9 / Pop)^2 +
        (selected_Mean_total_billions * 1e9 * Pop_sd / Pop^2)^2
    ),

    # WHO scenario per capita in USD
    WHO_exppc_usd_mean = (transition_WHO_selected_Mean_total_billions * 1e9) / Pop,
    WHO_exppc_usd_sd = sqrt(
      (transition_WHO_selected_SD_total_billions * 1e9 / Pop)^2 +
        (transition_WHO_selected_Mean_total_billions * 1e9 * Pop_sd / Pop^2)^2
    )
  )

write_csv(perio_expenditure_countries, "outputs_forecast/expenditure_summary_forecast_percapita.csv")

top_10_exppc <- perio_expenditure_countries %>%
  filter(Level == 3, Year == 2050) %>%
  arrange(desc(WHO_exppc_usd_mean)) %>%
  slice(1:5) %>%
  select(iso3c)

top_10_exppc_df <- perio_expenditure_countries %>%
  inner_join(top_10_exppc, by = "iso3c") %>%
  mutate(location_name = fct_reorder(location_name, WHO_exppc_usd_mean)) %>%
  mutate(location_name = fct_rev(location_name))

plot_top10_exppc_cost <- ggplot(top_10_exppc_df, aes(x = Year, y = selected_exppc_usd_mean, group = location_name)) +
  geom_line(data = perio_expenditure_countries, aes(x = Year, y = selected_exppc_usd_mean, linetype = "Base, current-usage scenario"), alpha = 0.15) +
  geom_line(data = top_10_exppc_df, aes(x = Year, y = selected_exppc_usd_mean, color = location_name, linetype = "Base, current-usage scenario")) +
  geom_ribbon(
    aes(
      ymin = selected_exppc_usd_mean - selected_exppc_usd_sd,
      ymax = selected_exppc_usd_mean + selected_exppc_usd_sd, fill = location_name
    ),
    alpha = 0.1
  ) +
  geom_line(aes(
    x = Year, y = WHO_exppc_usd_mean,
    color = location_name, linetype = "WHO target scenario"
  )) +
  geom_ribbon(
    aes(
      ymin = WHO_exppc_usd_mean - WHO_exppc_usd_sd,
      ymax = WHO_exppc_usd_mean + WHO_exppc_usd_sd, fill = location_name
    ),
    alpha = 0.05
  ) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Total Periodontitis Expenditure Per Capita (2025 USD)",
    color = "Country (top 5 for 2050 expenditure per capita, WHO target) "
  ) +
  scale_linetype_manual(
    values = c("Base, current-usage scenario" = "solid", "WHO target scenario" = "dashed"),
    name = "Periodontitis expenditure from 2021 to 2050"
  ) +
  guides(fill = "none", alpha = "none")

plot_top10_exppc_cost

plot_countries_expenditure <- plot_top10_exp_cost + plot_top10_exppc_cost + 
  plot_annotation(tag_levels = "A")  + 
  plot_layout(ncol = 1, nrow = 2) 

ggsave("outputs_forecast/patchwork_countries_expenditure.png", plot_countries_expenditure, width = 15, height = 12, dpi = 300)





#------------------------------------------------------------------------------------------
# Plotting world map with direct expenditure
#------------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(patchwork)

# Read your data
perio_expenditure <- read_csv("outputs_forecast/expenditure_summary_forecast_percapita.csv")

# Load world map & crop Antarctica
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")

# Join
map_data <- world %>%
  inner_join(perio_expenditure, by = c("iso_a3_eh" = "iso3c"), relationship = "many-to-many")

map_data_2021 <- map_data %>%
  filter(Year == 2021)

# Plot
map_base_2021 <- ggplot(map_data_2021) +
  geom_sf(aes(fill = selected_Mean_total_billions * 1000)) + # billions → millions
  scale_fill_viridis_c(
    trans = "log10",
    option = "plasma",
    begin = 0.15, # skew lighter: higher begin shifts scale lighter
    end = 1,
    direction = -1, # higher = darker
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    limits = c(1e-1, 1e5),
    labels = scales::comma_format(suffix = "M", scale = 1),
    na.value = "grey80",
    name = "Periodontal Expenditure (Millions USD)"
  ) +
  labs(
    title = "Estimated Periodontal Expenditure by Country (2021)",
    subtitle = "Countries with no data shown in grey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

map_base_2021

ggsave("outputs_forecast/world_periodontal_expenditure_map_2021.png",
       plot = map_base_2021,
       width = 24, height = 10, dpi = 300
)

#-----------------------------------------------------------------------------------------------------------------

# Plotting the global map in 2050 for the base scenario

map_data_2050 <- map_data %>%
  filter(Year == 2050)

# Plot
map_base_2050 <- ggplot(map_data_2050) +
  geom_sf(aes(fill = selected_Mean_total_billions * 1000)) + # billions → millions
  scale_fill_viridis_c(
    trans = "log10",
    option = "plasma",
    begin = 0.15, # skew lighter: higher begin shifts scale lighter
    end = 1,
    direction = -1, # higher = darker
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    limits = c(1e-1, 1e5),
    labels = scales::comma_format(suffix = "M", scale = 1),
    na.value = "grey80",
    name = "Periodontal Expenditure (Millions USD)"
  ) +
  labs(
    title = "Base Scenario: Estimated Periodontal Expenditure by Country (2050)",
    subtitle = "Countries with no data shown in grey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

map_base_2050

#-----------------------------------------------------------------------------------------------------------------

# Plotting the global map in 2050 for the WHO scenario

map_data_2050 <- map_data %>%
  filter(Year == 2050)

# Plot
map_base_2050_WHO <- ggplot(map_data_2050) +
  geom_sf(aes(fill = transition_WHO_selected_Mean_total_billions * 1000)) + # billions → millions
  scale_fill_viridis_c(
    trans = "log10",
    option = "plasma",
    begin = 0.15, # skew lighter: higher begin shifts scale lighter
    end = 1,
    direction = -1, # higher = darker
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    limits = c(1e-1, 1e5),
    labels = scales::comma_format(suffix = "M", scale = 1),
    na.value = "grey80",
    name = "Periodontal Expenditure (Millions USD)"
  ) +
  labs(
    title = "WHO Target Scenario: Estimated Periodontal Expenditure by Country (2050)",
    subtitle = "Countries with no data shown in grey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

map_base_2050_WHO

#-----------------------------------------------------------------------------------------------------------------

# Plotting the gap in dental expenditure between the current dental expenditure and WHO target in 2050

perio_expenditure_wide <- read_csv("outputs_forecast/expenditure_summary_forecast_wide.csv") %>%
  rename(location_name = LocationHeader) %>%
  mutate(
    gap_WHO_mean = WHO_selected_Mean_total_billions_2050 - selected_Mean_total_billions_2021
  )

# Load world map & crop Antarctica
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")

# Join
map_data_gap <- world %>%
  inner_join(perio_expenditure_wide, by = c("iso_a3_eh" = "iso3c"), relationship = "many-to-many")

# Plot
map_base_gap <- ggplot(map_data_gap) +
  geom_sf(aes(fill = gap_WHO_mean * 1000)) + # billions → millions
  scale_fill_viridis_c(
    trans = "log10",
    option = "plasma",
    begin = 0.15, # skew lighter: higher begin shifts scale lighter
    end = 1,
    direction = -1, # higher = darker
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    limits = c(1e-1, 1e5),
    labels = scales::comma_format(suffix = "M", scale = 1),
    na.value = "grey80",
    name = "Periodontal Expenditure (Millions USD)"
  ) +
  labs(
    title = "Gap between 2021 expenditure and 2050 WHO target",
    subtitle = "Countries with decrease or no change in grey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

map_base_gap


#-----------------------------------------------------------------------------------------------------------------

# Save with suitable dimensions for world map


plot_patchwork_map <- (map_base_2050 + map_base_2050_WHO + map_base_gap) +
  plot_layout(ncol = 1, nrow = 3) +
  plot_annotation(tag_levels = "A")

plot_patchwork_map

ggsave("outputs_forecast/world_periodontal_expenditure_map_2021_2025.png",
  plot = plot_patchwork_map,
  width = 18, height = 20, dpi = 300
)






# #-----------------------------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------------------------
# # Assessment of prevalent counts around the world over time
# #-----------------------------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------------------------
#
# # Load files and data prep
#
# library(tidyverse)
# library(patchwork)
# library(countrycode)
#
# perio_prevalence_counts <- read_csv("data/GBD_prev_perio_count.csv") %>%
#   select(location_id, location_name, val, upper, lower) %>%
#   rename("mean" = "val") %>%
#   mutate (year_id = 2021)
#
# hier <- read_csv("data/GBD_location_hierarchy.csv", locale = locale(encoding = "Latin1")) %>%
#   select("Location ID", "Location Name", "Level", "Sort Order") %>%
#   rename(location_id = "Location ID", location_name = "Location Name") %>%
#   slice(1:233) # all groupings after this are non-GBD hierarchy groupings
#
# perio_prevalence_counts_forecast <- read_csv("data/GBD_prev_perio_count_forecast.csv") %>%
#   filter(sex == "Both", age_group_name == "All Ages") %>%
#   filter(year_id %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
#   select(-sex, -age_group_name) %>%
#   bind_rows(perio_prevalence_counts) %>%
#   rename("count" = "mean", "Year" = "year_id") %>%
#   left_join(hier) %>%
#   mutate(count = count /1000000) %>%
#   mutate(location_name = fct_reorder(location_name, count)) %>%
#   mutate(location_name = fct_rev(location_name)) %>%
#   mutate(upper = upper / 1000000) %>%
#   mutate(lower = lower / 1000000)
#
# write_csv(perio_prevalence_counts_forecast, "outputs_forecast/perio_prevalence_count_2021_2050.csv")
#
# perio_prevalence_counts_forecast_countries <- perio_prevalence_counts_forecast %>%
#   filter (Level ==3)
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Assess prevalence count changes for each superregion
#
# superregion_df <- perio_prevalence_counts_forecast %>%
#   filter(Level == 1)
#
# plot_superregion_pop <- ggplot(data = superregion_df, aes(x = Year, y = count, group = location_name)) +
#   geom_line(aes(color = location_name))+
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Prevalence counts (millions)",
#     color = "GBD Superregions (in descending order)"
#   ) +
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper, fill = location_name),
#               alpha = 0.1) +
#   scale_fill_discrete(guide = "none")
#
# # ggsave("outputs_forecast/population_by_superegion.pdf", plot_superregion_pop, width = 15, height = 8, dpi = 300)
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Assess prevalence count changes globally
# global_df <- perio_prevalence_counts_forecast %>%
#   filter(Level == 0)
#
# plot_global_pop <- ggplot(data = global_df, aes(x = Year, y = count, group = location_name)) +
#   geom_line(aes(color = location_name))+
#   scale_y_continuous(limits = c(0,NA)) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Prevalence counts (millions)",
#     color = "Global"
#   ) +
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper, fill = location_name),
#               alpha = 0.1) +
#   scale_fill_discrete(guide = "none")
#
# # ggsave("outputs_forecast/population_by_global.pdf", plot_global_pop, width = 15, height = 8, dpi = 300)
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Select top 5 countries by current perio expenditure to assess population changes
#
# country_totals <- read_csv("outputs/short_final_selected_output.csv") %>%
#   filter(Level == 3) %>%
#   arrange(desc(selected_Mean_total_billions)) %>%
#   slice(1:5) %>%
#   select(iso3c)
#
# top_5_count_df <- perio_prevalence_counts_forecast %>%
#   filter(Level == 3) %>%
#   mutate(iso3c = countrycode(location_name, origin = "country.name", destination = "iso3c"))%>%
#   inner_join(country_totals, by = "iso3c")
#
# plot_top5_exp <- ggplot(top_5_count_df, aes(x = Year, y = count, group = location_name)) +
#   geom_line (data = perio_prevalence_counts_forecast_countries, aes(x = Year, y = count), alpha = 0.1) +
#   geom_line(aes(color = location_name))+
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Prevalence counts (millions)",
#     color = "Country (top 5 in periodontitis expenditure) "
#   )  +
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper, fill = location_name),
#               alpha = 0.1) +
#   guides(fill = "none", alpha = "none")
#
# # top_20_count_df_ex_China <- top_20_count_df %>%
# #   filter (!location_name == "China")
# #
# # plot_top20_exp_ex_China <- ggplot(top_20_count_df_ex_China, aes(x = Year, y = count, group = location_name)) +
# #   geom_line(aes(color = location_name))+
# #   geom_point() +
# #   theme_minimal() +
# #   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
# #   labs(
# #     y = "Prevalence counts (millions)",
# #     color = "Country (top 20 in periodontitis expenditure)"
# #   )
#
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Select top 5 countries by 2050 periodontitis counts to assess population changes
#
# top_5_perio_list <- perio_prevalence_counts_forecast %>%
#   filter(Level == 3) %>%
#   filter(Year == 2021) %>%
#   slice_max (count, n = 5)%>%
#   pull(location_name)
#
# top_5_perio_df <- perio_prevalence_counts_forecast %>%
#   filter(location_name %in% top_5_perio_list) %>%
#   filter(Level == 3)
#
# plot_top5_pop <- ggplot(top_5_perio_df, aes(x = Year, y = count, group = location_name)) +
#   geom_line (data = perio_prevalence_counts_forecast_countries, aes(x = Year, y = count), alpha = 0.1) +
#   geom_line(aes(color = location_name))+
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Prevalence counts (millions)",
#     color = "Country (top 5 in periodontitis prevalence counts by 2050)"
#   ) +
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper, fill = location_name),
#               alpha = 0.1)   +
#   guides(fill = "none", alpha = "none")
#
# #Select top 20 countries by perio case counts to assess population changes excluding India and China
# #
# # top_20_perio_df_ex_India_China <- top_20_perio_df %>%
# #   filter(!location_name == "India") %>%
# #   filter(!location_name == "China")
# #
# # plot_top20_pop_ex_India_China <- ggplot(top_20_perio_df_ex_India_China, aes(x = Year, y = count, group = location_name)) +
# #   geom_line(aes(color = location_name))+
# #   geom_point() +
# #   theme_minimal() +
# #   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
# #   labs(
# #     y = "Prevalence counts (millions)",
# #     color = "Country (top 20 in periodontitis prevalence counts) ex China and India"
# #   )
#
# # plot_patchwork_pop <- (plot_global_pop + plot_superregion_pop + plot_top5_exp + plot_top5_pop) +
# #   plot_layout(ncol = 2, nrow = 3)
# #
# # ggsave("outputs_forecast/patchwork_population.pdf", plot_patchwork_pop, width = 25, height = 20, dpi = 300)
#
#
# #-----------------------------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------------------------
# # Assessment of expenditure around the world over time (parallel to before)
# #-----------------------------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------------------------
#
# # Load files and data prep
#
# library(tidyverse)
# library(patchwork)
# library(countrycode)
#
# perio_expenditure <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
#   rename(location_name = LocationHeader)
#
# perio_expenditure_countries <- perio_expenditure %>%
#   filter (Level ==3)
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Assess perio expenditure changes for each superregion
# superregion_cost_df <- perio_expenditure %>%
#   filter(Level == 1) %>%
#   mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
#   mutate(location_name = fct_rev(location_name))
#
# plot_superregion_cost <- ggplot(data = superregion_cost_df, aes(x = Year, y = selected_Mean_total_billions,
#                                                                 group = location_name)) +
#   geom_line(aes(color = location_name))+
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
#               alpha = 0.1) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Total Periodontitis Expenditure (billions)",
#     color = "GBD Superregions (in descending order)"
#   ) +
#   scale_fill_discrete(guide = "none")
#
# # plot_superregion_cost
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Assess perio expenditure changes globally
# global_cost_df <- perio_expenditure %>%
#   filter(Level == 0)
#
# plot_global_cost <- ggplot(data = global_cost_df, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
#   geom_line(aes(color = location_name))+
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
#               alpha = 0.1) +
#   scale_y_continuous(limits = c(0,NA)) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Total Periodontitis Expenditure (millions)",
#     color = "Global"
#   ) +
#   scale_fill_discrete(guide = "none")
#
# # plot_global_cost
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Select top 5 countries by current perio expenditure
#
# perio_expenditure_wide <- read_csv("outputs_forecast/expenditure_summary_forecast_wide.csv") %>%
#   filter(Level ==3) %>%
#   arrange(desc(selected_Mean_total_billions_2021)) %>%
#   slice(1:5) %>%
#   select(iso3c)
#
# top_5_expenditure_df <- perio_expenditure %>%
#   inner_join(perio_expenditure_wide, by = "iso3c") %>%
#   mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
#   mutate(location_name = fct_rev(location_name))
#
# plot_top5_exp_cost <- ggplot(top_5_expenditure_df, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
#   geom_line (data = perio_expenditure_countries, aes(x = Year, y = selected_Mean_total_billions), alpha = 0.1) +
#   geom_line(aes(color = location_name))+
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
#               alpha = 0.1) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Total Periodontitis Expenditure (millions)",
#     color = "Country (top 5 in 2025 periodontitis expenditure) "
#   ) +
#   guides(fill = "none", alpha = "none")
#
# # Select top 20 countries by 2025 excluding China and USA
# #
# # top_20_expenditure_df_ex_USA_China <- top_20_expenditure_df %>%
# #   filter(!iso3c %in% c("USA","CHN"))
# #
# # plot_top20_exp_ex_USA_China <- ggplot(top_20_expenditure_df_ex_USA_China, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
# #   geom_line(aes(color = location_name))+
# #   geom_point() +
# #   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
# #                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
# #               alpha = 0.1) +
# #   theme_minimal() +
# #   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
# #   labs(
# #     y = "Total Periodontitis Expenditure (millions)",
# #     color = "Country (top 5 in 2025 periodontitis expenditure) ex USA, CHN "
# #   ) +
# #   scale_fill_discrete(guide = "none")
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Select top 5 countries by 2050 periodontitis counts to assess expenditure changes
#
# perio_prevalence_counts_forecast <- read_csv("outputs_forecast/perio_prevalence_count_2021_2050.csv")
#
# top_5_perio_list <- perio_prevalence_counts_forecast %>%
#   filter(Level == 3) %>%
#   filter(Year == 2021) %>%
#   slice_max (count, n = 5)%>%
#   pull(location_name)
#
# top_5_count_df_joined <- perio_expenditure %>%
#   filter(location_name %in% top_5_perio_list) %>%
#   mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
#   mutate(location_name = fct_rev(location_name))
#
#
# plot_top5_pop_cost <- ggplot(top_5_count_df_joined, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
#   geom_line (data = perio_expenditure_countries, aes(x = Year, y = selected_Mean_total_billions), alpha = 0.1) +
#   geom_line(aes(color = location_name))+
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
#               alpha = 0.1) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Total Periodontitis Expenditure (millions)",
#     color = "Country (top 5 in periodontitis prevalence counts by 2050)"
#   ) +
#   guides(fill = "none", alpha = "none")
#
#
# #Select top 20 countries by perio case counts to assess population changes excluding USA and China
# #
# # top_20_count_df_joined_USA_China <- top_20_count_df_joined %>%
# #   filter(!iso3c %in% c("USA","CHN"))
# #
# # plot_top20_pop_ex_USA_China <- ggplot(top_20_count_df_joined_USA_China, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
# #   geom_line(aes(color = location_name))+
# #   geom_point() +
# #   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
# #                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
# #               alpha = 0.1) +
# #   theme_minimal() +
# #   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
# #   labs(
# #     y = "Prevalence counts (millions)",
# #     color = "Country (top 5 in periodontitis prevalence counts) ex USA, CHN"
# #   ) +
# #   scale_fill_discrete(guide = "none")
#
# plot_patchwork <- (plot_global_cost +plot_global_pop +
#                      plot_superregion_cost + plot_superregion_pop +
#                      plot_top5_exp_cost + plot_top5_exp +
#                      plot_top5_pop_cost + plot_top5_pop
# ) +
#   plot_layout(ncol = 2, nrow = 4)
#
# ggsave("outputs_forecast/patchwork_expenditure_summary.pdf", plot_patchwork, width = 25, height = 25, dpi = 300)
#
# # plot_patchwork_exp <- (plot_global_cost + plot_superregion_cost + plot_top20_exp + plot_top20_exp_ex_USA_China + plot_top20_pop + plot_top20_pop_ex_USA_China) +
# #   plot_layout(ncol = 2, nrow = 3)
# #
# # ggsave("outputs_forecast/patchwork_expenditure.pdf", plot_patchwork_exp, width = 25, height = 20, dpi = 300)
#
#
#
#
#
#
# #-----------------------------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------------------------
# # Assessment of changes in dental utilisation
# #-----------------------------------------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------------------------------------
#
# # Load files and data prep
#
# library(tidyverse)
# library(patchwork)
#
# base_scenario <- read_csv("outputs_forecast/base_scenario.csv") %>%
#   rename(location_name = Country) %>%
#   mutate(Scenario = "Base")
#
# mid_scenario <- read_csv("outputs_forecast/mid_scenario.csv") %>%
#   rename(location_name = Country) %>%
#   mutate(Scenario = "Mid")
#
# high_scenario <- read_csv("outputs_forecast/high_scenario.csv") %>%
#   rename(location_name = Country) %>%
#   mutate(Scenario = "High")
#
# full_scenarios <- bind_rows(base_scenario, mid_scenario, high_scenario) %>%
#   mutate(Scenario = fct_reorder(Scenario, selected_Mean_total_billions)) %>%
#   mutate(Scenario = fct_rev(Scenario)) %>%
#   mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
#   mutate(location_name = fct_rev(location_name))
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Assess perio expenditure changes globally for each dental utilisation scenario
#
# # Assess perio expenditure changes globally
# global_full_scenarios <- full_scenarios %>%
#   filter(Level == 0)
#
# global_base_scenario <- global_full_scenarios %>%
#   filter(Scenario == "Base")
#
# plot_global_cost_scenarios <- ggplot(data = global_full_scenarios, aes(x = Year, y = selected_Mean_total_billions,
#                                                       group = interaction(location_name, Scenario))
#                                      )+
#   geom_line(aes(color = location_name, linetype = Scenario))+
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
#               alpha = 0.05) +
#   geom_point(data = global_base_scenario, aes(x = Year, y = selected_Mean_total_billions, color = location_name), alpha = 0.5, size = 1.3) +
#   scale_y_continuous(limits = c(0,NA)) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Total Periodontitis Expenditure (millions)",
#     color = "Global",
#     linetype = "Scenarios (datapoints representing current dental utilisation)"
#   ) +
#   guides(fill = "none")
#
# # plot_global_cost_scenarios
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Assess perio expenditure changes for each superregion
#
# superregion_cost_df_scenario <- full_scenarios %>%
#   filter(Level == 1) %>%
#   filter(!location_name == "High-income")
#
# superregion_current_df <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
#   rename(location_name = LocationHeader) %>%
#   filter(Level == 1) %>%
#   filter(!location_name == "High-income") %>%
#   mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
#   mutate(location_name = fct_rev(location_name)) %>%
#   mutate(Scenario = "base")
#
# plot_superregion_cost_scenarios <- ggplot(data = superregion_cost_df_scenario, aes(x = Year, y = selected_Mean_total_billions,
#                                                                 group = interaction(location_name, Scenario))) +
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
#               alpha = 0.03) +
#   geom_line(aes(color = location_name, linetype = Scenario), alpha = 0.5) +
#   geom_point(data = superregion_current_df, aes(x = Year, y = selected_Mean_total_billions, color = location_name), alpha = 0.5, size = 1.3) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Total Periodontitis Expenditure (billions)",
#     color = "GBD Superregions (excluding High-Income)",
#     linetype = "Scenarios (datapoints representing current dental utilisation)"
#   ) +
#   guides(fill = "none")
#
# # plot_superregion_cost_scenarios
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Select top 5 countries by 2050 periodontitis counts to assess expenditure changes
#
# perio_prevalence_counts_forecast <- read_csv("outputs_forecast/perio_prevalence_count_2021_2050.csv")
#
# top_5_perio_list <- perio_prevalence_counts_forecast %>%
#   filter(Level == 3) %>%
#   filter(Year == 2021) %>%
#   slice_max (count, n = 5)%>%
#   pull(location_name)
#
# top_5_count_df_joined_scenarios <- full_scenarios %>%
#   filter(location_name %in% top_5_perio_list)
#
# perio_expenditure <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
#   rename(location_name = LocationHeader)
#
# top_5_count_df_joined <- perio_expenditure %>%
#   filter(location_name %in% top_5_perio_list) %>%
#   mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
#   mutate(location_name = fct_rev(location_name)) %>%
#   mutate(Scenario = "base")
#
# plot_top5_pop_cost_scenarios <- ggplot(top_5_count_df_joined_scenarios, aes(x = Year, y = selected_Mean_total_billions,
#                                                                   group = interaction(location_name, Scenario))) +
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions,
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name),
#               alpha = 0.03) +
#   geom_line(aes(color = location_name, linetype = Scenario), alpha = 0.5) +
#   geom_point(data = top_5_count_df_joined, aes(x = Year, y = selected_Mean_total_billions, color = location_name), alpha = 0.5, size = 1.3) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Total Periodontitis Expenditure (millions)",
#     color = "Country (top 5 in periodontitis prevalence counts by 2050)",
#     linetype = "Scenarios (datapoints representing current dental utilisation)"
#   ) +
#   guides(fill = "none", alpha = "none")
#
# # plot_top5_pop_cost_scenarios
#
# plot_patchwork_scenarios <- (plot_global_cost_scenarios + plot_superregion_cost_scenarios + plot_top5_pop_cost_scenarios) +
#   plot_layout(ncol = 1, nrow = 3)
#
# ggsave("outputs_forecast/patchwork_expenditure_scenarios.pdf", plot_patchwork_scenarios, width = 14, height = 20, dpi = 300)
#
#
#
#
# #------------------------------------------------------------------------------------------
# # Plotting world map with direct expenditure
# #------------------------------------------------------------------------------------------
#
# # Load libraries
# library(tidyverse)
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(countrycode)
# library(patchwork)
#
# # Read your data
# perio_expenditure <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
#   rename(location_name = LocationHeader)
#
# # Load world map & crop Antarctica
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
#   filter(admin != "Antarctica")
#
# # Join
# map_data <- world %>%
#   inner_join(perio_expenditure, by = c("iso_a3_eh" = "iso3c"))
#
# map_data_2021 <- map_data %>%
#   filter(Year == 2021)
#
# # Plot
# map_base_2021 <- ggplot(map_data_2021) +
#   geom_sf(aes(fill = selected_Mean_total_billions * 1000)) + # billions → millions
#   scale_fill_viridis_c(
#     trans = "log10",
#     option = "plasma",
#     begin = 0.15, # skew lighter: higher begin shifts scale lighter
#     end = 1,
#     direction = -1, # higher = darker
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     limits = c(1e-1, 1e5),
#     labels = scales::comma_format(suffix = "M", scale = 1),
#     na.value = "grey80",
#     name = "Periodontal Expenditure (Millions USD)"
#   ) +
#   labs(
#     title = "Estimated Periodontal Expenditure by Country (2021)",
#     subtitle = "Countries with no data shown in grey"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right"
#   )
#
# # map_base_2021
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Plotting the global map in 2050 for the base scenario
#
# map_data_2050 <- map_data %>%
#   filter(Year == 2050)
#
# # Plot
# map_base_2050 <- ggplot(map_data_2050) +
#   geom_sf(aes(fill = selected_Mean_total_billions * 1000)) + # billions → millions
#   scale_fill_viridis_c(
#     trans = "log10",
#     option = "plasma",
#     begin = 0.15, # skew lighter: higher begin shifts scale lighter
#     end = 1,
#     direction = -1, # higher = darker
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     limits = c(1e-1, 1e5),
#     labels = scales::comma_format(suffix = "M", scale = 1),
#     na.value = "grey80",
#     name = "Periodontal Expenditure (Millions USD)"
#   ) +
#   labs(
#     title = "Base Scenario: Estimated Periodontal Expenditure by Country (2050)",
#     subtitle = "Countries with no data shown in grey"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right"
#   )
#
# # map_base_2050
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Plotting the global map in 2050 for the mid scenario
#
# mid_scenario <- read_csv("outputs_forecast/mid_scenario.csv")
#
# # Load world map & crop Antarctica
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
#   filter(admin != "Antarctica")
#
# # Join
# map_data_mid <- world %>%
#   inner_join(mid_scenario, by = c("iso_a3_eh" = "iso3c"))
#
# map_data_2050_mid <- map_data_mid %>%
#   filter(Year == 2050)
#
# # Plot
# map_mid_2050 <- ggplot(map_data_2050_mid) +
#   geom_sf(aes(fill = selected_Mean_total_billions * 1000)) + # billions → millions
#   scale_fill_viridis_c(
#     trans = "log10",
#     option = "plasma",
#     begin = 0.15, # skew lighter: higher begin shifts scale lighter
#     end = 1,
#     direction = -1, # higher = darker
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     limits = c(1e-1, 1e5),
#     labels = scales::comma_format(suffix = "M", scale = 1),
#     na.value = "grey80",
#     name = "Periodontal Expenditure (Millions USD)"
#   ) +
#   labs(
#     title = "Mid Scenario: Estimated Periodontal Expenditure by Country (2050)",
#     subtitle = "Countries with no data shown in grey"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right"
#   )
#
# # map_mid_2050
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Plotting the global map in 2050 for the high scenario
#
# high_scenario <- read_csv("outputs_forecast/high_scenario.csv")
#
# # Load world map & crop Antarctica
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
#   filter(admin != "Antarctica")
#
# # Join
# map_data_high <- world %>%
#   inner_join(high_scenario, by = c("iso_a3_eh" = "iso3c"))
#
# map_data_2050_high <- map_data_high %>%
#   filter(Year == 2050)
#
# # Plot
# map_high_2050 <- ggplot(map_data_2050_high) +
#   geom_sf(aes(fill = selected_Mean_total_billions * 1000)) + # billions → millions
#   scale_fill_viridis_c(
#     trans = "log10",
#     option = "plasma",
#     begin = 0.15, # skew lighter: higher begin shifts scale lighter
#     end = 1,
#     direction = -1, # higher = darker
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     limits = c(1e-1, 1e5),
#     labels = scales::comma_format(suffix = "M", scale = 1),
#     na.value = "grey80",
#     name = "Periodontal Expenditure (Millions USD)"
#   ) +
#   labs(
#     title = "High Scenario: Estimated Periodontal Expenditure by Country (2050)",
#     subtitle = "Countries with no data shown in grey"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right"
#   )
#
# # map_high_2050
#
# #-----------------------------------------------------------------------------------------------------------------
#
# # Save with suitable dimensions for world map
#
#
# plot_patchwork_map <- (map_base_2021 + map_base_2050 + map_mid_2050 + map_high_2050) +
#   plot_layout(ncol = 2, nrow = 2)
#
# plot_patchwork_map
#
# ggsave("outputs_forecast/world_periodontal_expenditure_map_2021_2025.pdf",
#        plot = plot_patchwork_map,
#        width = 24, height = 10, dpi = 300
# )
