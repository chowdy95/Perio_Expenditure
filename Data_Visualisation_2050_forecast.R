#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
# Assessment of prevalent counts around the world over time
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------

# Load files and data prep

library(tidyverse)
library(patchwork)
library(countrycode)

perio_prevalence_counts <- read_csv("data/GBD_prev_perio_count.csv") %>%
  select(location_id, location_name, val, upper, lower) %>%
  rename("mean" = "val") %>%
  mutate (year_id = 2021)

hier <- read_csv("data/GBD_location_hierarchy.csv", locale = locale(encoding = "Latin1")) %>%
  select("Location ID", "Location Name", "Level", "Sort Order") %>%
  rename(location_id = "Location ID", location_name = "Location Name") %>%
  slice(1:233) # all groupings after this are non-GBD hierarchy groupings

perio_prevalence_counts_forecast <- read_csv("data/GBD_prev_perio_count_forecast.csv") %>%
  filter(sex == "Both", age_group_name == "All Ages") %>%
  filter(year_id %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  select(-sex, -age_group_name) %>%
  bind_rows(perio_prevalence_counts) %>%
  rename("count" = "mean", "Year" = "year_id") %>%
  left_join(hier) %>%
  mutate(count = count /1000000) %>%
  mutate(location_name = fct_reorder(location_name, count)) %>%
  mutate(location_name = fct_rev(location_name)) %>%
  mutate(upper = upper / 1000000) %>%
  mutate(lower = lower / 1000000)

write_csv(perio_prevalence_counts_forecast, "outputs_forecast/perio_prevalence_count_2021_2050")

perio_prevalence_counts_forecast_countries <- perio_prevalence_counts_forecast %>%
  filter (Level ==3)

#-----------------------------------------------------------------------------------------------------------------

# Assess prevalence count changes for each superregion

superregion_df <- perio_prevalence_counts_forecast %>%
  filter(Level == 1) 

plot_superregion_pop <- ggplot(data = superregion_df, aes(x = Year, y = count, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "GBD Superregions (in descending order)"
  ) +
  geom_ribbon(aes(ymin = lower, 
                  ymax = upper, fill = location_name), 
              alpha = 0.1) +
  scale_fill_discrete(guide = "none") 

plot_superregion_pop

# ggsave("outputs_forecast/population_by_superegion.pdf", plot_superregion_pop, width = 15, height = 8, dpi = 300)

#-----------------------------------------------------------------------------------------------------------------

# Assess prevalence count changes globally
global_df <- perio_prevalence_counts_forecast %>%
  filter(Level == 0)

plot_global_pop <- ggplot(data = global_df, aes(x = Year, y = count, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  scale_y_continuous(limits = c(0,NA)) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "Global"
  ) +
  geom_ribbon(aes(ymin = lower, 
                  ymax = upper, fill = location_name), 
              alpha = 0.1) +
  scale_fill_discrete(guide = "none")

# ggsave("outputs_forecast/population_by_global.pdf", plot_global_pop, width = 15, height = 8, dpi = 300)

#-----------------------------------------------------------------------------------------------------------------

# Select top 5 countries by current perio expenditure to assess population changes

country_totals <- read_csv("outputs/short_final_selected_output.csv") %>%
  filter(Level == 3) %>%
  arrange(desc(selected_Mean_total_billions)) %>%
  slice(1:5) %>%
  select(iso3c)

top_5_count_df <- perio_prevalence_counts_forecast %>%
  filter(Level == 3) %>%
  mutate(iso3c = countrycode(location_name, origin = "country.name", destination = "iso3c"))%>%
  inner_join(country_totals, by = "iso3c")

plot_top5_exp <- ggplot(top_5_count_df, aes(x = Year, y = count, group = location_name)) +
  geom_line (data = perio_prevalence_counts_forecast_countries, aes(x = Year, y = count), alpha = 0.1) +
  geom_line(aes(color = location_name))+
  geom_point() +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "Country (top 5 in periodontitis expenditure) "
  )  +
  geom_ribbon(aes(ymin = lower, 
                  ymax = upper, fill = location_name), 
              alpha = 0.1) +
  guides(fill = "none", alpha = "none")

# top_20_count_df_ex_China <- top_20_count_df %>%
#   filter (!location_name == "China")
# 
# plot_top20_exp_ex_China <- ggplot(top_20_count_df_ex_China, aes(x = Year, y = count, group = location_name)) +
#   geom_line(aes(color = location_name))+
#   geom_point() +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Prevalence counts (millions)",
#     color = "Country (top 20 in periodontitis expenditure)"
#   )


#-----------------------------------------------------------------------------------------------------------------

# Select top 5 countries by 2050 periodontitis counts to assess population changes

top_5_perio_list <- perio_prevalence_counts_forecast %>%
  filter(Level == 3) %>%
  filter(Year == 2021) %>%
  slice_max (count, n = 5)%>%
  pull(location_name)

top_5_perio_df <- perio_prevalence_counts_forecast %>%
  filter(location_name %in% top_5_perio_list) %>%
  filter(Level == 3) 

plot_top5_pop <- ggplot(top_5_perio_df, aes(x = Year, y = count, group = location_name)) +
  geom_line (data = perio_prevalence_counts_forecast_countries, aes(x = Year, y = count), alpha = 0.1) +
  geom_line(aes(color = location_name))+
  geom_point() +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "Country (top 20 in periodontitis prevalence counts by 2050)"
  ) +
  geom_ribbon(aes(ymin = lower, 
                  ymax = upper, fill = location_name), 
              alpha = 0.1)   +
  guides(fill = "none", alpha = "none")

#Select top 20 countries by perio case counts to assess population changes excluding India and China
# 
# top_20_perio_df_ex_India_China <- top_20_perio_df %>%
#   filter(!location_name == "India") %>%
#   filter(!location_name == "China")
# 
# plot_top20_pop_ex_India_China <- ggplot(top_20_perio_df_ex_India_China, aes(x = Year, y = count, group = location_name)) +
#   geom_line(aes(color = location_name))+
#   geom_point() +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Prevalence counts (millions)",
#     color = "Country (top 20 in periodontitis prevalence counts) ex China and India"
#   )

# plot_patchwork_pop <- (plot_global_pop + plot_superregion_pop + plot_top5_exp + plot_top5_pop) +
#   plot_layout(ncol = 2, nrow = 3)
# 
# ggsave("outputs_forecast/patchwork_population.pdf", plot_patchwork_pop, width = 25, height = 20, dpi = 300)


#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
# Assessment of expenditure around the world over time (parallel to before)
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------

# Load files and data prep

library(tidyverse)
library(patchwork)
library(countrycode)

perio_expenditure <- read_csv("outputs_forecast/expenditure_summary_forecast.csv") %>%
  rename(location_name = LocationHeader)

perio_expenditure_countries <- perio_expenditure %>%
  filter (Level ==3)

#-----------------------------------------------------------------------------------------------------------------

# Assess perio expenditure changes for each superregion
superregion_cost_df <- perio_expenditure %>%
  filter(Level == 1) %>%
  mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
  mutate(location_name = fct_rev(location_name))

plot_superregion_cost <- ggplot(data = superregion_cost_df, aes(x = Year, y = selected_Mean_total_billions, 
                                                                group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions, 
                  ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name), 
              alpha = 0.1) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Total Periodontitis Expenditure (billions)",
    color = "GBD Superregions (in descending order)"
  ) +
  scale_fill_discrete(guide = "none")  

# plot_superregion_cost

#-----------------------------------------------------------------------------------------------------------------

# Assess perio expenditure changes globally
global_cost_df <- perio_expenditure %>%
  filter(Level == 0)

plot_global_cost <- ggplot(data = global_cost_df, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions, 
                  ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name), 
              alpha = 0.1) +
  scale_y_continuous(limits = c(0,NA)) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Total Periodontitis Expenditure (millions)",
    color = "Global"
  ) +
  scale_fill_discrete(guide = "none")  

# plot_global_cost

#-----------------------------------------------------------------------------------------------------------------

# Select top 5 countries by current perio expenditure 

perio_expenditure_wide <- read_csv("outputs_forecast/expenditure_summary_forecast_wide.csv") %>%
  filter(Level ==3) %>%
  arrange(desc(selected_Mean_total_billions_2021)) %>%
  slice(1:5) %>%
  select(iso3c)

top_5_expenditure_df <- perio_expenditure %>%
  inner_join(perio_expenditure_wide, by = "iso3c") %>%
  mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
  mutate(location_name = fct_rev(location_name))

plot_top5_exp_cost <- ggplot(top_5_expenditure_df, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
  geom_line (data = perio_expenditure_countries, aes(x = Year, y = selected_Mean_total_billions), alpha = 0.1) +
  geom_line(aes(color = location_name))+
  geom_point() +
  geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions, 
                  ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name), 
              alpha = 0.1) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Total Periodontitis Expenditure (millions)",
    color = "Country (top 5 in 2025 periodontitis expenditure) "
  ) +
  guides(fill = "none", alpha = "none")

# Select top 20 countries by 2025 excluding China and USA
# 
# top_20_expenditure_df_ex_USA_China <- top_20_expenditure_df %>%
#   filter(!iso3c %in% c("USA","CHN"))
# 
# plot_top20_exp_ex_USA_China <- ggplot(top_20_expenditure_df_ex_USA_China, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
#   geom_line(aes(color = location_name))+
#   geom_point() +
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions, 
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name), 
#               alpha = 0.1) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Total Periodontitis Expenditure (millions)",
#     color = "Country (top 20 in 2025 periodontitis expenditure) ex USA, CHN "
#   ) +
#   scale_fill_discrete(guide = "none")  

#-----------------------------------------------------------------------------------------------------------------

# Select top 5 countries by 2050 periodontitis counts to assess population changes

top_5_perio_list <- perio_prevalence_counts_forecast %>%
  filter(Level == 3) %>%
  filter(Year == 2021) %>%
  slice_max (count, n = 5)%>%
  pull(location_name)

top_5_count_df_joined <- perio_expenditure %>%
  filter(location_name %in% top_5_perio_list) %>%
  mutate(location_name = fct_reorder(location_name, selected_Mean_total_billions)) %>%
  mutate(location_name = fct_rev(location_name))


plot_top5_pop_cost <- ggplot(top_5_count_df_joined, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
  geom_line (data = perio_expenditure_countries, aes(x = Year, y = selected_Mean_total_billions), alpha = 0.1) +
  geom_line(aes(color = location_name))+
  geom_point() +
  geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions, 
                  ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name), 
              alpha = 0.1) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Total Periodontitis Expenditure (millions)",
    color = "Country (top 5 in periodontitis prevalence counts by 2050)"
  ) +
  guides(fill = "none", alpha = "none")

plot_top5_pop_cost

#Select top 20 countries by perio case counts to assess population changes excluding USA and China
# 
# top_20_count_df_joined_USA_China <- top_20_count_df_joined %>%
#   filter(!iso3c %in% c("USA","CHN"))
# 
# plot_top20_pop_ex_USA_China <- ggplot(top_20_count_df_joined_USA_China, aes(x = Year, y = selected_Mean_total_billions, group = location_name)) +
#   geom_line(aes(color = location_name))+
#   geom_point() +
#   geom_ribbon(aes(ymin = selected_Mean_total_billions - selected_SD_total_billions, 
#                   ymax = selected_Mean_total_billions + selected_SD_total_billions, fill = location_name), 
#               alpha = 0.1) +
#   theme_minimal() +
#   theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
#   labs(
#     y = "Prevalence counts (millions)",
#     color = "Country (top 20 in periodontitis prevalence counts) ex USA, CHN"
#   ) +
#   scale_fill_discrete(guide = "none")  

plot_patchwork <- (plot_global_cost +plot_global_pop +
                     plot_superregion_cost + plot_superregion_pop +
                     plot_top5_exp_cost + plot_top5_exp +
                     plot_top5_pop_cost + plot_top5_pop
) +
  plot_layout(ncol = 2, nrow = 4)

ggsave("outputs_forecast/patchwork_expenditure_summary.pdf", plot_patchwork, width = 25, height = 25, dpi = 300)

# plot_patchwork_exp <- (plot_global_cost + plot_superregion_cost + plot_top20_exp + plot_top20_exp_ex_USA_China + plot_top20_pop + plot_top20_pop_ex_USA_China) +
#   plot_layout(ncol = 2, nrow = 3)
# 
# ggsave("outputs_forecast/patchwork_expenditure.pdf", plot_patchwork_exp, width = 25, height = 20, dpi = 300)


