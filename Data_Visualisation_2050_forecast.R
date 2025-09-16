#------------------------------------------------------------------------------------------
# Assessment of prevalent counts around the world over time
#------------------------------------------------------------------------------------------

# Load files and data prep

library(tidyverse)
library(patchwork)
library(countrycode)

perio_prevalence_counts <- read_csv("data/GBD_prev_perio_count.csv") %>%
  select(location_id, location_name, val) %>%
  rename("2021" = "val")

hier <- read_csv("data/GBD_location_hierarchy.csv", locale = locale(encoding = "Latin1")) %>%
  select("Location ID", "Location Name", "Level", "Sort Order") %>%
  rename(location_id = "Location ID", location_name = "Location Name") %>%
  slice(1:233) # all groupings after this are non-GBD hierarchy groupings

perio_prevalence_counts_forecast <- read_csv("data/GBD_prev_perio_count_forecast.csv") %>%
  filter(sex == "Both", age_group_name == "All Ages") %>%
  filter(year_id %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  select(-sex, -age_group_name, -lower, -upper) %>%
  pivot_wider(names_from = year_id, values_from = mean) %>%
  left_join(perio_prevalence_counts) %>%
  relocate("2021", .before = "2025") %>%
  left_join(hier, by =) %>%
  arrange(Level)

#-----------------------------------------------------------------------------------------------------------------

# Assess prevalence count changes for each superregion
superregion_df <- perio_prevalence_counts_forecast %>%
  filter(Level == 1) %>%
  pivot_longer(!c(location_name, location_id, Level, "Sort Order"), 
               names_to = "Year", values_to = "count")  %>%
  mutate(count = count /1000000) %>%
  mutate(location_name = fct_reorder(location_name, count)) %>%
  mutate(location_name = fct_rev(location_name))
  

plot_superregion_pop <- ggplot(data = superregion_df, aes(x = Year, y = count, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "GBD Superregions (in descending order)"
  )

# ggsave("outputs_forecast/population_by_superegion.pdf", plot_superregion_pop, width = 15, height = 8, dpi = 300)

#-----------------------------------------------------------------------------------------------------------------

# Assess prevalence count changes globally
global_df <- perio_prevalence_counts_forecast %>%
  filter(Level == 0) %>%
  pivot_longer(!c(location_name, location_id, Level, "Sort Order"), 
               names_to = "Year", values_to = "count")  %>%
  mutate(count = count /1000000)

plot_global_pop <- ggplot(data = global_df, aes(x = Year, y = count, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  scale_y_continuous(limits = c(0,NA)) +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "Global"
  )

# ggsave("outputs_forecast/population_by_global.pdf", plot_global_pop, width = 15, height = 8, dpi = 300)

#-----------------------------------------------------------------------------------------------------------------

# Select top 20 countries by current perio expenditure to assess population changes

country_totals <- read_csv("outputs/short_final_selected_output.csv") %>%
  filter(Level == 3) %>%
  arrange(desc(selected_Mean_total_billions)) %>%
  slice(1:20) %>%
  select(iso3c)
         

top_20_count_df <- perio_prevalence_counts_forecast %>%
  filter(Level == 3) %>%
  mutate(iso3c = countrycode(location_name, origin = "country.name", destination = "iso3c"))%>%
  inner_join(country_totals, by = "iso3c") %>% 
  pivot_longer(!c(location_name, location_id, Level, "Sort Order", iso3c), 
               names_to = "Year", values_to = "count") %>%
  mutate (count = count/1000000) %>%
  mutate(location_name = fct_reorder(location_name, count)) %>%
  mutate(location_name = fct_rev(location_name))

plot_top20_exp <- ggplot(top_20_count_df, aes(x = Year, y = count, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "Country (top 20 in periodontitis expenditure) "
  )

top_20_count_df_ex_China <- perio_prevalence_counts_forecast %>%
  filter(Level == 3) %>%
  mutate(iso3c = countrycode(location_name, origin = "country.name", destination = "iso3c"))%>%
  inner_join(country_totals, by = "iso3c") %>% 
  pivot_longer(!c(location_name, location_id, Level, "Sort Order", iso3c), 
               names_to = "Year", values_to = "count") %>%
  filter (!location_name == "China") %>%
  mutate (count = count/1000000) %>%
  mutate(location_name = fct_reorder(location_name, count)) %>%
  mutate(location_name = fct_rev(location_name))


plot_top20_exp_ex_China <- ggplot(top_20_count_df_ex_China, aes(x = Year, y = count, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "Country (top 20 in periodontitis expenditure)"
  )

#-----------------------------------------------------------------------------------------------------------------

# Select top 20 countries by 2050 periodontitis counts to assess population changes


#Select top 20 countries by perio case counts to assess population changes
top_20_perio_df <- perio_prevalence_counts_forecast %>%
  filter(Level == 3) %>%
  arrange(desc(.data[["2050"]])) %>%
  slice(1:20) %>%
  pivot_longer(!c(location_name, location_id, Level, "Sort Order"), 
               names_to = "Year", values_to = "count") %>%
  mutate (count = count/1000000) %>%
  mutate(location_name = fct_reorder(location_name, count)) %>%
  mutate(location_name = fct_rev(location_name))


plot_top20_pop <- ggplot(top_20_perio_df, aes(x = Year, y = count, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "Country (top 20 in periodontitis prevalence counts by 2050)"
  )

#Select top 20 countries by perio case counts to assess population changes excluding India and China
top_20_perio_df_ex_India_China <- perio_prevalence_counts_forecast %>%
  filter(Level == 3) %>%
  arrange(desc(.data[["2050"]])) %>%
  slice(1:20) %>%
  pivot_longer(!c(location_name, location_id, Level, "Sort Order"), 
               names_to = "Year", values_to = "count") %>%
  mutate (count = count/1000000) %>%
  filter(!location_name == "India") %>%
  filter(!location_name == "China") %>%
  mutate(location_name = fct_reorder(location_name, count)) %>%
  mutate(location_name = fct_rev(location_name))


plot_top20_pop_ex_India_China <- ggplot(top_20_perio_df_ex_India_China, aes(x = Year, y = count, group = location_name)) +
  geom_line(aes(color = location_name))+
  geom_point() +
  theme_minimal() +
  theme(plot.margin = margin(t = 30, r = 40, b = 20, l = 40)) +
  labs(
    y = "Prevalence counts (millions)",
    color = "Country (top 20 in periodontitis prevalence counts) ex China and India"
  )

plot_patchwork_pop <- (plot_global_pop + plot_superregion_pop + plot_top20_exp + plot_top20_exp_ex_China + plot_top20_pop + plot_top20_pop_ex_India_China) +
  plot_layout(ncol = 2, nrow = 3)

ggsave("outputs_forecast/patchwork_population.pdf", plot_patchwork_pop, width = 25, height = 20, dpi = 300)



#------------------------------------------------------------------------------------------
# Assessment of prevalent counts around the world over time
#------------------------------------------------------------------------------------------

# Load files and data prep

library(tidyverse)
library(patchwork)
library(countrycode)