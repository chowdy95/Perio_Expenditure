#------------------------------------------------------------------------------------------
# Stacked bar chart of the top 30 countries by periodontal expenditure and the breakup; log transformation done as well
#------------------------------------------------------------------------------------------

library(tidyverse)
library(patchwork)

country_totals <- read_csv("outputs/short_final_selected_output.csv") %>%
  mutate(Country = if_else(Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom", Country))

# Calculate residual cost (others)
df <- country_totals %>%
  mutate(
    Others = selected_Mean_total_billions - selected_Mean_perio_billions - selected_Mean_replace_billions
  ) %>%
  pivot_longer(
    cols = c(
      selected_Mean_perio_billions,
      selected_Mean_replace_billions,
      Others
    ),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(
    Category = case_when(
      Category == "selected_Mean_perio_billions" ~ "Cost of periodontal treatment (narrowly defined)",
      Category == "selected_Mean_replace_billions" ~ "Cost of tooth replacement",
      Category == "Others" ~ "Prevention, diagnostics, consultations, extractions"
    )
  )

# Add total for ordering and filtering
df <- df %>%
  group_by(Country) %>%
  mutate(Total = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Total)) %>%
  mutate(Country = fct_reorder(Country, Total, .desc = TRUE)) %>%
  group_by(Country) %>%
  mutate(rank = cur_group_id()) %>%
  ungroup()

# Keep only top 20 and 60
df_top20 <- df %>%
  filter(rank <= 20)
df_top60 <- df %>%
  filter(rank <= 60)

# Plot with original scale and per capita
my_plot <- ggplot(df_top20, aes(x = Country, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs( # NOTE THAT JOURNALS DON'T TYPICALLY LIKE PLOT TITLES OR SUBTITLES
    # title = "Top 20 countries by periodontal expenditure",
    # subtitle = "Periodontal spending is geographically concentrated, and substantially driven by preventive care, tooth replacement, and ancillary procedures",
    x = "",
    y = "Mean Cost (Billions USD)",
    fill = "Expenditure Type"
  ) +
  scale_fill_brewer(palette = "Pastel1") + # SUGGEST A COLOURBLIND-FRIENDLY PALETTE LIKE OKABE-ITO OR VIRIDIS
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "plain"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 30, r = 40, b = 20, l = 40),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, 20)) +
  coord_flip()
my_plot

# NOTE: IT MIGHT BE ILLUSTRATIVE TO ALSO PLOT THE PER CAPITA EXPENDITURE IN A SEPARATE PANEL USING THE patchwork PACKAGE
# FORMAT IS EASY - FIRST CREATE A RANKED LIST OF TOP 20 PER CAPITA, MAKE AN IDENTICAL PLOT, AND ADD THEM USING GGPLOT SYNTAX
# E.G. my_plot + my_plot_per_capita + plot_annotation(tag_levels = 'A'), see https://patchwork.data-imaginist.com/articles/guides/annotation.html

ggsave("outputs/oral_health_costs_top20.pdf", my_plot, width = 20, height = 12, dpi = 300)

# Log transforming the x axis to better see the differences in breakdown
log_transformed_my_plot <- ggplot(df_top60, aes(x = Country, y = Value * 1e9, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  labs(
    title = "Breakdown of Oral Health Costs by Country, Log Transformed (Top 60)",
    x = "",
    y = "Mean Cost (Billions USD)",
    fill = "Expenditure Type"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 14) +
  coord_flip()

log_transformed_my_plot # INTERESTING CONCEPT BUT NOT SURE IT ADDS MUCH
# ggsave("outputs/oral_health_costs_log_transformed_top60.png", log_transformed_my_plot, width = 20, height = 12, dpi = 300)



#------------------------------------------------------------------------------------------
# Plotting the best fit line showing that periodontal expenditure as predicted is a consistent and reasonable
# proportion of total dental expenditure
#------------------------------------------------------------------------------------------

library(ggrepel)
library(ggpmisc)

country_totals <- read_csv("outputs/short_final_selected_output.csv")

# 1. Filter top 30 by total predicted expenditure
top30 <- country_totals %>%
  filter(Country != "Global") %>%
  arrange(desc(selected_Mean_total_billions)) %>%
  slice_head(n = 30)

# 2. Remaining countries
others <- country_totals %>%
  filter(Country != "Global" & !Country %in% top30$Country)

# 3. Combine again for plotting
plot_data <- bind_rows(
  top30 %>% mutate(Group = "Top30"),
  others %>% mutate(Group = "Other")
)

# 4. Generate plot
regression <- ggplot(plot_data, aes(x = Dent_exp_usd, y = selected_Mean_total_billions)) +
  # Error bars for all points
  geom_errorbar(
    aes(
      ymin = selected_Mean_total_billions - selected_SD_total_billions,
      ymax = selected_Mean_total_billions + selected_SD_total_billions
    ),
    width = 0, color = "grey50", alpha = 0.5
  ) +
  # Points: top30 colored by country, others neutral
  geom_point(
    data = filter(plot_data, Group == "Top30"),
    aes(color = Country), size = 3
  ) +
  geom_point(
    data = filter(plot_data, Group == "Other"),
    color = "grey70", size = 2
  ) +
  # Labels: only top30, matching point color, small, repel, no overlap
  ggrepel::geom_text_repel(
    data = filter(plot_data, Group == "Top30"),
    aes(label = Country, color = Country),
    size = 4.5,
    box.padding = 0.3,
    point.padding = 0.2,
    max.overlaps = Inf,
    segment.color = NA # removes lines connecting text to points
  ) +
  # Best-fit line for all points
  geom_smooth(method = "lm", se = TRUE, color = "firebrick1", alpha = 0.4) +
  # Add regression equation and R^2
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x = 0.1,
    label.y = 70,
    size = 4
  ) +
  # Scale and axis limits
  scale_x_continuous(limits = c(0, max(plot_data$Dent_exp_usd) * 1.05)) + # STILL NOT LOVING THE MISMATCHED AXES
  scale_y_continuous(limits = c(0, 60)) +
  coord_fixed(ratio = 1) +
  # Nice pastel palette for countries
  scale_color_manual(values = scales::hue_pal()(length(unique(top30$Country)))) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  labs(
    title = "Predicted periodontal expenditure forms a consistent and reasonable proportion of dental expenditure",
    x = "Total Dental Expenditure (Billions USD)",
    y = "Predicted Periodontal Expenditure (Billions USD)"
  )
regression

# Or save bigger for clarity
# ggsave("outputs/validation_regression.png", regression, width = 25, height = 12, dpi = 300)


#------------------------------------------------------------------------------------------
# Plotting the best fit line showing that periodontal expenditure as predicted is a consistent and reasonable
# proportion of total dental expenditure with inset graph
#------------------------------------------------------------------------------------------

library(ggrepel)
library(ggpmisc)
library(patchwork)
library(readr)

# 1. Load data and fix USA name
country_totals <- read_csv("outputs/short_final_selected_output.csv") %>%
  mutate(Country = if_else(Country == "United States", "United States of America", Country)) %>%
  filter(Country != "Global")

# 2. Top 30
top30 <- country_totals %>%
  arrange(desc(selected_Mean_total_billions)) %>%
  slice_head(n = 30)

plot_data <- country_totals %>%
  mutate(Group = ifelse(Country %in% top30$Country, "Top30", "Other"))

# 3. Main plot (excluding USA & China)
main_data <- plot_data %>% filter(!Country %in% c("United States of America", "China"))
inset_data <- plot_data

# 4. Zoom window: what will the inset cover?
xlim_zoom <- range(main_data$Dent_exp_usd, na.rm = TRUE) * c(0.95, 1.05)
ylim_zoom <- c(0, min(max(main_data$selected_Mean_total_billions, na.rm = TRUE) * 1.05, 75))

# 5. Best fit line for all data
fit <- lm(selected_Mean_total_billions ~ Dent_exp_usd, data = inset_data)

# 6. Main plot
main_plot <- ggplot(main_data, aes(x = Dent_exp_usd, y = selected_Mean_total_billions)) +
  geom_point(data = filter(main_data, Group == "Top30"), aes(color = Country), size = 2.5) +
  geom_point(data = filter(main_data, Group == "Other"), color = "grey70", size = 1.8) +
  ggrepel::geom_text_repel(
    data = filter(main_data, Group == "Top30"),
    aes(label = Country, color = Country),
    size = 3,
    box.padding = 0.25,
    point.padding = 0.2,
    segment.color = NA
  ) +
  # Add custom regression line
  geom_abline(intercept = 0.00451, slope = 0.336, color = "plum2", size = 1, alpha = 0.5) +
  # Add equation and R² as annotation in the top left
  annotate("text",
    x = 1, y = 12, label = "y = 0.00451 + 0.336x\nR² = 0.99",
    hjust = -0.05, vjust = 1.1, size = 5, color = "grey20"
  ) +
  scale_color_manual(values = scales::hue_pal()(length(unique(top30$Country)))) +
  scale_x_continuous(limits = xlim_zoom) +
  scale_y_continuous(limits = ylim_zoom) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Predicted periodontal expenditure forms a consistent and reasonable proportion of dental expenditure (magnified)",
    subtitle = "Zoomed out chart including USA and China inset",
    x = "Total Dental Expenditure (Billions USD)",
    y = "Predicted Periodontal Expenditure (Billions USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(color = "grey20", hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(color = "grey20", hjust = 0.5, size = 10),
    axis.title = element_text(color = "grey20"),
    legend.position = "none"
  )
main_plot

# 7. Inset plot: full data, no labels
inset_plot <- ggplot(inset_data, aes(x = Dent_exp_usd, y = selected_Mean_total_billions)) +
  # Add grey rectangle from (0,0) to (34,13)
  annotate("rect",
    xmin = 0, xmax = 34, ymin = 0, ymax = 13,
    fill = NA, color = "grey50", size = 0.7, alpha = 0.5
  ) +
  # Add points
  geom_point(data = filter(inset_data, Group == "Top30"), aes(color = Country), size = 1.5) +
  geom_point(data = filter(inset_data, Group == "Other"), color = "grey70", size = 1.5) +
  # Add data labels for China and USA
  ggrepel::geom_text_repel(
    data = filter(inset_data, Country %in% c("United States of America", "China", "Germany", "Japan", "Italy", "Canada")),
    aes(label = Country, color = Country),
    size = 2,
    box.padding = 0.25,
    point.padding = 0.2,
    segment.color = NA
  ) +
  # Regression line
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    color = "plum2", alpha = 0.5, size = 0.8
  ) +
  # Custom colors
  scale_color_manual(values = scales::hue_pal()(length(unique(top30$Country)))) +
  # Keep plot within limits
  coord_cartesian(
    xlim = c(0, max(inset_data$Dent_exp_usd) * 1.05),
    ylim = c(0, max(inset_data$selected_Mean_total_billions) * 1.05)
  ) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Predicted periodontal expenditure forms a consistent and reasonable proportion of dental expenditure",
    x = "Total Dental Expenditure (Billions USD)",
    y = "Predicted Periodontal Expenditure (Billions USD)"
  ) +
  theme_minimal(base_size = 6) +
  theme(
    plot.title = element_text(color = "grey20", hjust = 0.5, size = 6, face = "bold"),
    axis.title = element_text(color = "grey20"),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA), # plot area
    plot.background = element_rect(fill = "white", color = NA) # entire ggplot area
  )

# 8. Combine with patchwork
final_plot <- main_plot +
  inset_element(
    inset_plot,
    left = 0.65, bottom = 0.05,
    right = 0.95, top = 0.35
  )

# 9. Save
ggsave("outputs/validation_regression_inset.pdf", final_plot, width = 18, height = 10, dpi = 300)

#------------------------------------------------------------------------------------------
# Plotting world map with direct expenditure
#------------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

# Read your data
country_totals <- read_csv("outputs/short_final_selected_output.csv")

# Add ISO codes
country_totals <- country_totals %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

# Load world map & crop Antarctica
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")

# Join
map_data <- world %>%
  left_join(country_totals, by = c("iso_a3_eh" = "iso3c"))

# Plot
p <- ggplot(map_data) +
  geom_sf(aes(fill = selected_Mean_total_billions * 1000)) + # billions → millions
  scale_fill_viridis_c(
    trans = "log10",
    option = "plasma",
    begin = 0.15, # skew lighter: higher begin shifts scale lighter
    end = 1,
    direction = -1, # higher = darker
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::comma_format(suffix = "M", scale = 1),
    na.value = "grey80",
    name = "Periodontal Expenditure (Millions USD)"
  ) +
  labs(
    title = "Estimated Periodontal Expenditure by Country",
    subtitle = "Countries with no data shown in grey"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

# Save with suitable dimensions for world map
ggsave("outputs/world_periodontal_expenditure_map.pdf",
  plot = p,
  width = 12, height = 6, dpi = 300
)


#------------------------------------------------------------------------------------------
# Plotting world map with utilisation scenario
#------------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)

# Read your data
country_totals <- read_csv("outputs/short_final_selected_output.csv")

# Add ISO codes
country_totals <- country_totals %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

# Ensure 'selected_model' is an ordered factor
country_totals <- country_totals %>%
  mutate(selected_model = factor(selected_model,
                                 levels = c("low", "mid", "high"),
                                 ordered = TRUE))

# Load world map & crop Antarctica
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")

# Join data to map
map_data <- world %>%
  left_join(country_totals, by = c("iso_a3_eh" = "iso3c"))

# Plot categorical map
p <- ggplot(map_data) +
  geom_sf(aes(fill = selected_model), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("low" = "#3182bd", "mid" = "#fed976", "high" = "#de2d26"),
    na.value = "grey80",
    name = "Utilisation scenario"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Save
ggsave("outputs/world_utilisation_scenario_map.pdf",
       plot = p,
       width = 12, height = 6, dpi = 300
)




#------------------------------------------------------------------------------------------
# Creating a stacked area chart to breakdown cost components between countries (after normalising total)
#------------------------------------------------------------------------------------------

library(tidyverse)
library(forcats)

country_totals <- read_csv("outputs/short_final_selected_output.csv")

# Step 1: Preprocess data
df <- country_totals %>%
  filter(Country != "Global", selected_Mean_total_billions > 0) %>%
  mutate(
    Others = selected_Mean_total_billions - selected_Mean_perio_billions - selected_Mean_replace_billions
  ) %>%
  pivot_longer(
    cols = c(
      selected_Mean_perio_billions,
      selected_Mean_replace_billions,
      Others
    ),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(
    Category = case_when(
      Category == "selected_Mean_perio_billions" ~ "Cost of periodontal treatment (narrowly defined)",
      Category == "selected_Mean_replace_billions" ~ "Cost of tooth replacement",
      Category == "Others" ~ "Prevention, diagnostics, consultations, extractions"
    )
  )

stacked_data <- df %>%
  filter(!is.na(Value) & Value > 0) %>% # Remove NA or 0
  group_by(Country) %>%
  mutate(
    Total = sum(Value, na.rm = TRUE),
    Proportion = Value / Total
  ) %>%
  ungroup()

# Step 2: Rank countries by total spending
ranked_countries <- stacked_data %>%
  distinct(Country, selected_Mean_total_billions) %>%
  arrange(desc(selected_Mean_total_billions)) %>%
  pull(Country)

# Step 3: Create numeric y for geom_area (in ascending periodontal order)
stacked_data <- stacked_data %>%
  filter(Country %in% ranked_countries) %>%
  mutate(
    Country = factor(Country, levels = ranked_countries),
    y = as.numeric(Country)
  )

# Step 3: Plot
stacked_area_plot <- ggplot(stacked_data, aes(x = Proportion, y = y, fill = Category)) +
  geom_area(stat = "identity", position = "stack", orientation = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(
    breaks = NULL,
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Proportion of total cost",
    y = NULL,
    title = "Expenditure composition by country in ascending order of total periodontal expenditure",
    subtitle = "Composition varies widely between countries, with tooth replacement proportion declining alongside overall expenditure",
    fill = "Cost category"
  ) +
  annotate("rect",
    xmin = 0, xmax = 1, ymin = 1, ymax = 40,
    fill = NA, color = "grey50", size = 1.5, alpha = 0.5
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(color = "grey20", hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(color = "grey20", hjust = 0.5, size = 10, face = "bold"),
    plot.margin = margin(t = 10, r = 40, b = 10, l = 40)
  )

# ggsave("outputs/stacked_area_plot.jpg",
#       plot = stacked_area_plot,
#       width = 12, height = 12, dpi = 300)

#------------------------------------------------------------------------------------------
# Creating a stacked bar chart to break down cost components between countries (after normalising total)
#------------------------------------------------------------------------------------------

library(tidyverse)
library(forcats)

country_totals <- read_csv("outputs/short_final_selected_output.csv")

# Step 1: Preprocess data
df <- country_totals %>%
  filter(Country != "Global", selected_Mean_total_billions > 0) %>%
  mutate(Country = if_else(Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom", Country)) %>%
  arrange(desc(selected_Mean_total_billions)) %>%
  slice_head(n = 40) %>%
  mutate(
    Others = selected_Mean_total_billions - selected_Mean_perio_billions - selected_Mean_replace_billions
  ) %>%
  pivot_longer(
    cols = c(
      selected_Mean_perio_billions,
      selected_Mean_replace_billions,
      Others
    ),
    names_to = "Category",
    values_to = "Value"
  ) %>%
  mutate(
    Category = case_when(
      Category == "selected_Mean_perio_billions" ~ "Cost of periodontal treatment (narrowly defined)",
      Category == "selected_Mean_replace_billions" ~ "Cost of tooth replacement",
      Category == "Others" ~ "Prevention, diagnostics, consultations, extractions"
    )
  )

stacked_data <- df %>%
  filter(!is.na(Value) & Value > 0) %>% # Remove NA or 0
  group_by(Country) %>%
  mutate(
    Total = sum(Value, na.rm = TRUE),
    Proportion = Value / Total
  ) %>%
  ungroup()

# Step 2: Rank countries by total spending
country_order <- stacked_data %>%
  distinct(Country, selected_Mean_total_billions) %>%
  arrange(desc(selected_Mean_total_billions)) %>%
  pull(Country)

# Step 3: Apply factor ordering
stacked_data <- stacked_data %>%
  mutate(Country = factor(Country, levels = country_order))

# Step 4: Create stacked horizontal bar chart
stacked_chart_plot <- ggplot(stacked_data, aes(x = Proportion, y = Country, fill = Category)) +
  geom_col(position = "stack", width = 0.95) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    x = "Proportion of total cost",
    y = NULL,
    title = "Top 40 countries, expenditure composition by country",
    subtitle = "Expenditure composition varies widely even across the highest spending countries"
    #    fill = "Cost category"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(color = "grey20", hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(color = "grey20", hjust = 0.5, size = 10, face = "bold"),
    plot.margin = margin(t = 10, r = 70, b = 10, l = 40)
  )

# ggsave("outputs/stacked_chart_plot_top40.png",
#       plot = stacked_chart_plot,
#       width = 12, height = 12, dpi = 300)


#------------------------------------------------------------------------------------------
# Combining stacked area with stacked bar plot
#------------------------------------------------------------------------------------------

library(cowplot)

combined_stacked_plot <- plot_grid(stacked_chart_plot, stacked_area_plot, nrow = 1, rel_widths = c(1, 1))
final_combined_stacked_plot <- ggdraw(combined_stacked_plot) +
  draw_line(
    x = c(0.465, 0.523),
    y = c(0.095, 0.095),
    color = "grey30",
    size = 1,
    alpha = 0.7,
    linetype = "dashed"
  ) +
  draw_line(
    x = c(0.464, 0.5225),
    y = c(0.937, 0.27),
    color = "grey30",
    size = 1,
    alpha = 0.7,
    linetype = "dashed"
  )

ggsave("outputs/combined_stacked_plot.pdf",
  plot = final_combined_stacked_plot,
  width = 24, height = 12, dpi = 300
)



#------------------------------------------------------------------------------------------
# Tree Map by Procedure
#------------------------------------------------------------------------------------------

library(tidyverse)
library(treemapify)
library(patchwork)

# Load your data
df <- read_csv("outputs/global_procedure.csv") %>%
  mutate(
    Procedure = recode(Procedure,
      "Consult_perio" = "Consult (severe periodontitis)",
      "Consult_simple" = "Consult (healthy/mild periodontitis)",
      "Denture" = "Denture Fabrication",
      "Denture_repair" = "Denture Repair",
      "Full_fixed" = "Full-arch Fixed Prosthesis",
      "GTR" = "Regenerative Surgery",
      "Implant_surgery" = "Implant Surgery (full-arch prostheses)",
      "Maintenance_perio" = "Maintenance (severe periodontitis)",
      "Maintenance_simp" = "Maintenance (healthy/mild periodontitis)",
      "OFD" = "Access Flap Surgery",
      "OHI" = "Oral Hygiene Education",
      "OPG" = "Panoramic Radiograph",
      "PA" = "Periapicals",
      "Prophy" = "Prophylaxis",
      "RootDeb" = "Root Surface Debridement",
      "Single_implant" = "Single Implant (prosthesis + placement)"
    ),
    Group = case_when(
      Procedure %in% c("Prophylaxis", "Consult (healthy/mild periodontitis)", "Maintenance (healthy/mild periodontitis)") ~ "Preventive",
      Procedure %in% c("Panoramic Radiograph", "Periapicals") ~ "Diagnostic",
      Procedure %in% c(
        "Oral Hygiene Education", "Root Surface Debridement", "Regenerative Surgery",
        "Access Flap Surgery", "Maintenance (severe periodontitis)", "Consult (severe periodontitis)"
      ) ~ "Periodontal treatment",
      Procedure == "Extraction" ~ "Extractions",
      TRUE ~ "Tooth replacement"
    ),
    global_Mean_total_billions = round(global_Mean_total_billions, 2),
    global_WHO_Mean_total_billions = round(global_WHO_Mean_total_billions, 2)
  )

# Plot
treemap <- ggplot(df, aes(
  area = global_Mean_total_billions,
  fill = Group,
  label = paste0(Procedure, "\n", global_Mean_total_billions, "B"),
  subgroup = Group
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white", size = 2) +
  geom_treemap_text(
    colour = "grey20",
    fontface = "plain",
    family = "sans",
    place = "centre",
    reflow = TRUE,
    size = 10
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  ggtitle("Based on current utilisation (2021)") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(color = "grey20", hjust = 0.5, size = 16, face = "bold"),
    # plot.subtitle = element_text(color = "grey20", hjust = 0.5, size = 10, face = "bold"),
    plot.margin = margin(t = 20, r = 40, b = 20, l = 40)
  )

treemap_target <- ggplot(df, aes(
  area = global_WHO_Mean_total_billions,
  fill = Group,
  label = paste0(Procedure, "\n", global_WHO_Mean_total_billions, "B"),
  subgroup = Group
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white", size = 2) +
  geom_treemap_text(
    colour = "grey20",
    fontface = "plain",
    family = "sans",
    place = "centre",
    reflow = TRUE,
    size = 10
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  ggtitle("Based on WHO target (2021)") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(color = "grey20", hjust = 0.5, size = 16, face = "bold"),
    # plot.subtitle = element_text(color = "grey20", hjust = 0.5, size = 10, face = "bold"),
    plot.margin = margin(t = 20, r = 40, b = 20, l = 40)
  )

treemap_combined <- treemap + treemap_target

ggsave("outputs/treemap.pdf",
  plot = treemap_combined,
  width = 20, height = 10, dpi = 300
)

#------------------------------------------------------------------------------------------
# Line chart showing periodontal expenditure by scenario
#------------------------------------------------------------------------------------------

library(tidyverse)
library(RColorBrewer)

# Load data
df <- read_csv("outputs/final_selected_output.csv") %>%
  filter(
    !Country %in% c("United States of America", "China", "Global", "India"),
    selected_Mean_total_billions != 0
  )


# Create rank by selected total
df_ranked <- df %>%
  arrange(desc(selected_Mean_total_billions)) %>%
  mutate(CountryRank = row_number())

# Define scenario colors
scenario_levels <- c("high", "mid", "low")
colors <- brewer.pal(n = 3, name = "Pastel1")
names(colors) <- scenario_levels

# Reshape to long format for the first 3 layers (high, mid, low)
df_long <- df_ranked %>%
  select(
    CountryRank,
    Mean_total_billions_high,
    Mean_total_billions_mid,
    Mean_total_billions_low
  ) %>%
  pivot_longer(
    cols = starts_with("Mean_total_billions_"),
    names_to = "Scenario",
    values_to = "Cost"
  ) %>%
  mutate(
    Scenario = str_replace(Scenario, "Mean_total_billions_", ""),
    Scenario = factor(Scenario, levels = scenario_levels)
  )

# Base plot
p <- ggplot() +
  # First 3 layers: High, Mid, Low
  geom_point(
    data = df_long,
    aes(x = CountryRank, y = Cost, color = Scenario),
    size = 0.3,
    alpha = 0.5,
    position = position_jitter(width = 0.3)
  ) +
  # Fourth layer: selected scenario
  geom_point(
    data = df_ranked,
    aes(
      x = CountryRank, y = selected_Mean_total_billions,
      shape = selected_model,
      color = selected_model
    ),
    size = 1.5,
    stroke = 1.1,
    position = position_jitter(width = 0.3)
  ) +
  scale_color_manual(
    name = "Hypothetical Dental Utilisation by Model",
    values = colors,
    breaks = scenario_levels,
    labels = c("High Scenario Model", "Medium Scenario Model", "Low Scenario Model")
  ) +
  scale_shape_manual(
    name = "Selected Dental Utilisation",
    values = c(high = 17, mid = 18, low = 15), # triangle, diamond, square
    labels = c("High Dental Utilisation", "Medium Dental Utilisation", "Low Dental Utilisation")
  ) +
  labs(
    title = "Periodontal Expenditure by Scenario and Country",
    subtitle = "A shift toward higher dental utilisation could lead to significantly greater periodontal expenditure",
    x = "Countries ranked by total cost",
    y = "Expenditure (Billions USD)",
    caption = "USA, China and India excluded due to incompatible scale"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "plain"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.margin = margin(t = 30, r = 40, b = 20, l = 40)
  )

# Save as PDF
ggsave(
  filename = "outputs/periodontal_expenditure_plot.pdf",
  plot = p,
  width = 12,
  height = 6,
  dpi = 300
)


#------------------------------------------------------------------------------------------
# Validation chart showing that estimated periodontal expenditure forms a reasonable proportion of dental expenditure
#------------------------------------------------------------------------------------------

library(tidyverse)
library(RColorBrewer)

# Load data
df <- read_csv("outputs/short_final_selected_output.csv")

# Create proportion, median and rank
df <- df %>%
  filter(Country != "Global", selected_Mean_total_billions != 0) %>%
  mutate(
    PerioProp = selected_Mean_total_billions / Dent_exp_usd,
    median_value = median(PerioProp),
    Rank = rank(selected_Mean_total_billions),
    selected_model = factor(selected_model, levels = c("high", "mid", "low")),
    ModelLabel = recode(selected_model,
      high = "High Utilisation",
      mid = "Medium Utilisation",
      low = "Low Utilisation"
    )
  ) %>%
  filter(PerioProp < 1)

# Define shapes and colors
shapes <- c(high = 17, mid = 18, low = 15)
colors <- brewer.pal(3, "Pastel1")
names(colors) <- c("high", "mid", "low")

# Create plot
p <- ggplot(df, aes(x = Rank, y = PerioProp * 100, color = selected_model, shape = selected_model)) +
  geom_point(aes(size = Dent_exppc_usd), alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(
    name = "Dental Utilisation",
    values = colors,
    labels = c("High Utilisation", "Medium Utilisation", "Low Utilisation")
  ) +
  scale_shape_manual(
    name = "Dental Utilisation",
    values = shapes,
    labels = c("High Utilisation", "Medium Utilisation", "Low Utilisation")
  ) +
  scale_size_continuous(
    name = "Dental Expenditure Per Capita (USD)",
    range = c(0.05, 5),
    labels = scales::dollar
  ) +
  labs(
    title = "Estimated Periodontal Expenditure as a Share of Total Dental Spending",
    subtitle = "Across countries, periodontal expenditure forms a reasonable proportion of total dental spending,\nincreasing with dental expenditure per capita, supporting the plausibility of the estimation model",
    x = "Countries ranked by periodontal expenditure",
    y = "Periodontal expenditure (% of dental spending)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.margin = margin(30, 30, 30, 30)
  )

# Save as PDF
ggsave(
  filename = "outputs/periodontal_prop_scatter.pdf",
  plot = p,
  width = 10,
  height = 6.5,
  device = cairo_pdf # Ensures font rendering works well with showtext
)


#------------------------------------------------------------------------------------------
# Graph investigating the effect of dental expenditure per capita on utilisation
#------------------------------------------------------------------------------------------

library(tidyverse)

# Load data
df <- read_csv("outputs/short_final_selected_output.csv") %>%
  filter(Country != "Global", selected_Mean_total_billions != 0) %>%
  mutate(
    PerioProp = selected_Mean_total_billions / Dent_exp_usd,
    median_value = median(PerioProp),
    Rank = rank(selected_Mean_total_billions),
    selected_model = factor(selected_model, levels = c("high", "mid", "low")),
    ModelLabel = recode(selected_model,
      high = "High Utilisation",
      mid = "Medium Utilisation",
      low = "Low Utilisation"
    )
  )


jitter_plot <- ggplot(df, aes(x = selected_model, y = Dent_exppc_usd, color = selected_model)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.9) +
  scale_color_brewer(palette = "Pastel1") +
  labs(
    x = "Dental Utilisation Scenario",
    y = "Dental Expenditure per Capita (USD)",
    title = "Dental Expenditure Per Capita by Utilisation Scenario",
    subtitle = ""
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.margin = margin(30, 40, 40, 30)
  )

# Save as PNG
ggsave(
  filename = "outputs/jitter_dental_expenditure_against_scenario.pdf",
  plot = jitter_plot,
  width = 9,
  height = 8,
  dpi = 300
)

ggplot(df, aes(x = selected_model, y = GDP_per_capita_PPP_2021, color = selected_model)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  scale_color_brewer(palette = "Pastel1") +
  labs(
    x = "Dental Utilisation Scenario",
    y = "Dental Expenditure per Capita (USD)",
    title = "Per-Capita Dental Expenditure by Utilisation Scenario"
  ) +
  theme_minimal()



#------------------------------------------------------------------------------------------
# GDP vs Periodontitis Expenditure per Capita (log-log plot, minimalist pastel version)
#------------------------------------------------------------------------------------------

library(tidyverse)

# Read and prepare data
plot_data <- read_csv("outputs/short_final_selected_output.csv") %>%
  filter(!is.na(iso3c)) %>% # exclude aggregates
  mutate(
    exp_per_capita = (selected_Mean_total_billions * 1e9) / Pop,
    selected_model = factor(selected_model, levels = c("low", "mid", "high"), ordered = TRUE)
  ) %>%
  filter(!is.na(exp_per_capita), !is.na(GDP_per_capita_PPP_2021), !is.na(Dent_exppc_usd))

# Define color palette (colorblind-friendly blue-yellow-red)
model_colors <- c("low" = "#3182bd", "mid" = "#fed976", "high" = "#de2d26")

# Plot
p <- ggplot(plot_data, aes(
  x = GDP_per_capita_PPP_2021,
  y = exp_per_capita,
  color = selected_model,
  size = Dent_exppc_usd
)) +
  geom_point(alpha = 0.9) +
  scale_color_manual(values = model_colors, name = "Utilisation scenario") +
  scale_size_continuous(
    range = c(1, 8),
    labels = scales::comma_format(accuracy = 1),
    name = "Dental expenditure\nper capita (USD)"
  ) +
  scale_x_log10(
    labels = scales::comma_format(),
    name = "GDP per capita (PPP, USD) (log scale)"
  ) +
  scale_y_log10(
    labels = scales::comma_format(accuracy = 1),
    name = "Periodontitis expenditure per capita (USD) (log scale)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.3),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save
ggsave("outputs/gdp_vs_expenditure_percapita.pdf",
       plot = p,
       width = 8, height = 6, dpi = 300
)


#------------------------------------------------------------------------------------------
# SDI vs Periodontitis Expenditure per Capita (log scale, minimalist blue-yellow-red)
#------------------------------------------------------------------------------------------

library(tidyverse)
library(countrycode)

#-----------------------------
# 1. Read and prepare main data
#-----------------------------
main_data <- read_csv("outputs/short_final_selected_output.csv") %>%
  filter(!is.na(iso3c)) %>% # exclude aggregates
  mutate(
    exp_per_capita = (selected_Mean_total_billions * 1e9) / Pop,
    selected_model = factor(selected_model, levels = c("low", "mid", "high"), ordered = TRUE)
  )

#-----------------------------
# 2. Read and prepare SDI data
#-----------------------------
sdi_data <- read_csv("data/GBD_SDI_quintiles.csv") %>%
  rename(
    Country = `Location Name`,
    SDI = `2023 SDI Index Value`
  ) %>%
  mutate(
    iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(!is.na(iso3c)) # keep only valid country matches

#-----------------------------
# 3. Merge datasets on iso3c
#-----------------------------
merged_data <- main_data %>%
  left_join(sdi_data %>% select(iso3c, SDI), by = "iso3c") %>%
  filter(!is.na(SDI), !is.na(exp_per_capita), !is.na(Dent_exppc_usd))

#-----------------------------
# 4. Define color palette (strong, colorblind-friendly)
#-----------------------------
model_colors <- c(
  "low"  = "#3182bd",  # blue
  "mid"  = "#fed976",  # yellow
  "high" = "#de2d26"   # red
)

#-----------------------------
# 5. Plot
#-----------------------------
p <- ggplot(merged_data, aes(
  x = SDI,
  y = exp_per_capita,
  color = selected_model,
  size = Dent_exppc_usd
)) +
  geom_point(alpha = 0.9) +
  scale_color_manual(values = model_colors, name = "Utilisation scenario") +
  scale_size_continuous(
    range = c(1, 8),
    labels = scales::comma_format(accuracy = 1),
    name = "Dental expenditure\nper capita (USD)"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    name = "Socio-demographic Index (SDI)"
  ) +
  scale_y_log10(
    labels = scales::comma_format(accuracy = 1),
    name = "Periodontitis expenditure per capita (USD) (log scale)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.3),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

#-----------------------------
# 6. Save output
#-----------------------------
ggsave("outputs/sdi_vs_expenditure_percapita.pdf",
       plot = p,
       width = 8, height = 6, dpi = 300
)



#------------------------------------------------------------------------------------------
# Plotting utilisation scenario vs expenditure, sized by SDI
#------------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(countrycode)

# Read datasets
country_totals <- read_csv("outputs/short_final_selected_output.csv")
sdi_data <- read_csv("data/GBD_SDI_quintiles.csv")

# Clean and standardize column names
sdi_data <- sdi_data %>%
  rename(
    Country = `Location Name`,
    SDI = `2023 SDI Index Value`
  )

# Add ISO codes for matching
country_totals <- country_totals %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

sdi_data <- sdi_data %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

# Merge and clean
merged_data <- country_totals %>%
  left_join(sdi_data %>% select(iso3c, SDI), by = "iso3c") %>%
  filter(!is.na(iso3c), !is.na(SDI))

# Calculate periodontitis expenditure per capita (USD)
merged_data <- merged_data %>%
  mutate(perio_exppc_usd = (selected_Mean_total_billions * 1e9) / Pop)

#------------------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------------------

p <- ggplot(merged_data, aes(
  x = Dent_exppc_usd,
  y = perio_exppc_usd,
  color = selected_model,
  size = SDI
)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(
    labels = scales::comma_format(scale = 1, suffix = " USD"),
    name = "Dental Expenditure per Capita (log scale)"
  ) +
  scale_y_log10(
    labels = scales::comma_format(scale = 1, suffix = " USD"),
    name = "Periodontal Expenditure per Capita (log scale)"
  ) +
  scale_color_manual(
    values = c(  "low"  = "#3182bd",  
                 "mid"  = "#fed976",  
                 "high" = "#de2d26"   ),
    name = "Utilisation Scenario"
  ) +
  scale_size_continuous(
    range = c(0.5, 6),
    name = "Socio-demographic Index (SDI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.line = element_line(colour = "black", linewidth = 0.3),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 20, 10, 10)
  )

#------------------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------------------
ggsave(
  "outputs/scatter_periodontal_vs_dental_SDI_sized.pdf",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)

#------------------------------------------------------------------------------------------
# Same plot but on a continuous axis
#------------------------------------------------------------------------------------------

p <- ggplot(merged_data, aes(
  x = Dent_exppc_usd,
  y = perio_exppc_usd,
  color = selected_model,
  size = SDI
)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(
    labels = scales::comma_format(scale = 1, suffix = " USD"),
    name = "Dental Expenditure per Capita"
  ) +
  scale_y_continuous(
    labels = scales::comma_format(scale = 1, suffix = " USD"),
    name = "Periodontal Expenditure per Capita"
  ) +
  scale_color_manual(
    values = c(  "low"  = "#3182bd",  
                 "mid"  = "#fed976",  
                 "high" = "#de2d26"   ),
    name = "Utilisation Scenario"
  ) +
  scale_size_continuous(
    range = c(0.1, 5),
    name = "Socio-demographic Index (SDI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.line = element_line(colour = "black", linewidth = 0.3),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 20, 10, 10)
  )

#------------------------------------------------------------------------------------------
# Save output
#------------------------------------------------------------------------------------------
ggsave(
  "outputs/scatter_periodontal_vs_dental_SDI_continuous.pdf",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)
