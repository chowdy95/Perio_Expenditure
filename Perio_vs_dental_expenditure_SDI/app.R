# app.R
library(shiny)
library(tidyverse)
library(countrycode)
library(plotly)

# ---------------------------
# Data prep (runs once)
# ---------------------------
country_totals <- read_csv("outputs/short_final_selected_output.csv")
sdi_data <- read_csv("data/GBD_SDI_quintiles.csv") %>%
  rename(Country = `Location Name`, SDI = `2023 SDI Index Value`)

# Ensure country name column exists in main dataset
if (!"Country" %in% names(country_totals)) {
  stop("Main CSV must contain a 'Country' column. Rename it or update code.")
}

# Add iso3c to both datasets
country_totals <- country_totals %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

sdi_data <- sdi_data %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  distinct(iso3c, .keep_all = TRUE)

# Merge datasets
merged_data <- country_totals %>%
  left_join(sdi_data %>% select(iso3c, SDI), by = "iso3c") %>%
  filter(!is.na(iso3c), !is.na(SDI)) %>%
  mutate(
    perio_exppc_usd = (selected_Mean_total_billions * 1e9) / Pop,
    selected_model = factor(selected_model, levels = c("low", "mid", "high"), ordered = TRUE)
  ) %>%
  filter(!is.na(Dent_exppc_usd), !is.na(perio_exppc_usd))

# ---------------------------
# Discretise SDI based on GBD quintiles
# ---------------------------
sdi_breaks <- c(0, 0.531563818, 0.630484502, 0.676662495, 0.718826599, 1)
sdi_labels <- c("Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI")

merged_data <- merged_data %>%
  mutate(
    SDI_cat = cut(
      SDI,
      breaks = sdi_breaks,
      labels = sdi_labels,
      include.lowest = TRUE,
      right = TRUE,
      ordered_result = TRUE
    )
  )

# enforce order for legend
merged_data$SDI_cat <- factor(merged_data$SDI_cat, levels = sdi_labels, ordered = TRUE)

# Prepare choices for filters
all_superregions <- sort(unique(na.omit(merged_data$Superregion)))
all_regions <- sort(unique(na.omit(merged_data$Region)))
all_countries <- sort(unique(na.omit(merged_data$Country)))

# Color palette (blue-yellow-red)
model_colors <- c("low"  = "#3182bd", "mid"  = "#fed976", "high" = "#de2d26")

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Periodontal vs Dental Expenditure (interactive)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Filter by Superregion, Region, or Country. Hover points to see details."),
      selectInput(
        "superregion",
        "Superregion",
        choices = c("All", all_superregions),
        selected = "All"
      ),
      selectInput(
        "region",
        "Region",
        choices = c("All", all_regions),
        selected = "All"
      ),
      selectizeInput(
        "countries",
        "Country (multi-select)",
        choices = all_countries,
        selected = NULL,
        multiple = TRUE,
        options = list(placeholder = 'Select countries (optional)')
      ),
      hr(),
      checkboxInput("show_points", "Show points", value = TRUE),
      width = 3
    ),
    mainPanel(
      h4("Periodontal vs Dental Expenditure per capita"),
      plotlyOutput("scatter", height = "700px"),
      br(),
      p("Hover over a point to see: Country, Region, Superregion, SDI category, Dental exp per capita, Periodontal exp per capita, and Utilisation scenario."),
      width = 9
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # Update country choices dynamically
  observeEvent(list(input$superregion, input$region), {
    df <- merged_data
    if (input$superregion != "All") df <- df %>% filter(Superregion == input$superregion)
    if (input$region != "All") df <- df %>% filter(Region == input$region)
    choices <- sort(unique(na.omit(df$Country)))
    curr <- isolate(input$countries)
    new_sel <- intersect(curr, choices)
    updateSelectizeInput(session, "countries", choices = choices, selected = new_sel, server = TRUE)
  }, ignoreInit = TRUE)
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- merged_data
    if (input$superregion != "All") df <- df %>% filter(Superregion == input$superregion)
    if (input$region != "All") df <- df %>% filter(Region == input$region)
    if (!is.null(input$countries) && length(input$countries) > 0) {
      df <- df %>% filter(Country %in% input$countries)
    }
    df
  })
  
  # Plot
  output$scatter <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    gp <- ggplot(df, aes(
      x = Dent_exppc_usd,
      y = perio_exppc_usd,
      color = selected_model,
      size = SDI_cat,
      text = paste0(
        "<b>", Country, "</b><br>",
        "Region: ", Region, "<br>",
        "Superregion: ", Superregion, "<br>",
        "Utilisation scenario: ", selected_model, "<br>",
        "SDI: ", round(SDI, 3), " (", SDI_cat, ")<br>",
        "Dental exp per capita (USD): ", scales::comma(Dent_exppc_usd, accuracy = 0.01), "<br>",
        "Periodontal exp per capita (USD): ", scales::comma(perio_exppc_usd, accuracy = 0.01)
      )
    )) +
      geom_point() +
      scale_x_log10(
        labels = scales::comma_format(scale = 1, suffix = " USD"),
        name = "Dental Expenditure per Capita (log scale)"
      ) +
      scale_y_log10(
        labels = scales::comma_format(scale = 1, suffix = " USD"),
        name = "Periodontal Expenditure per Capita (log scale)"
      ) +
      scale_color_manual(values = model_colors, name = "Utilisation Scenario") +
      scale_size_manual(
        values = c(1.5, 2.5, 3.5, 5, 6),
        name = "SDI Category"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid = element_blank(),
        legend.position = "right",
        axis.line = element_line(colour = "black", linewidth = 0.3),
        axis.ticks = element_line(colour = "black", linewidth = 0.3),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black")
      )
    
    ggplotly(gp, tooltip = "text") %>%
      layout(legend = list(orientation = "v", x = 1.02, y = 0.95))
  })
}

# ---------------------------
# Run the app
# ---------------------------
shinyApp(ui, server)
