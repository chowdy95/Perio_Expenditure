# app.R
library(shiny)
library(tidyverse)
library(countrycode)
library(plotly)
library(rsconnect)

# ---------------------------
# Data prep (runs once)
# ---------------------------

country_totals <- read_csv("outputs/short_final_selected_output.csv")
# country_totals <- read_csv("short_final_selected_output.csv")

sdi_data <- read_csv("data/GBD_SDI_quintiles.csv") %>%
  rename(Country = `Location Name`, SDI = `2023 SDI Index Value`)

# sdi_data <- read_csv("GBD_SDI_quintiles.csv") %>%
#   rename(Country = `Location Name`, SDI = `2023 SDI Index Value`)

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
    selected_model = factor(selected_model, levels = c("low", "mid", "high"), ordered = TRUE),
    # non-selected columns per capita
    perio_exppc_high = (Mean_total_billions_high * 1e9) / Pop,
    perio_exppc_mid  = (Mean_total_billions_mid  * 1e9) / Pop,
    perio_exppc_low  = (Mean_total_billions_low  * 1e9) / Pop
  ) %>%
  mutate(
    # pick the two non-selected scenarios
    nonselected1 = case_when(
      selected_model == "low"  ~ perio_exppc_mid,
      selected_model == "mid"  ~ perio_exppc_low,
      selected_model == "high" ~ perio_exppc_mid
    ),
    nonselected2 = case_when(
      selected_model == "low"  ~ perio_exppc_high,
      selected_model == "mid"  ~ perio_exppc_high,
      selected_model == "high" ~ perio_exppc_low
    )
  ) %>%
  filter(!is.na(Dent_exppc_usd), !is.na(perio_exppc_usd))

# ---------------------------
# Discretise SDI
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
merged_data$SDI_cat <- factor(merged_data$SDI_cat, levels = sdi_labels, ordered = TRUE)

# Prepare choices
all_superregions <- sort(unique(na.omit(merged_data$Superregion)))
all_regions <- sort(unique(na.omit(merged_data$Region)))
all_countries <- sort(unique(na.omit(merged_data$Country)))

model_colors <- c("low"  = "#3182bd", "mid"  = "#fed976", "high" = "#de2d26")

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Periodontal vs Dental Expenditure (interactive)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Filter by Superregion, Region, or Country. Click points to focus on a country."),
      selectInput("superregion", "Superregion", choices = c("All", all_superregions), selected = "All"),
      selectInput("region", "Region", choices = c("All", all_regions), selected = "All"),
      selectizeInput("countries", "Country (multi-select)", choices = all_countries,
                     selected = NULL, multiple = TRUE,
                     options = list(placeholder = 'Select countries (optional)')),
      hr(),
      checkboxInput("show_points", "Show points", value = TRUE),
      actionButton("reset_click", "Reset country selection"),
      width = 3
    ),
    mainPanel(
      h4("Periodontal vs Dental Expenditure per capita"),
      plotlyOutput("scatter", height = "700px"),
      br(),
      p("Click a point to focus on that country. Other countries are semi-transparent. Use 'Reset country selection' to clear.")
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # reactive value to store clicked country
  clicked_country <- reactiveVal(NULL)
  
  # Reset click
  observeEvent(input$reset_click, {
    clicked_country(NULL)
  })
  
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
  
  # Reactive filtered data with alpha for semi-transparent dimming
  filtered_data <- reactive({
    df <- merged_data
    # Sidebar filters
    if (input$superregion != "All") df <- df %>% filter(Superregion == input$superregion)
    if (input$region != "All") df <- df %>% filter(Region == input$region)
    if (!is.null(input$countries) && length(input$countries) > 0) df <- df %>% filter(Country %in% input$countries)
    # Add alpha column for transparency
    if (!is.null(clicked_country())) {
      df <- df %>% mutate(alpha_point = if_else(Country == clicked_country(), 1, 0.2))
    } else {
      df <- df %>% mutate(alpha_point = 1)
    }
    df
  })
  
  # Plot
  output$scatter <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    # Reshape non-selected for small points
    df_long <- df %>%
      pivot_longer(
        cols = c(nonselected1, nonselected2),
        names_to = "scenario",
        values_to = "perio_exppc_nonselected"
      )
    
    gp <- ggplot() +
      # Main points
      geom_point(data = df, aes(
        x = Dent_exppc_usd,
        y = perio_exppc_usd,
        color = selected_model,
        size = SDI_cat,
        alpha = alpha_point,
        key = Country,
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
      # Small non-selected points
      geom_point(data = df_long, aes(
        x = Dent_exppc_usd,
        y = perio_exppc_nonselected,
        alpha = alpha_point,
        key = Country,
        text = paste0(
          "<b>", Country, "</b><br>",
          "Region: ", Region, "<br>",
          "Superregion: ", Superregion, "<br>",
          "Utilisation scenario: Non-selected<br>",
          "Dental exp per capita (USD): ", scales::comma(Dent_exppc_usd, accuracy = 0.01), "<br>",
          "Periodontal exp per capita (USD): ", scales::comma(perio_exppc_nonselected, accuracy = 0.01)
        )
      ), color = "grey50", size = 2) +
      scale_x_log10(labels = scales::comma_format(scale = 1, suffix = " USD"),
                    name = "Dental Expenditure per Capita (log scale)") +
      scale_y_log10(labels = scales::comma_format(scale = 1, suffix = " USD"),
                    name = "Periodontal Expenditure per Capita (log scale)") +
      scale_color_manual(values = model_colors, name = "Utilisation Scenario") +
      scale_size_manual(values = c(1.5, 2.5, 3.5, 5, 6), name = "SDI Category") +
      scale_alpha(range = c(0.2, 1), guide = "none") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid = element_blank(),
        legend.position = "right",
        axis.line = element_line(colour = "black", linewidth = 0.3),
        axis.ticks = element_line(colour = "black", linewidth = 0.3),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black")
      )
    
    ggplotly(gp, tooltip = "text", dynamicTicks = TRUE) %>%
      event_register("plotly_click")
  })
  
  # Update clicked country when a point is clicked
  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click)) {
      clicked_country(click$key[1])  # key contains the Country
    }
  })
  
}

# ---------------------------
# Run the app
# ---------------------------
shinyApp(ui, server)


rsconnect::deployApp("D:/JT research collaboration/GBD Cost Effectiveness Projections/Perio_Expenditure/Perio_vs_Dental_Expenditure")
