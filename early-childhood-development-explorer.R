# Early Childhood Development Explorer

library(shiny)
library(tidyverse)
library(readr)
library(janitor)
library(plotly)

# Load and clean data
df <- read_csv("early_childhood_transformed_data.csv",
               show_col_types = FALSE) %>% 
  clean_names() %>% 
  # Fix UTF-8 encoding issues
  mutate(across(where(is.character),
                ~ iconv(.x, from = "UTF-8", to = "UTF-8")))

# Convert key variables to factors for plotting and filtering
df <- df %>%
  mutate(
    countries_and_areas = as.factor(countries_and_areas),
    equity_level = as.factor(equity_level),
    book_access_level = as.factor(book_access_level)
  )

# Numeric variables available for mapping, scatter plots, and summaries
num_vars <- c("books_total", "playthings_total", "stim_total", "ed_wealth_gap")


# User Interface
ui <- fluidPage(
  titlePanel("Early Childhood Development Explorer (UNICEF)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Dropdown for choosing specific countries
      selectInput("country_filter", "Select Countries",
                  choices = sort(unique(df$countries_and_areas)),
                  selected = sort(unique(df$countries_and_areas))[1],
                  multiple = TRUE),
      
      # Button to automatically select all countries
      actionButton("select_all_btn", "Select All Countries",
                   class = "btn btn-primary"),
      
      tags$hr(),
      
      # Inputs for selecting variables used in scatter plot axes
      selectInput("x_var", "X-axis Variable",
                  choices = num_vars,
                  selected = "books_total"),
      
      selectInput("y_var", "Y-axis Variable",
                  choices = num_vars,
                  selected = "stim_total"),
      
      # Variable selection for choropleth map coloring
      selectInput("map_var", "Map Variable",
                  choices = num_vars,
                  selected = "books_total"),
      
      # Checkboxes for selecting summary statistics variables
      checkboxGroupInput("summary_vars", "Summary Table Variables",
                         choices = num_vars,
                         selected = num_vars)
    ),
    
    mainPanel(
      width = 8,
      
      # Tabs for each visualization component
      tabsetPanel(
        tabPanel("Interactive Map", plotlyOutput("plotly_map", height = "600px")),
        tabPanel("Bar Chart", plotOutput("bar_plot")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot")),
        tabPanel("Summary Table", tableOutput("summary_table"))
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  
  # When "Select All Countries" is clicked, update dropdown selection
  observeEvent(input$select_all_btn, {
    updateSelectInput(
      session,
      "country_filter",
      selected = sort(unique(df$countries_and_areas))
    )
  })
  
  # Filter dataset based on selected countries
  filtered_data <- eventReactive(input$country_filter, {
    df %>%
      filter(countries_and_areas %in% input$country_filter)
  })
  
  # Create summary table for bar chart (mean book access by category)
  bar_summary <- reactive({
    filtered_data() %>%
      group_by(book_access_level) %>%
      summarise(mean_books = mean(books_total, na.rm = TRUE))
  })
  
  # Bar Chart
  output$bar_plot <- renderPlot({
    ggplot(bar_summary(), aes(x = book_access_level, y = mean_books)) +
      geom_col(fill = "steelblue", color = "black") +
      labs(
        title = "Average Book Access by Book Access Level",
        x = "Book Access Level",
        y = "Average % of Children with 3+ Books"
      ) +
      theme_minimal()
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(size = 3, color = "steelblue", alpha = 0.8) +
      # Add regression trend line
      geom_smooth(method = "lm", color = "gray20", se = FALSE) +
      labs(
        title = paste("Scatter Plot of Child Engagement vs Books"),
        x = "Percent of Children with 3+ Books (books_total)",
        y = "Percent of Children Engaged in 4+ Activities (stim_total)"
      ) +
      theme_minimal()
  })
  
  # Summary Table
  output$summary_table <- renderTable({
    filtered_data() %>%
      select(all_of(input$summary_vars)) %>% 
      summary() %>% 
      as.data.frame()
  }, rownames = TRUE)
  
  # Plotly Choropleth Map
  output$plotly_map <- renderPlotly({
    
    data <- filtered_data()
    map_var <- input$map_var
    
    # Prepare dataset for mapping: country label + selected metric
    df_map <- data %>%
      transmute(
        countries_and_areas = as.character(countries_and_areas),
        value = .data[[map_var]]
      )
    
    # Build interactive choropleth map
    plot_ly(
      df_map,
      type = "choropleth",
      locations = ~countries_and_areas,
      locationmode = "country names",
      z = ~value,
      colorscale = "Viridis",
      marker = list(line = list(color = "gray20", width = 0.5)),
      # Hover labels with country name + value
      text = ~paste0(
        "<b>", countries_and_areas, "</b><br>",
        map_var, ": ", round(value, 1), "%"
      ),
      hoverinfo = "text"
    ) %>%
      colorbar(title = map_var) %>% 
      layout(
        title = paste("Interactive World Map of", map_var),
        geo = list(
          projection = list(type = "natural earth"),
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "gray50",
          landcolor = "lightgray"
        )
      )
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)
