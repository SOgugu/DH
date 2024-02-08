# PART 2

library(shiny)
library(tidyverse)
library(plotly)
library(RColorBrewer)

# Load data
mi_prevalence <- read.csv("mental_illness_prevalence.csv")

# Rename dataset column
mi_prevalence <- mi_prevalence %>%
  rename(Country = Entity)

# Filter for Germany
germany_data <- subset(mi_prevalence, Country == "Germany")

# Filter for selected countries
selected_countries <- c("Austria", "Belgium", "Poland", "Denmark", "Czechia", "France", "Netherlands")
selected_data <- mi_prevalence %>% filter(Country %in% selected_countries)

# Convert the column to numeric if necessary
selected_data$Depressive.disorders <- as.numeric(selected_data$Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized)

# Define UI for Shiny app
ui <- fluidPage(
  titlePanel("Depression Trends in Germany and Neighbouring Countries (1990-2019)"),
  plotlyOutput("linePlot")
)

# Define server logic
server <- function(input, output) {
  
  # Render the line plot
  output$linePlot <- renderPlotly({
    
    color_palette <- colorRampPalette(brewer.pal(8, "Set1"))(length(selected_countries))
    
    plot_ly() %>%
      add_trace(
        data = germany_data,
        x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized,
        type = 'scatter', mode = 'lines', name = 'Germany', line = list(color = color_palette[1])
      ) %>%
      add_trace(
        data = selected_data,
        x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized,
        type = 'scatter', mode = 'lines', name = selected_countries[1],
        line = list(color = color_palette[2])
      ) %>%
      add_trace(
        data = selected_data %>% filter(Country == selected_countries[2]),
        x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized,
        type = 'scatter', mode = 'lines', name = selected_countries[2],
        line = list(color = color_palette[3])
      ) %>%
      add_trace(
        data = selected_data %>% filter(Country == selected_countries[3]),
        x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized,
        type = 'scatter', mode = 'lines', name = selected_countries[3],
        line = list(color = color_palette[4])
      ) %>%
      add_trace(
        data = selected_data %>% filter(Country == selected_countries[4]),
        x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized,
        type = 'scatter', mode = 'lines', name = selected_countries[4],
        line = list(color = color_palette[5])
      ) %>%
      add_trace(
        data = selected_data %>% filter(Country == selected_countries[5]),
        x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized,
        type = 'scatter', mode = 'lines', name = selected_countries[5],
        line = list(color = color_palette[6])
      ) %>%
      add_trace(
        data = selected_data %>% filter(Country == selected_countries[6]),
        x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized,
        type = 'scatter', mode = 'lines', name = selected_countries[6],
        line = list(color = color_palette[7])
      ) %>%
      add_trace(
        data = selected_data %>% filter(Country == selected_countries[7]),
        x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized,
        type = 'scatter', mode = 'lines', name = selected_countries[7],
        line = list(color = color_palette[8])
      ) %>%
      # Repeat the add_trace block for each additional country in selected_countries
      
      layout(
        title = list(
          text = "The World Health Organization (WHO) reported almost 1 billion people worldwide had a mental disorder in 2019.
          In 2019, 970 million people around the world were living with anxiety and depressive disorders. 
          Mental disorders are the leading cause of disability globally, resulting in 1 to 5 years lived with disability.",
          font = list(size = 10)  # Adjust the font size as needed
        ),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Depression Scores"),
        
        legend = list(x = 1, y = 0.5, xanchor = "left", yanchor = "middle")
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)
