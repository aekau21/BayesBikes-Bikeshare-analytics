library(knitr)
library(tidyverse)
library(here)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(bslib)
library(bsicons)
library(plotly)
library(bayesrules)
library(dashboardthemes)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(janitor)
library(broom.mixed)
library(ggpubr)
library(ggcorrplot)
library(lubridate)

# Load the data
data(bikes)

# Prepare the data
bikes$date <- as.Date(bikes$date, format = "%Y-%m-%d")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Bayes Bikes Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About The Data", tabName = "dashboard", icon = icon("database")),
      menuItem("Time Series Analysis", tabName = "time_series", icon = icon("chart-line")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("chart-bar")),
      menuItem("Humidity & Rides", tabName = "humidity_rides", icon = icon("water")),
      menuItem("Bayesian Models", tabName = "bayesian_models", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    skin = "blue",
    dashboardthemes::shinyDashboardThemes(theme = "blue_gradient"),
    tabItems(
      # About the Data Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Dataset Overview", status = "primary", solidHeader = TRUE,
                    dataTableOutput("data_table")
                )
              )
      ),
      
      # Time Series Analysis Tab
      tabItem(tabName = "time_series",
              fluidRow(
                box(title = "Rides Over Time", status = "primary", solidHeader = TRUE,
                    plotlyOutput("rides_time_series")
                ),
                box(title = "Rides vs Temperature", status = "primary", solidHeader = TRUE,
                    plotlyOutput("rides_temp_scatter")
                )
              )
      ),
      
      # Correlation Analysis Tab
      tabItem(tabName = "correlation",
              fluidRow(
                box(title = "Correlation Heatmap", status = "primary", solidHeader = TRUE,
                    plotOutput("correlation_heatmap")
                )
              )
      ),
      
      # Humidity & Rides Tab
      tabItem(tabName = "humidity_rides",
              fluidRow(
                box(title = "Average Rides by Humidity Levels", status = "primary", solidHeader = TRUE,
                    plotlyOutput("humidity_bar_plot")
                ),
                box(title = "Rides vs Humidity & Windspeed", status = "primary", solidHeader = TRUE,
                    plotlyOutput("humidity_scatter_plot")
                )
              )
      ),
      
      # Bayesian Models Tab
      tabItem(tabName = "bayesian_models",
              fluidRow(
                box(title = "Simple Bayesian Model (Rides vs Feels Like Temperature)", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("bike_model_summary")
                ),
                box(title = "Comprehensive Bayesian Model Prediction", status = "primary", solidHeader = TRUE,
                    plotOutput("bayesian_prediction_plot")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  # Data Table
  output$data_table <- renderDataTable({
    bikes
  })
  
  # Time Series Plot
  output$rides_time_series <- renderPlotly({
    p <- ggplot(bikes, aes(x = date, y = rides)) +
      geom_line(color = "steelblue") +
      labs(title = "Trend of Rides Over Time", x = "Date", y = "Total Rides")
    ggplotly(p)
  })
  
  # Rides vs Temperature Scatter Plot
  output$rides_temp_scatter <- renderPlotly({
    p <- ggplot(bikes, aes(x = date, y = rides, color = temp_actual, shape = weekend)) +
      geom_point() +
      labs(title = "Rides vs Temperature", x = "Date", y = "Total Rides")
    ggplotly(p)
  })
  
  # Correlation Heatmap
  output$correlation_heatmap <- renderPlot({
    cor_matrix <- cor(bikes[, c("temp_actual", "temp_feel", "humidity", "windspeed", "rides")], 
                      use = "complete.obs")
    ggcorrplot(cor_matrix, lab = TRUE, colors = c("red", "white", "blue"))
  })
  
  # Humidity Bar Plot
  output$humidity_bar_plot <- renderPlotly({
    p <- bikes %>%
      ggplot(aes(x = factor(round(humidity, -1)), y = rides)) +
      geom_bar(stat = 'summary', fun = 'mean', fill = 'skyblue') +
      labs(
        title = 'Average Number of Rides by Humidity Levels',
        x = 'Humidity (%) (Grouped)',
        y = 'Average Number of Rides'
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Humidity Scatter Plot
  output$humidity_scatter_plot <- renderPlotly({
    p <- bikes %>%
      ggplot(aes(x = humidity, y = rides, color = windspeed)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = 'lm', se = FALSE, color = 'red') +
      labs(
        title = 'Scatter Plot of Rides vs. Humidity with Windspeed Indication',
        x = 'Humidity (%)',
        y = 'Number of Rides',
        color = 'Windspeed (m/s)'
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Bayesian Model
  bike_model <- stan_glm(rides ~ temp_feel, data = bikes, 
                         family = gaussian, 
                         prior_intercept = normal(5000, 1000), 
                         prior = normal(100, 40), 
                         prior_aux = exponential(0.0008), 
                         chains = 4, 
                         iter = 5000*2, 
                         seed = 84735)
  
  # Model Summary
  output$bike_model_summary <- renderPrint({
    print(bike_model)
  })
  
  # Comprehensive Bayesian Model
  bike_model_all <- stan_glm(
    rides ~ season + year + month + day_of_week + weekend + holiday + 
      temp_actual + temp_feel + humidity + windspeed + weather_cat, 
    data = bikes, 
    family = gaussian, 
    chains = 4, 
    iter = 5000, 
    seed = 84735
  )
  
  # Prediction for a specific scenario
  new_data_for_prediction <- data.frame(
    temp_feel = 90, 
    temp_actual = 85, 
    season = "fall", 
    year = 2011, 
    month = "Oct", 
    day_of_week = "Wed", 
    weekend = FALSE, 
    holiday = "no", 
    humidity = 10, 
    windspeed = 17, 
    weather_cat = "categ1"
  )
  
  # Bayesian Prediction Plot
  output$bayesian_prediction_plot <- renderPlot({
    prediction <- posterior_predict(bike_model_all, newdata = new_data_for_prediction)
    mcmc_dens(prediction) + xlab("New Prediction of Rides")
  })
}

# Run the application 
shinyApp(ui, server)
