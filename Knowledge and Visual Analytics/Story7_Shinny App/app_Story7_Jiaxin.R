# Story 7: Telling a Data Story with Shiny App
# Author: Jiaxin Zheng

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(marginaleffects)
library(plotly)
library(broom)

# 1. First need to load the Ames housing data, I use story 5's dataset
df <- read_csv(
  "https://raw.githubusercontent.com/Jennyjjxxzz/Data-608_Story_5/refs/heads/main/train.csv"
)

# 2. Set up the UI
ui <- dashboardPage(
  dashboardHeader(title = "What Drives Home Prices in Ames?"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Price vs Overall Quality", tabName = "multi", icon = icon("project-diagram")),
      menuItem("Predicted Price", tabName = "sim", icon = icon("sliders-h")),
      menuItem("Download", tabName = "download", icon = icon("download"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview
      tabItem("overview",
              fluidRow(column(8, h2("Story 7: What Drives Home Prices in Ames?")))
      ),
      
      # Data Explorer
      tabItem("explorer",
              fluidRow(
                box(width = 4, title = "Filters", status = "primary", solidHeader = TRUE,
                    selectInput("neighborhood", "Neighborhood",
                                choices = unique(df$Neighborhood),
                                multiple = TRUE),
                    sliderInput("yearBuilt", "Year Built",
                                min = min(df$YearBuilt),
                                max = max(df$YearBuilt),
                                value = c(min(df$YearBuilt),
                                          max(df$YearBuilt))),
                    selectInput("houseStyle", "House Style",
                                choices = unique(df$HouseStyle),
                                multiple = TRUE)
                ),
                box(width = 8, title = "Data & Plots", status = "primary", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Table", DTOutput("table_explorer")),
                      tabPanel("Price Histogram", plotOutput("hist_price")),
                      tabPanel("Scatter", plotlyOutput("scatter_explorer"))
                    )
                )
              )
      ),
      
      # Price vs Overall Quality
      tabItem("multi",
              fluidRow(
                box(width = 4, title = "Covariates", status = "info", solidHeader = TRUE,
                    checkboxGroupInput("multiCovars", "Select predictors:",
                                       choices = c("GrLivArea", "LotArea", "OverallQual",
                                                   "KitchenQual", "GarageArea", "YearBuilt"),
                                       selected = c("GrLivArea", "OverallQual", "GarageArea")
                    )
                ),
                box(width = 8, title = "Variable Importance", status = "info", solidHeader = TRUE,
                    plotOutput("multi_imp")
                )
              )
      ),
      
      # Predicted Price
      tabItem("sim",
              fluidRow(
                box(width = 4, title = "Inputs", status = "success", solidHeader = TRUE,
                    sliderInput("simQual", "OverallQual", min = 1, max = 10, value = 6),
                    numericInput("simArea", "GrLivArea (sq ft)",
                                 value = 1500, min = 300, max = 5000),
                    selectInput("simKitchen", "KitchenQual",
                                choices = unique(df$KitchenQual),
                                selected = "TA")
                ),
                box(width = 8, title = "Predicted Price", status = "success", solidHeader = TRUE,
                    h3(textOutput("sim_price"))
                )
              )
      ),
      
      # Download
      tabItem("download",
              fluidRow(
                box(width = 6, title = "Download Data", status = "danger", solidHeader = TRUE,
                    downloadButton("df", "Download the Data Set")
                )
              )
      )
    )
  )
)

# 3. Set up the Server
server <- function(input, output, session) {

  explorer_df <- reactive({
    df %>%
      filter(
        (is.null(input$neighborhood) | Neighborhood %in% input$neighborhood) &
          YearBuilt >= input$yearBuilt[1] & YearBuilt <= input$yearBuilt[2] &
          (is.null(input$houseStyle)   | HouseStyle %in% input$houseStyle)
      )
  })
  
  # Explorer outputs
  output$table_explorer <- renderDT(explorer_df())
  output$hist_price       <- renderPlot({
    explorer_df() %>%
      ggplot(aes(SalePrice)) +
      geom_histogram(
        bins = 30, fill = "steelblue", color = "white"
      ) +
      scale_x_continuous(labels = scales::dollar) +
      theme_minimal()
  })
  output$scatter_explorer <- renderPlotly({
    p <- explorer_df() %>%
      ggplot(
        aes(x = GrLivArea, y = SalePrice, size = LotArea)
      ) +
      geom_point(alpha = 0.7) +
      scale_x_continuous(
        labels = scales::comma,
        name   = "Living Area (sq. ft.)"
      ) +
      scale_y_continuous(
        labels = scales::dollar,
        name   = "Sale Price"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Multivariable model for Price vs Overall Quality
  multi_fit <- reactive({
    req(input$multiCovars)
    lm(reformulate(input$multiCovars, "SalePrice"), data = df)
  })
  output$multi_imp <- renderPlot({
    broom::tidy(multi_fit()) %>%
      filter(term != "(Intercept)") %>%
      mutate(imp = abs(estimate)) %>%
      arrange(desc(imp)) %>%
      ggplot(aes(x = reorder(term, imp), y = imp)) +
      geom_col(fill = "tomato") +
      coord_flip() +
      labs(
        x     = "Predictor",
        y     = "Sale Price",
        title = "Price vs Overall Quality"
      ) +
      theme_minimal()
  })
  
  # Simulator for Predicted Price
  sim_fit <- reactive({
    lm(SalePrice ~ OverallQual + GrLivArea + KitchenQual, data = df)
  })
  output$sim_price <- renderText({
    newdata <- tibble(
      OverallQual = input$simQual,
      GrLivArea   = input$simArea,
      KitchenQual = input$simKitchen
    )
    scales::dollar(predict(sim_fit(), newdata))
  })
  
  # Download output
  output$df <- downloadHandler(
    filename =   function() "story7_data.csv",
    content  =   function(file) write_csv(explorer_df(), file)
  )
}

shinyApp(ui, server)
