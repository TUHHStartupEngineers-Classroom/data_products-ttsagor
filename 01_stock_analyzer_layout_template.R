# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

source(file = "stock_analysis_functions.R")


stock_list_tbl <- get_stock_list(stock_index = "nasdaq")

stock_data_tbl <- "AAPL" %>% get_stock_data()


# UI ----
    ui <- fluidPage(
      title = "Stock Analyzer",
      
      # 1.0 HEADER ----
      div(
        h1("Stock Analzer")
      ), 
      
      # 2.0 APPLICATION UI -----
      div(
        column(
          width = 4,
          multiple = F, 
          actionsBox = FALSE,
          size = 10,
          wellPanel(
            strong("Stock List(Pick One to Abalyze)"),
            pickerInput(inputId = "indices",
                        choices = c("dax", "sp500", "dow", "nasdaq")),
            
            strong("Stock List(Pick One to Abalyze)"),
            
            uiOutput("indices"),
            
            dateRangeInput(inputId = "date_range", 
                           label   = h4("Date Range"), 
                           start   = "2018-01-01", 
                           end     = today(), 
                           startview = "year"),
            
            actionButton(inputId = "analyze", 
                         label   = "Analyze",
                         icon    = icon("download")),
            
            sliderInput(inputId = "short_moving_slider", 
                        label   = "Short Moving Average", 
                        min     = 5,
                        max     = 40, 
                        value   = c(20), 
                        step    = 1, 
                        round   = TRUE),
            
            sliderInput(inputId = "long_moving_slider", 
                        label   = "Long Moving Average", 
                        min     = 50,
                        max     = 120, 
                        value   = c(50), 
                        step    = 1, 
                        round   = TRUE)
            
          )
        ), 
        column(
          width = 8,
          div(
            div(h4(textOutput(outputId = "plot_header"))),
            div(plotlyOutput("output_map"))
          )
        )
      ), 
      
      div(
        column(
          width = 12,
          div(h4("Analyst Commentary"))
        )
      ),
      
      div(
        column(
          width = 12,
          p(textOutput(outputId = "analyst_commentary"))
        )
      )
    )



# 3.0 ANALYST COMMENTARY ----

# SERVER ----
    server <- function(input, output, session) {
      output$indices <- renderUI({
        stock_list_tbl <- get_stock_list(stock_index = input$indices)
        pickerInput(inputId = "stock_selection", 
                    choices = stock_list_tbl$label)
      })
      
      
      update_ui <- function() {
        stock_symbol <- eventReactive(input$analyze, {
          input$stock_selection
        })
        
        symbol <- strsplit(stock_symbol(), ", ")[[1]][1]
        
        output$plot_header <- renderText(stock_symbol())
        
        sma <- input$short_moving_slider
        
        lma <- input$long_moving_slider
        
        start_date = input$date_range[1] 
        end_date = input$date_range[2] 
        
        stock_data_table = get_stock_data(
          stock_symbol = symbol, 
          from = start_date, 
          to   = end_date,
          mavg_short = sma,
          mavg_long  = lma)
        
        output$output_map <- renderPlotly(stock_data_table %>%  plot_stock_data()) 
        
        output$analyst_commentary <- renderText(generate_commentary(data = stock_data_table, user_input = symbol))
      }
      
      observeEvent(eventExpr = input$analyze, handlerExpr = {
        update_ui()
      })
      
      observeEvent(eventExpr = input$short_moving_slider, handlerExpr = {
        update_ui()
      })
      
      observeEvent(eventExpr = input$long_moving_slider, handlerExpr = {
        update_ui()
      })
      
      observeEvent(eventExpr = input$date_range, handlerExpr = {
        update_ui()
      })
    }

# RUN APP ----
    shinyApp(ui = ui, server = server)
