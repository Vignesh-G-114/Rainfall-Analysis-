
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# Sample rainfall data for three years
rainfall_data <- data.frame(
  MonthNo = rep(1:12, 3),
  Month = rep(month.name, 3),
  Year = rep(2024:2026, each = 12),
  Rainfall = c(
    35, 40, 65, 90, 160, 230, 280, 260, 200, 130, 70, 40,
    45, 55, 75, 110, 190, 270, 320, 290, 240, 160, 85, 50,
    40, 50, 70, 100, 180, 250, 300, 280, 220, 150, 80, 45
  )
)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Rainfall Intelligence"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard",
               icon = icon("dashboard")),
      menuItem("Prediction", tabName = "prediction",
               icon = icon("chart-line")),
      menuItem("Add Rainfall", tabName = "add",
               icon = icon("plus-circle")),
      menuItem("Data Reports", tabName = "reports",
               icon = icon("file-download"))
    ),
    
    hr(),
    
    selectInput("year", "Select Year",
                choices = c("All", 2024, 2025, 2026),
                selected = "2026"),
    
    sliderInput("threshold", "Heavy Rainfall Limit (mm)",
                min = 50, max = 400,
                value = 200, step = 10),
    
    sliderInput("drought", "Low Rainfall Limit (mm)",
                min = 0, max = 150,
                value = 50, step = 5)
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f1f5f9; }
      .box, .small-box { border-radius: 10px; }
    "))),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("total", width = 3),
          valueBoxOutput("average", width = 3),
          valueBoxOutput("highest", width = 3),
          valueBoxOutput("alerts", width = 3)
        ),
        
        fluidRow(
          box(title = "Monthly Rainfall Trend",
              width = 8, status = "primary",
              plotOutput("trend", height = 320)),
          
          box(title = "Rainfall Categories",
              width = 4, status = "success",
              plotOutput("categories", height = 320))
        ),
        
        fluidRow(
          box(title = "Year Comparison",
              width = 6, status = "info",
              plotOutput("compare", height = 300)),
          
          box(title = "Weather Alerts",
              width = 6, status = "warning",
              textOutput("alertText"),
              br(),
              textOutput("droughtText"))
        )
      ),
      
      tabItem(
        tabName = "prediction",
        
        fluidRow(
          box(title = "Rainfall Prediction",
              width = 12, status = "primary",
              p("Estimates the next three months using a linear trend model."),
              plotOutput("forecastPlot", height = 350),
              tableOutput("forecastTable"),
              p("Prediction is based on sample historical data and is not a weather forecast.")
          )
        )
      ),
      
      tabItem(
        tabName = "add",
        
        fluidRow(
          box(title = "Enter Rainfall Record",
              width = 6, status = "primary",
              
              selectInput("newMonth", "Month",
                          choices = month.name),
              
              numericInput("newYear", "Year",
                           value = 2027,
                           min = 1900, max = 2100),
              
              numericInput("newRain", "Rainfall (mm)",
                           value = 100,
                           min = 0, max = 5000),
              
              actionButton("addRecord", "Add Record",
                           icon = icon("plus"),
                           class = "btn-success")
          ),
          
          box(title = "Rainfall Insights",
              width = 6, status = "info",
              textOutput("insight"))
        )
      ),
      
      tabItem(
        tabName = "reports",
        
        fluidRow(
          box(title = "Rainfall Records",
              width = 12, status = "primary",
              
              textInput("search", "Search Month or Year", ""),
              
              DTOutput("rainTable"),
              
              br(),
              
              downloadButton("download", "Download CSV")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rainfall <- reactiveVal(rainfall_data)
  
  # Update available years when records change
  observe({
    years <- sort(unique(rainfall()$Year))
    
    updateSelectInput(
      session, "year",
      choices = c("All", years),
      selected = if (input$year %in% c("All", as.character(years))) {
        input$year
      } else {
        as.character(max(years))
      }
    )
  })
  
  # Selected year data
  selectedData <- reactive({
    d <- rainfall()
    
    if (input$year != "All") {
      d <- d[d$Year == as.numeric(input$year), ]
    }
    
    d[order(d$MonthNo), ]
  })
  
  # Add categories and alerts
  analysisData <- reactive({
    d <- selectedData()
    
    d$Category <- ifelse(
      d$Rainfall >= input$threshold, "Heavy",
      ifelse(d$Rainfall <= input$drought, "Low", "Normal")
    )
    
    d
  })
  
  # Dashboard cards
  output$total <- renderValueBox({
    valueBox(
      paste(round(sum(selectedData()$Rainfall), 1), "mm"),
      "Total Rainfall",
      icon = icon("cloud-rain"),
      color = "blue"
    )
  })
  
  output$average <- renderValueBox({
    valueBox(
      paste(round(mean(selectedData()$Rainfall), 1), "mm"),
      "Average Rainfall",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$highest <- renderValueBox({
    d <- selectedData()
    best <- d[which.max(d$Rainfall), ]
    
    valueBox(
      paste(best$Month, best$Rainfall, "mm"),
      "Highest Rainfall",
      icon = icon("cloud-showers-heavy"),
      color = "purple"
    )
  })
  
  output$alerts <- renderValueBox({
    n <- sum(selectedData()$Rainfall >= input$threshold)
    
    valueBox(
      n, "Heavy Rainfall Months",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Monthly trend
  output$trend <- renderPlot({
    d <- analysisData()
    
    ggplot(d, aes(MonthNo, Rainfall)) +
      geom_col(fill = "steelblue") +
      geom_hline(yintercept = input$threshold,
                 color = "red", linetype = "dashed") +
      scale_x_continuous(breaks = 1:12,
                         labels = month.abb) +
      labs(x = "Month", y = "Rainfall (mm)") +
      theme_minimal()
  })
  
  # Rainfall categories
  output$categories <- renderPlot({
    d <- analysisData()
    
    counts <- as.data.frame(table(d$Category))
    names(counts) <- c("Category", "Count")
    
    ggplot(counts, aes(Category, Count, fill = Category)) +
      geom_col() +
      labs(x = "Category", y = "Number of Months") +
      theme_minimal()
  })
  
  # Compare yearly rainfall
  output$compare <- renderPlot({
    d <- rainfall()
    
    ggplot(d, aes(MonthNo, Rainfall,
                  color = factor(Year), group = Year)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(breaks = 1:12,
                         labels = month.abb) +
      labs(x = "Month", y = "Rainfall (mm)",
           color = "Year") +
      theme_minimal()
  })
  
  # Weather alerts
  output$alertText <- renderText({
    d <- selectedData()
    months <- d$Month[d$Rainfall >= input$threshold]
    
    if (!length(months)) {
      return("No heavy rainfall months detected.")
    }
    
    paste("Heavy rainfall:", paste(months, collapse = ", "))
  })
  
  output$droughtText <- renderText({
    d <- selectedData()
    months <- d$Month[d$Rainfall <= input$drought]
    
    if (!length(months)) {
      return("No months below the low-rainfall limit.")
    }
    
    paste("Low rainfall warning:", paste(months, collapse = ", "))
  })
  
  # Prediction using linear regression
  forecastData <- reactive({
    d <- selectedData()
    
    validate(need(nrow(d) >= 2,
                  "At least two records are needed for prediction."))
    
    model <- lm(Rainfall ~ MonthNo, data = d)
    
    future <- data.frame(MonthNo = 13:15)
    
    future$PredictedRainfall <- pmax(
      0, as.numeric(predict(model, newdata = future))
    )
    
    future$Month <- c("Next Month 1",
                      "Next Month 2",
                      "Next Month 3")
    
    future
  })
  
  output$forecastPlot <- renderPlot({
    f <- forecastData()
    
    ggplot(f, aes(Month, PredictedRainfall)) +
      geom_col(fill = "darkorange") +
      labs(x = "Future Period",
           y = "Predicted Rainfall (mm)") +
      theme_minimal()
  })
  
  output$forecastTable <- renderTable({
    f <- forecastData()
    
    data.frame(
      Month = f$Month,
      `Predicted Rainfall (mm)` =
        round(f$PredictedRainfall, 1),
      check.names = FALSE
    )
  })
  
  # Add rainfall record
  observeEvent(input$addRecord, {
    d <- rainfall()
    
    if (any(d$Year == input$newYear &
            d$Month == input$newMonth)) {
      showNotification(
        "A record already exists for this month and year.",
        type = "error"
      )
      return()
    }
    
    newRow <- data.frame(
      MonthNo = match(input$newMonth, month.name),
      Month = input$newMonth,
      Year = input$newYear,
      Rainfall = input$newRain
    )
    
    rainfall(rbind(d, newRow))
    
    showNotification("Rainfall record added successfully!")
  })
  
  # Dynamic insight
  output$insight <- renderText({
    d <- selectedData()
    
    paste(
      "Total:", sum(d$Rainfall), "mm |",
      "Maximum:", max(d$Rainfall), "mm |",
      "Minimum:", min(d$Rainfall), "mm"
    )
  })
  
  # Searchable data table
  output$rainTable <- renderDT({
    d <- analysisData()
    
    if (nzchar(input$search)) {
      keep <- grepl(input$search, d$Month, ignore.case = TRUE) |
        grepl(input$search, as.character(d$Year))
      
      d <- d[keep, ]
    }
    
    datatable(d, rownames = FALSE,
              options = list(pageLength = 12))
  })
  
  # Export complete dataset
  output$download <- downloadHandler(
    filename = function() {
      paste0("rainfall_report_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rainfall(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)