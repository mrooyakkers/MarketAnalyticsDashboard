rm(list=ls())

library(shiny)
library(shinydashboard)
library(plotly)

# Shiny Function
Marketing_Shiny <- function (imported_data, input) {

  # Marketing Analytics Project
  customer_total_purchases <- NULL
  id <- unique(marketing_data$ID)
  for (i in 1:length(id)) {
    id_code <- id[i]
    current <- marketing_data[marketing_data$ID == id_code,]
    total_purchases <- sum(current$NumCatalogPurchases, current$NumDealsPurchases, 
                           current$NumStorePurchases, current$NumWebPurchases)
    customer_total_purchases <- c(customer_total_purchases, total_purchases)
  }
  
  # Potential Dependents 
  Total_Purchases <- customer_total_purchases
  Sale_Purchases <- marketing_data$NumDealsPurchases
  Catologue_Purchases <- marketing_data$NumCatalogPurchases
  Store_Purchases <- marketing_data$NumStorePurchases
  Online_Purchases <- marketing_data$NumWebPurchases
  
  # Potential Independents
  Age <- 2021 - marketing_data$Year_Birth
  Income <- marketing_data$Income
  Income <-  as.numeric(gsub('[$,]', '', Income))
  
  Number_Kids <- marketing_data$Kidhome
  Number_Teens <- marketing_data$Teenhome
  
  # Categorical Independents
  Country <- marketing_data$Country
  Education <- marketing_data$Education
  Marital_Status <- marketing_data$Marital_Status
  
  # Make child binary 
  Children_YN <- NULL
  for (i in 1:length(id)) {
    id_code <- id[i]
    current <- marketing_data[marketing_data$ID == id_code,]
    
    if (current$Kidhome >= 1 | current$Teenhome >= 1) {
      Children_YN <- c(Children_YN, 1)
    }
    
    else {
      Children_YN <- c(Children_YN, 0)
    }
  }
  

  # Shiny Script
  # User Interface
  ui <- shiny::pageWithSidebar(
    shiny::headerPanel(title = "Marketing Analytics Dashboard"),
    
    # Making a sidebar where the user can indicate the specific time they are interested in
    # and a checklist where they can index what statistics they would like to be reported on
    # the specified time frame.
    shiny::sidebarPanel((" "),
                        shiny::checkboxGroupInput("Ind_ID", "Predictors:",
                                                  choices = c("Age","Income","Number of Children", 
                                                              "Number of Teens", "Country", "Education",
                                                              "Marital Status", "Children (Y/N)")),
                        
                        shiny::checkboxGroupInput("Dep_ID", "Outcome:",
                                                  choices = c("Total Purchases","Online Purchases","Sale Purchases", 
                                                              "Catologue Purchases", "In-Store Purchases"))
                        
                        
    ),
    
    # The plot and the statistics should be on the main panel / right side of the app.
    shiny::mainPanel(
      shiny::fluidRow(
        plotly::plotlyOutput('plot1'),
        shinydashboard::box(
          title = "Statistics Output",
          shiny::tableOutput("StatisticsOutput")
        )
      )
    )
  )

  # Server
  server <- function(input, output) {
    
    # Plot Output 
    output$plot1 <- plotly::renderPlotly({
      
      # I need a vector that selects for the selected inputs 
      constrained_data_ind <- as.data.frame(1:2240)
      if ("Age" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Age)
      }
      if ("Income" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Income)
      }
      if ("Number of Children" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Number_Kids)
      }
      if ("Number of Teens" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Number_Teens)
      }
      if ("Country" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Country)
      }
      if ("Education" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Education)
      }
      if ("Marital Status" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Marital_Status)
      }
      if ("Children (Y/N)" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Children_YN)
      }
      
      # dependent variables
      constrained_data_dep <- as.data.frame(1:2240)
      if ("Total Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Total_Purchases)
      }
      if ("Online Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Online_Purchases)
      }
      if ("Sale Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Sale_Purchases)
      }
      if ("Catologue Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Catologue_Purchases)
      }
      if ("In-Store Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Store_Purchases)
      }
      
      constrained_data <- cbind(constrained_data_ind, constrained_data_dep)
      x <- constrained_data[,2]
      y <- constrained_data[,4]
      plot1 <- plotly::plot_ly(constrained_data, x = constrained_data[,2],
                              y = constrained_data[,4], type = "scatter", mode = "markers")
      fit <- lm(constrained_data[,4] ~ constrained_data[,2])
      plotly::add_lines(constrained_data, x = constrained_data[,2], y = constrained_data[,4], line = fitted(fit), name = "Linear")
      
      plot1
      
      })
    
    # Statistics Table Calculations and Output
    output$StatisticsOutput <- shiny::renderTable({
      
      table <- data.frame(matrix(ncol = 4, nrow = 1, dimnames = list (NULL, c("Estimate", "R", "P-Value", "Evaluation"))))
      
      # I need a vector that selects for the selected inputs 
      constrained_data_ind <- as.data.frame(1:2240)
      if ("Age" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Age)
      }
      if ("Income" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Income)
      }
      if ("Number of Children" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Number_Kids)
      }
      if ("Number of Teens" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Number_Teens)
      }
      if ("Country" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Country)
      }
      if ("Education" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Education)
      }
      if ("Marital Status" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Marital_Status)
      }
      if ("Children (Y/N)" %in% input$Ind_ID) {
        constrained_data_ind <- cbind(constrained_data_ind, Children_YN)
      }
      # dependent variables
      constrained_data_dep <- as.data.frame(1:2240)
      if ("Total Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Total_Purchases)
      }
      if ("Online Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Online_Purchases)
      }
      if ("Sale Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Sale_Purchases)
      }
      if ("Catologue Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Catologue_Purchases)
      }
      if ("In-Store Purchases" %in% input$Dep_ID) {
        constrained_data_dep <- cbind(constrained_data_dep, Store_Purchases)
      }
      constrained_data <- cbind(constrained_data_ind, constrained_data_dep)
      x <- constrained_data[,2]
      y <- constrained_data[,4]
      
      # Specifying the min/max values that should be applied to the data before
      # calculating statistics
      fit <- summary(lm(y~x))
      # P value evaluator 
      Predictor <- NULL
      PVal <- fit$coefficients[2,4]
      if (PVal < 0.001) {
        Predictor <- "Great Predictor"
      } else if (PVal < 0.01) {
        Predictor <- "Good Predictor"
      } else if (PVal < 0.05) {
        Predictor <- "Moderate Predictor"
      } else {
        Predictor <- "Poor Predictor"
      }
      data <- c(round(fit$coefficients[2,1],5), round(fit$adj.r.squared,5), round(fit$coefficients[2,4],5), Predictor)
      table <- rbind(table, data)
      table <- table[-1,]
      
      table
      
    })
   
}
  
shiny::shinyApp(ui, server)
  
}
