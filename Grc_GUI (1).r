
## If you don't have the libraries uncomment and install
# install.packages("arules")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("flexdashboard")
# install.packages("scales")
# install.packages("plotly")
# install.packages("shiny")
# install.packages("shinydashboard")

library(arules)
library(readxl)
library(ggplot2)
library(dplyr)
library(flexdashboard)
library(scales)
library(plotly)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "GRC Analysis"),
  dashboardSidebar(
    fileInput("file", "Upload CSV file")
  ),
  
  dashboardBody(
    fluidRow(
      box(tableOutput("dataTable"), title = "Data" ,width = 6,style = "overflow-x: scroll;", collapsible = TRUE ),
      verbatimTextOutput("dataCleaningInfo" ,)
    ),
    fluidRow(
      box(plotlyOutput("clusterPlot"), title = "K-means Clustering",
          collapsible = TRUE ),
      box(collapsible = TRUE ,title = "Number Of Clusters" , selectInput("clusters", "Number of Clusters", choices = c(2, 3, 4), selected = 2) )
      ,box( collapsible = TRUE ,title = "Centers" , tableOutput("kmeans_result_table"))
      ),
    fluidRow(
      box(collapsible = TRUE ,title = "Association Rules" , tableOutput("rulesTable"))
      ,
      box(
        collapsible = TRUE ,
        title = "Select Minumum Support And Confidence ",        
        sliderInput("support", "Minimum Support", min = 0.001, max = 1, value = 0.1) ,
        sliderInput("confidence", "Minimum Confidence", min = 0.001, max = 1, value = 0.1)
      ),
      ),
    fluidRow(
      box(collapsible = TRUE ,title = "Cash Vs Credit Totals" , plotlyOutput("cash_vs_credit") ),
      box(collapsible = TRUE ,title = "Age And Total Spening" , plotlyOutput("total_age")),
  ) ,
  fluidRow(
    box( collapsible = TRUE ,title = "Totals By City" , plotlyOutput("total_city") ),
    box(collapsible = TRUE ,title = "Total Spending " , plotlyOutput("total_dist")),
  ) 
  

)
)

# Define server logic
server <- function(input, output, session) {
  # Load the data from the uploaded CSV file
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    return(df)
  })

  data_clean <- function(){
    data_unique <- unique(data())
    
    
    # Convert columns to appropriate data types
    data_unique$count <- as.integer(data_unique$count)
    data_unique$age <- as.integer(data_unique$age)
    data_unique$rnd <- as.integer(data_unique$rnd)
    data_unique$total <- as.integer(data_unique$total)
    data_cleaned <- na.omit(data_unique)
    # Print cleaned data
    
    return(data_unique)
  }
  data_grouped <- function(){
    req(data())
    req(data_clean())
    grouped_data <- data_clean() %>%
      group_by(customer) %>%
      summarise(
        total_transactions = n(),              # Total number of transactions per customer
        total_amount = sum(total),             # Total amount spent by each customer
        avg_age = mean(age),                   # Average age of customers
        count = sum(count),               # Average count of items per transaction for each customer
        city = unique(city)                    # City associated with the customer (assuming one city per customer)
        # Add more summary statistics as needed
      )
    
      return(grouped_data)
        
  }
  
  kmean_results <- function(){
    req(data())
    req(data_clean())
    req(data_grouped())
    
    columns_to_cluster <- data_grouped()[c("avg_age", "total_amount")]
    kmeans_result <- kmeans(columns_to_cluster, centers = input$clusters)
    
    return (kmeans_result)
  }
  
    
  # Data cleaning plot
  output$dataTable <- renderTable({
      req(data())
      head(data(),8)
    
    })
  
  # Data cleaning information
  # Data cleaning and information
  output$dataCleaningInfo <- renderPrint({
    req(data())
    # Display structure of the data
    cat("Structure of the data:\n")
    str(data())
    # Check for duplicates
    duplicates <- data()[duplicated(data()), ]
    duplicates_count <- nrow(duplicates)
     nan_count <- sum(is.nan(unlist(data())))
    missing_count <- sum(is.na(unlist(data())))
    
    # Generate cleaning information message
    cat("\nFound", duplicates_count, "duplicates and removed them.",
                     "Found", nan_count, "NaNs and", missing_count, "missing values.\n")
    cat("\nData After Cleaning :\n")
    str(data_clean())
  })
  
  # K-means clustering plot
  output$clusterPlot <- renderPlotly({
    req(data())
    req(data_clean())
    req(data_grouped())
    req(kmean_results())
    columns_to_cluster <- data_grouped()[c("avg_age", "total_amount")]
    plot_ly(x = columns_to_cluster$avg_age, y = columns_to_cluster$total_amount, color = factor(kmean_results()$cluster),
            type = "scatter", mode = "markers", marker = list(size = 10)) %>%
      layout(title = "K-means Clustering", xaxis = list(title = "Age"), yaxis = list(title = "Total Spending"))
    
      })
  output$kmeans_result_table <- renderTable({
    req(data())
    req(data_clean())
    req(data_grouped())
    req(kmean_results())
    
    columns_to_cluster <- data_grouped() %>% select(avg_age, total_amount)
    result_df <- data_grouped()[c("customer" , "avg_age" , "total_amount")]
    kmeans_result_df <- cbind(result_df, cluster = kmean_results()$cluster)
    return(kmeans_result_df)
    })
  
  # Association rules table
  output$rulesTable <- renderTable({
    req(data())
    req(data_clean())
    
    # Generate association rules
    items <- data_clean()$items
    items_list <- strsplit(items, ",")
    transactions <- as(items_list, "transactions")
    rules <- apriori(transactions, parameter = list(support = input$support, confidence = input$confidence))
    
    # Convert rules to data frame
    rules_df <- as(rules, "data.frame")
    
    # Subset to include only the top 15 rules based on support
    rules_df <- rules_df[order(rules_df$support, decreasing = TRUE), ]
    rules_df <- rules_df[1:min(15, nrow(rules_df)), ]
    
    return(rules_df)
  })
output$cash_vs_credit <- renderPlotly(
  
  {
    req(data())
    req(data_clean())
    df <- data_clean()
    totals_by_payment <- aggregate(total ~ paymentType, data = df, FUN = sum)
    totals_by_payment
    
    plot_ly(totals_by_payment, labels = ~paymentType, values = ~total, type = 'pie', text = ~paymentType,
            marker = list(colors = c("#98c1d9", "#ee6c4d"))) 
    
    
  } 
  
)

output$total_age<- renderPlotly(
  
  {
    req(data())
    req(data_clean())
    req(data_grouped())
    ggplot(data_grouped(), aes(x = avg_age, y = total_amount)) +
      geom_point(color = "#98c1d9", size = 3) +
      labs(title = "Total Spending by Age", x = "Age", y = "Total Spending") +
      theme_minimal()
    ggplotly()
    
    
  } 
  
)

output$total_city <- renderPlotly(
  
  {
    req(data())
    req(data_clean())
    totals_by_city <- aggregate(total ~ city, data = data_clean(), FUN = sum)
    totals_by_city <- totals_by_city[order(totals_by_city$total, decreasing = TRUE), ]
    
    ggplot(data = totals_by_city, aes(x = reorder(city, -total), y = total)) +
      geom_bar(stat = "identity", fill = "#3d5a80") +
      labs(title = "Total Spending by City",
           x = "City",
           y = "Total Spending") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly()
    
  } 
  
)

output$total_dist <- renderPlotly(
  
  {
    req(data())
    req(data_clean())
    ggplot(data = data_clean(), aes(y = total)) +
      geom_boxplot(fill = "#ee6c4d", color = "black") +
      labs(title = "Distribution of Total Spending",
           y = "Total Spending") +
      theme_minimal()
    ggplotly()
    
  } 
  
)






    }

# Run the application
shinyApp(ui = ui, server = server)


