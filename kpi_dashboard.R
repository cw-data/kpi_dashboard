# https://shiny.rstudio.com/

library(tidyverse)
library(rstudioapi)
library(janitor)
library(reshape2)
library(scales)
library(lubridate)
library(gt)
library(ggrepel)
library(patchwork)
library(tidyquant)
library(timetk)
library(priceR)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(DT)

################# Prep Data #################

data <- read.csv("cleandata.csv", header = TRUE) # load data, from https://www.kaggle.com/datasets/vivek468/superstore-dataset-final?resource=download
# 
# #------------ data cleaning #------------
working_data <- data # work on a copy of the data
# # change date columns to class:date
cols.to.date <- grep('date', names(working_data), ignore.case = TRUE) # find index of column(s) containing the word 'date'
for (i in 1:length(cols.to.date)){
  working_data[,cols.to.date[i]] <- as.Date(working_data[,cols.to.date[i]], format = '%m/%d/%Y') # change columns containing the word 'date' to Date format
}
# # change factor columns to class:factor
data.classes <- lapply(working_data, class) # find the class of all columns
# # if the column class == 'character' then that column should be changed to a factor
data.classes <- lapply(working_data, class) # extract class of all columns
data.classes <- melt(data.classes)
chr.to.fct <- subset(data.classes, value == 'character')$L1
working_data[chr.to.fct] <- lapply(working_data[chr.to.fct], as.factor) # https://stackoverflow.com/questions/35426903/change-class-of-multiple-columns-in-data-frame-without-for-loop
# glimpse(working_data)
# # In this dataset, integers should also be factors
int.to.fct <- subset(data.classes, L1 != 'Quantity') # leave Quantity as integer
int.to.fct <- subset(int.to.fct, value == 'integer')$L1 # change other integers to factors
working_data[int.to.fct] <- lapply(working_data[int.to.fct], as.factor) # https://stackoverflow.com/questions/35426903/change-class-of-multiple-columns-in-data-frame-without-for-loop

# summarize working_data by month
d1 <-
  working_data %>%
  timetk::summarise_by_time(
    .date_var = Order.Date,
    .by = "month",
    avg_sale = mean(Sales),
    avg_gp = mean(Profit),
    avg_gpp = mean(Profit)/mean(Sales),
    total_sales = sum(Sales),
    total_gp = sum(Profit),
    # total_gpp = mean(Profit)/mean(Sales),
    orders = n()
    
  )

d1 <- 
  pivot_longer(d1, # pivot longer so we can subset by KPI in the time series figure
               cols = 2:7, 
               names_to = "type",
               values_to = "value"
  )


type_list <- split(d1, d1$type)
# loop to calcualte the fitted and CIs for each dataframe
for (i in 1:length(type_list)){
  type_list[[i]]["fitted"] <- fitted(loess(type_list[[i]][["value"]] ~ as.numeric(type_list[[i]][["Order.Date"]])))
  type_list[[i]]["upper_CI"] <- type_list[[i]][["fitted"]] + (1.96 * predict(loess(type_list[[i]][["value"]] ~ as.numeric(type_list[[i]][["Order.Date"]])), se=T)[["se.fit"]])
  type_list[[i]]["lower_CI"] <- type_list[[i]][["fitted"]] - (1.96 * predict(loess(type_list[[i]][["value"]] ~ as.numeric(type_list[[i]][["Order.Date"]])), se=T)[["se.fit"]])
  type_list[[i]]["se_fit"] <- predict(loess(type_list[[i]][["value"]] ~ as.numeric(type_list[[i]][["Order.Date"]])), se=T)[["se.fit"]]
}
d1 <- do.call("rbind", type_list) # rbind the dataframes from type_list to replace d1 for use in selected_inputs()
d1$value <- ifelse(d1$type %in% c('avg_gpp'),
                   d1$value * 100,
                   d1$value)
d1$fitted <- ifelse(d1$type %in% c('avg_gpp'),
                    d1$fitted * 100,
                    d1$fitted)
d1$upper_CI <- ifelse(d1$type %in% c('avg_gpp'),
                      d1$upper_CI * 100,
                      d1$upper_CI)
d1$lower_CI <- ifelse(d1$type %in% c('avg_gpp'),
                      d1$lower_CI * 100,
                      d1$lower_CI)
d1$se_fit <- ifelse(d1$type %in% c('avg_gpp'),
                    d1$se_fit * 100,
                    d1$se_fit)

d1$label_date <- paste0('Date: ', format(d1$Order.Date, "%b %Y"))

# pretty aliases for each d1$type
d1$type_pretty <- ifelse(d1$type %in% c('avg_gpp'),
                         'Average order gross profit (%)',
                         ifelse(d1$type == 'orders',
                                'Orders ',
                                ifelse(d1$type == 'total_sales',
                                       'Total revenue',
                                       ifelse(d1$type == 'avg_sale',
                                              'Average order revenue',
                                              ifelse(d1$type == 'total_gp',
                                                     'Total gross profit',
                                                     ifelse(d1$type == 'avg_gp',
                                                            'Average order gross profit ($)',
                                                            '')
                                              )
                                       )
                                )
                         )
)


d1$label_value <- paste0(d1$type_pretty, ':', ifelse(d1$type %in% c('avg_gpp'),
                                                     paste0(' ', sprintf("%0.0f%%", d1$value), ' of revenue'),
                                                     ifelse(d1$type == 'orders',
                                                            round(d1$value, 0),
                                                            paste0(' ', format_dollars(d1$value))
                                                     )
)
)

d1$label_upper_95 <- paste0('Upper 95% CI: ', ifelse(d1$type %in% c('avg_gpp'),
                                                     paste0(' ', sprintf("%0.0f%%", d1$upper_CI), ' of revenue'),
                                                     ifelse(d1$type == 'orders',
                                                            round(d1$upper_CI, 0),
                                                            paste0(' ', format_dollars(d1$upper_CI))
                                                     )
)
)


d1$label_fitted <- paste0('Predicted: ', ifelse(d1$type %in% c('avg_gpp'),
                                                paste0(' ', sprintf("%0.0f%%", d1$fitted), ' of revenue'),
                                                ifelse(d1$type == 'orders',
                                                       round(d1$fitted, 0),
                                                       paste0(' ', format_dollars(d1$fitted))
                                                )
)
)

d1$label_lower_95 <- paste0('Lower 95% CI: ', ifelse(d1$type %in% c('avg_gpp'),
                                                     paste0(' ', sprintf("%0.0f%%", d1$lower_CI), ' of revenue'),
                                                     ifelse(d1$type == 'orders',
                                                            round(d1$lower_CI, 0),
                                                            paste0(' ', format_dollars(d1$lower_CI))
                                                     )
)
)

# custom plot tiltles for each d1$type
d1$title <- ifelse(d1$type == 'avg_gpp',
                   paste0('Average order gross profit (% of revenue), loess trendline and 95% CI'),
                   ifelse(d1$type == 'total_sales',
                          paste0('Total monthly revenue, loess trendline and 95% CI'),
                          ifelse(d1$type == 'orders',
                                 paste0('Monthly order volume, loess trendline and 95% CI'),
                                 ifelse(d1$type == 'avg_sale',
                                        'Average order revenue ($), loess trendline and 95% CI',
                                        ifelse(d1$type == 'total_gp',
                                               'Total monthly gross profit ($), loess trendline and 95% CI',
                                               ifelse(d1$type == 'avg_gp',
                                                      'Average order gross profit ($), loess trendline and 95% CI',
                                                      '')
                                        )
                                 )
                          )
                   )
)

# custom y-axis labels for each d1$type
d1$label_yaxis <- ifelse(d1$type == 'avg_gpp',
                         paste0('Gross profit (%)'),
                         ifelse(d1$type == 'total_sales',
                                paste0('Monthly revenue ($)'),
                                ifelse(d1$type == 'orders',
                                       paste0('Monthly orders'),
                                       ifelse(d1$type == 'avg_sale',
                                              'Order revenue ($)',
                                              ifelse(d1$type == 'total_gp',
                                                     'Gross profit ($)',
                                                     ifelse(d1$type == 'avg_gp',
                                                            'Average gross profit ($)',
                                                            '')
                                              )
                                       )
                                )
                         )
)

d1$label_xaxis <- "Date"

################# End Prep Data #################
################# Define UI #################

ui <- dashboardPage(
  dashboardHeader(title = "Schrute Paper KPI Dashboard",
                  titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("KPI time-series", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data download", tabName = "download_item", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("KPI time series"),
              fluidPage(
                mainPanel(
                  selectInput(inputId = "type", label = strong("Choose a key performance indicator"),
                              choices = unique(d1$type_pretty),
                              selected = "Travel"),
                  dateRangeInput("date", strong("Choose a date range"), start = min(d1$Order.Date), end = max(d1$Order.Date),
                                 min = min(d1$Order.Date), max = max(d1$Order.Date)),
                  plotlyOutput(outputId = "lineplot", height = "150%", width = "150%")
                  # tableOutput('table')
                )
              )
      ),
      

      
      # Second tab content
      tabItem(tabName = "download_item",
              h2("Download a .csv of the data"),
              fluidPage(
                mainPanel(
                  selectInput("dataset", "Choose a dataset:",
                              choices = c("Full dataset", "Regression summary")),
                  downloadButton("downloadData", "Download"),
                  DT::dataTableOutput("table", width = "150%")
                  # tableOutput('table')
                )
              )
      )
    )
  )
)

################# End Define UI #################
################# Define Server #################
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date) # don't calculate anything if there is no date
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    d1 %>%
      filter(
        type_pretty == input$type,
        Order.Date > as.POSIXct(input$date[1]) & Order.Date < as.POSIXct(input$date[2])
      )
  })
  
  output$lineplot <- renderPlotly({
    
    
    plot_ly(showlegend = FALSE) %>%
      layout(hovermode = "x unified",
             title = list(text = selected_trends()$title[1]), # reactive for plot title
             yaxis = list(title = selected_trends()$label_yaxis[1]),
             xaxis = list(title = selected_trends()$label_xaxis[1]),
             margin = list( # set plotly figure margins
               l = 50,
               r = 50,
               b = 100,
               t = 100,
               pad = 4
             )
      ) %>%
      add_trace(x = selected_trends()$Order.Date,
                y = selected_trends()$value,
                type="scatter",
                mode = "markers+lines",
                hoverinfo = 'text',
                text =  ~paste('</br>', selected_trends()$label_date,
                               '</br>', selected_trends()$label_value
                ),
                line=list(
                  width = 1
                )
      ) %>%
      add_trace(x = selected_trends()$Order.Date,
                y = selected_trends()$fitted + 1.96*selected_trends()$se_fit, 
                type = 'scatter',
                mode = 'lines',
                hoverinfo = 'text',
                text = ~paste('</br>', selected_trends()$label_upper_95),
                line=list(
                  color='#000000',
                  dash = 'dot',
                  width = 1
                )
      ) %>%
      add_trace(x = selected_trends()$Order.Date,
                y = selected_trends()$fitted,
                type = 'scatter',
                mode = 'lines',
                hoverinfo = 'text',
                text = ~paste('</br>', selected_trends()$label_fitted),
                line=list(
                  color='blue',
                  dash = 'dash',
                  width = 1
                )
      ) %>%
      add_trace(x = selected_trends()$Order.Date,
                y = selected_trends()$fitted - 1.96*selected_trends()$se_fit,
                type = 'scatter',
                mode = 'lines',
                hoverinfo = 'text',
                text = ~paste('</br>', selected_trends()$label_lower_95),
                line=list(
                  color='#000000',
                  dash = 'dot',
                  width = 1
                )
      )
    
    
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Full dataset" = working_data,
           "Regression summary" = d1
    )
  })
  
  output$table = DT::renderDataTable(
    datasetInput(),
    options = list(scrollX = TRUE)
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(tolower(gsub(" ", "_", input$dataset, fixed = TRUE)), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}
################# End Define Server #################

################# Create Shiny object #################
shinyApp(ui = ui, server = server)
################# End Create Shiny object #################