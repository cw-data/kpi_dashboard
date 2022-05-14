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

################# Prep Data #################

data <- read.csv("cleandata.csv", header = TRUE) # load data, from https://www.kaggle.com/datasets/vivek468/superstore-dataset-final?resource=download
# 
# #------------ data cleaning #------------
working_data <- data # work on a copy of the data
# # change date columns to class:date
cols.to.date <- grep('date', names(working_data), ignore.case = TRUE) # find index of column(s) containing the word 'date'
for (i in 1:length(cols.to.date)){
  working_data[,cols.to.date[i]] <- as.Date(working_data[,cols.to.date[i]], format = '%Y-%m-%d') # change columns containing the word 'date' to Date format
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
    total_sales = sum(Sales),
    orders = n(),
    avg_gpp = mean(Profit)/mean(Sales)
  )

d1 <- 
  pivot_longer(d1, # pivot longer so we can subset by KPI in the time series figure
               cols = 2:4, 
               names_to = "type",
               values_to = "value"
  )

type_list <- split(d1, d1$type)
# loop to calcualte the fitted, CIs, and SE for each dataframe (each selected_inputs() of the data)
for (i in 1:length(type_list)){
  type_list[[i]]["fitted"] <- fitted(loess(type_list[[i]][["value"]] ~ as.numeric(type_list[[i]][["Order.Date"]])))
  type_list[[i]]["upper_CI"] <- type_list[[i]][["fitted"]] + (1.96 * predict(loess(type_list[[i]][["value"]] ~ as.numeric(type_list[[i]][["Order.Date"]])), se=T)[["se.fit"]])
  type_list[[i]]["lower_CI"] <- type_list[[i]][["fitted"]] - (1.96 * predict(loess(type_list[[i]][["value"]] ~ as.numeric(type_list[[i]][["Order.Date"]])), se=T)[["se.fit"]])
  type_list[[i]]["se_fit"] <- predict(loess(type_list[[i]][["value"]] ~ as.numeric(type_list[[i]][["Order.Date"]])), se=T)[["se.fit"]]
}
d1 <- do.call("rbind", type_list) # rbind the dataframes from type_list to replace d1 for use in selected_inputs()

# format percentages for plotting
d1$value <- ifelse(d1$type == 'avg_gpp',
                   d1$value * 100,
                   d1$value)
d1$fitted <- ifelse(d1$type == 'avg_gpp',
                    d1$fitted * 100,
                    d1$fitted)
d1$upper_CI <- ifelse(d1$type == 'avg_gpp',
                      d1$upper_CI * 100,
                      d1$upper_CI)
d1$lower_CI <- ifelse(d1$type == 'avg_gpp',
                      d1$lower_CI * 100,
                      d1$lower_CI)
d1$se_fit <- ifelse(d1$type == 'avg_gpp',
                    d1$se_fit * 100,
                    d1$se_fit)


# create label columns for hovertext and figure titles in kpi time series (output$lineplot)
d1$label_date <- paste0('Date: ', format(d1$Order.Date, "%b %Y"))
d1$type_pretty <- ifelse(d1$type == 'avg_gpp',
                         'Avg order gross profit',
                         ifelse(d1$type == 'orders',
                                'Orders',
                                ifelse(d1$type == 'total_sales',
                                       'Revenue',
                                       "")
                         )
)
d1$label_value <- paste0(d1$type_pretty, ':', ifelse(d1$type == 'avg_gpp',
                                                paste0(' ', sprintf("%0.0f%%", (100 * d1$value)), ' of revenue'),
                                                ifelse(d1$type == 'total_sales',
                                                       paste0(' ', format_dollars(d1$value)),
                                                       paste0(round(d1$value, 0))
                                                )
)
)

d1$label_upper_95 <- paste0('Upper 95% CI: ', ifelse(d1$type == 'avg_gpp',
                                                     paste0(sprintf("%0.0f%%", (100 * d1$upper_CI)), ' of revenue'),
                                                     ifelse(d1$type == 'total_sales',
                                                            format_dollars(d1$upper_CI),
                                                            round(d1$upper_CI, 0)
                                                     )
)
)

d1$label_fitted <- paste0('Predicted: ', ifelse(d1$type == 'avg_gpp',
                                                paste0(sprintf("%0.0f%%", (100 * d1$fitted)), ' of revenue'),
                                                ifelse(d1$type == 'total_sales',
                                                       format_dollars(d1$fitted),
                                                       round(d1$fitted, 0)
                                                )
)
)

d1$label_lower_95 <- paste0('Lower 95% CI: ', ifelse(d1$type == 'avg_gpp',
                                                     paste0(sprintf("%0.0f%%", (100 * d1$lower_CI)), ' of revenue'),
                                                     ifelse(d1$type == 'total_sales',
                                                            format_dollars(d1$lower_CI),
                                                            round(d1$lower_CI, 0)
                                                     )
)
)
d1$title <- ifelse(d1$type == 'avg_gpp',
                   paste0('Average order gross profit (% of revenue), loess trendline and 95% CI'),
                   ifelse(d1$type == 'total_sales',
                          paste0('Total monthly revenue, loess trendline and 95% CI'),
                          ifelse(d1$type == 'orders',
                                 paste0('Monthly order volume, loess trendline and 95% CI'),
                                 "")
                   )
)
d1$label_yaxis <- ifelse(d1$type == 'avg_gpp',
                         paste0('Gross profit (%)'),
                         ifelse(d1$type == 'total_sales',
                                paste0('Monthly revenue'),
                                ifelse(d1$type == 'orders',
                                       paste0('Monthly orders'),
                                       "")
                         )
)
d1$label_xaxis <- "Date"

################# End Prep Data #################
################# Define UI #################
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Schrute Paper KPI Dashboard"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Performance Metric"),
                                choices = unique(d1$type_pretty),
                                selected = "Travel"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = min(d1$Order.Date), end = max(d1$Order.Date),
                                   min = min(d1$Order.Date), max = max(d1$Order.Date)),
                    
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotlyOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc")
                    #tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
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
             title = selected_trends()$title[1], # reactive for plot title
             yaxis = list(title = selected_trends()$label_yaxis[1]),
             xaxis = list(title = selected_trends()$label_xaxis[1])
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
}
################# End Define Server #################

################# Create Shiny object #################
shinyApp(ui = ui, server = server)
################# End Create Shiny object #################