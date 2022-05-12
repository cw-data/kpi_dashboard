# https://shiny.rstudio.com/
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
# tinytex::install_tinytex() # install tinytex() if you get an error: "No LaTeX installation detected"
# dir_script <- dirname(rstudioapi::getSourceEditorContext()$path) # extract the filepath in which this script and dataset are saved
# # setwd(dir_script) # set working directory so the script finds the dataset
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
  ) %>%
  pivot_longer( # pivot longer so we can subset by KPI in the time series figure
               cols = 2:4, 
               names_to = "type",
               values_to = "value"
  )
#format(min(d1$Order.Date), "%d %b %Y")


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Schrute Paper KPI Dashboard"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Performance Metric"),
                                choices = unique(d1$type),
                                selected = "Travel"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = min(d1$Order.Date), end = max(d1$Order.Date),
                                   min = min(d1$Order.Date), max = max(d1$Order.Date)),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.5, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotlyOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc")
                    #tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    d1 %>%
      filter(
        type == 'total_sales',
        Order.Date > as.POSIXct('2017-01-01') & Order.Date < as.POSIXct('2021-12-31'
        ))%>%
      mutate(
        fitted = fitted(loess(value ~ as.numeric(Order.Date))),
        upper_CI = value + (1.96 * predict(loess(value ~ as.numeric(Order.Date)), se=T)[["se.fit"]]),
        lower_CI = value - (1.96 * predict(loess(value ~ as.numeric(Order.Date)), se=T)[["se.fit"]])
      )
    # plx <- predict(loess(d1()$value ~ as.numeric(d1()$Order.Date)), se=T) # calculate SE for each prediction for plotting; https://stackoverflow.com/questions/22717930/how-to-get-the-confidence-intervals-for-lowess-fit-using-r
    # d1()$fitted <- fitted(loess(d1()$value ~ as.numeric(d1()$Order.Date))) # calculate the fitted (predicted) value
    # d1()$upper_CI <- d1()$value + (1.96 * plx[["se.fit"]]) # calculate the upper 95% CI
    # d1()$lower_CI <- d1()$value - (1.96 * plx[["se.fit"]]) # calculate the lower 95% CI
  })
  
  # 
  # plx <- predict(loess(selected_trends()$value ~ as.numeric(selected_trends()$Order.Date)), se=T) # calculate SE for each prediction for plotting; https://stackoverflow.com/questions/22717930/how-to-get-the-confidence-intervals-for-lowess-fit-using-r
  # selected_trends()$fitted <- fitted(loess(selected_trends()$value ~ as.numeric(selected_trends()$Order.Date))) # calculate the fitted (predicted) value
  # selected_trends()$upper_CI <- selected_trends()$value + (1.96 * plx[["se.fit"]]) # calculate the upper 95% CI
  # selected_trends()$lower_CI <- selected_trends()$value - (1.96 * plx[["se.fit"]]) # calculate the lower 95% CI
  # 
  
  # Create scatterplot object the plotOutput function is expecting
  # output$lineplot <- renderPlot({
  #   color = "#434343"
  #   par(mar = c(4, 4, 1, 1))
  #   plot(x = selected_trends()$Order.Date, y = selected_trends()$value, type = "l",
  #        xlab = "Date", ylab = "KPI", col = color, fg = color, col.lab = color, col.axis = color) # https://stackoverflow.com/questions/53127403/reactive-axis-labels-in-shiny-r
  #   # Display only if smoother is checked
  #   if(input$smoother){
  #     smooth_curve <- lowess(x = as.numeric(selected_trends()$Order.Date), y = selected_trends()$value, f = input$f)
  #     lines(smooth_curve, col = "#E6553A", lwd = 3)
  #   }
  # })
  
  output$lineplot <- renderPlotly({
    plot_ly(
            showlegend = FALSE
            ) %>%
      layout(
        hovermode = "x unified",
        title = "Monthly means with loess and 95% CI" # reactive for plot title?
        ) %>%
      add_trace(y = selected_trends()$value,
                type="scatter",
                mode = "markers+lines",
                hoverinfo = 'text',
                text = ~paste('</br> Date: ', format(selected_trends()$month, "%b %Y"),
                              '</br> Revenue: ', dollar_format()(selected_trends()$value)
                ),
                line=list(
                  width = 1
                )
      ) %>%
      add_trace(y = fitted(loess(selected_trends()$value ~ as.numeric(selected_trends()$month))) + (1.96 * plx[["se.fit"]]), 
                type = 'scatter',
                mode = 'lines',
                hoverinfo = 'text',
                text = ~paste('</br> Upper 95% CI: ', dollar_format()(selected_trends()$upper_CI)
                ),
                line=list(
                  color='#000000',
                  dash = 'dot',
                  width = 1
                )
      ) %>%
      add_trace(y = selected_trends()$fitted,
                type = 'scatter',
                mode = 'lines',
                hoverinfo = 'text',
                text = ~paste('</br> Predicted: ', dollar_format()(selected_trends()$fitted)
                ),
                line=list(
                  color='blue',
                  dash = 'dash',
                  width = 1
                )
      ) %>%
      add_trace(y = fitted(loess(selected_trends()$value ~ as.numeric(selected_trends()$month))) - (1.96 * plx[["se.fit"]]),
                type = 'scatter',
                mode = 'lines',
                hoverinfo = 'text',
                text = ~paste('</br> Lower 95% CI: ', dollar_format()(selected_trends()$lower_CI)
                ),
                line=list(
                  color='#000000',
                  dash = 'dot',
                  width = 1
                )
      )
    
    
    # plot_ly(
    #   x = selected_trends()$Order.Date,
    #   y = selected_trends()$value,
    #   type="scatter",
    #   mode = "markers+lines",
    #   hoverinfo = 'text',
    #   text = ~paste('</br> Date: ', format(selected_trends()$Order.Date, "%b %Y"),
    #                 '</br> Revenue: ', dollar_format()(selected_trends()$value)
    #   )
    # ) %>%
    #   layout(hovermode = "x unified",
    #          title = "Monthly means with loess curve" # reactive for plot title?
    #   ) %>%
    #   add_trace(y = fitted(loess(selected_trends()$value ~ as.numeric(selected_trends()$Order.Date)
    #   )
    #   )
    #   )
    # Display only if smoother is checked
    # if(input$smoother){
    #   smooth_curve <- lowess(x = as.numeric(selected_trends()$Order.Date), y = selected_trends()$value, f = input$f)
    #   lines(smooth_curve, col = "#E6553A", lwd = 3)
  }
  )
}

# # Pull in description of trend
# output$desc <- renderText({
#   trend_text <- filter(trend_description, type == input$type) %>% pull(text)
#   paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
# })

# Create Shiny object
shinyApp(ui = ui, server = server)