library(shiny)
library(plotly)

## makes sure that valid start and end dates are passed to the function that retrieves
## the stock data. These are global variables since they must be used in the server 
## function
firstDate <<- if(weekdays(Sys.Date() - 365) == "Saturday"){
    Sys.Date() - 366
}else if(weekdays(Sys.Date() - 365) == "Sunday"){
    Sys.Date() - 367
}else{
    Sys.Date() - 365
}

lastDate <<- if(weekdays(Sys.Date()) == "Saturday"){
    Sys.Date() - 1
}else if(weekdays(Sys.Date()) == "Sunday"){
    Sys.Date() - 2
}else if(weekdays(Sys.Date()) == "Monday"){
    Sys.Date() - 3
}else{
    Sys.Date()
}




shinyUI(fluidPage(
  
    titlePanel("Stock Prices"),
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "ticker", label = "Enter ticker symbol", value = "AAPL", 
                      placeholder = "e.g. AAPL, GOOG"),
            actionButton(inputId = "button", label = "Submit"),
            br(),
            dateRangeInput(inputId = "date", label = "Select Dates", start = firstDate,
                           end = lastDate - 1, min = firstDate, max = lastDate),
            numericInput(inputId = "MA", label = "Select an n-Day Moving Avergae", value = 10,
                         min = 1, max = 100, step = 1),
            checkboxInput(inputId = "show", label = "Show Moving Average", value = TRUE)
        ),
        
        mainPanel(
            plotlyOutput("graph"),
            verbatimTextOutput("test"),
            br(),
            h4("Summary of selected closing prices"),
            verbatimTextOutput("summary")
        )
    )
))


