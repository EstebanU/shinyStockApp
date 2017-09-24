library(shiny)
suppressMessages(library(quantmod))


## These are dates for which the stock market was closed due to holidays
invalidDates <- as.Date(c("2016-01-01", "2016-01-18", "2016-02-15", "2016-03-25", "2016-05-30",
                  "2016-07-04", "2016-09-05", "2016-11-24", "2016-12-26", "2017-01-02",
                  "2017-01-16", "2017-02-20", "2017-04-14", "2017-05-29", "2017-07-04", 
                  "2017-09-04", "2017-11-23", "2017-12-25"))



shinyServer(function(input, output, session){

    ## downloads historical stock data for a one year period
    df <- eventReactive(input$button, {
        
        getSymbols(input$ticker, src = "yahoo", from = "2016-01-01")
        stock <- get(input$ticker)
        stock <- as.data.frame(stock)
        names(stock) <- sub("^.*\\.", "", names(stock))
        stock <- cbind(rownames(stock), stock)
        n <- dim(stock)[1]
        rownames(stock) <- 1:n
        names(stock)[1] <- "Date"
        
        ## Only the dates, closing prices, and volume are kept
        stock <- stock[, c(1, 5, 6)]
        stock[, 1] <- as.Date(stock[, 1])
        names(stock) <- tolower(names(stock))
        stock
    })

    
    ## Computes the moving averages with a default value of 10
    n <- reactive({dim(df())[1]})
    movingAvg <- function(x = 10){
        ma <- c(rep(0, n() - x + 1))
        for(i in x:n()){
            ma[i] <- sum(df()[(i - x + 1):i, "close"])/x
        }
        ma
    }
    
    
    
    startDate <- reactive({input$date[1]})
    endDate <- reactive({input$date[2]})
    
    ## 4 reactive values are created to store the last valid start and end dates
    rv <- reactiveValues(lastStart = firstDate, currentStart = firstDate, lastEnd = lastDate,
                         currentEnd = lastDate, countStart = 1, countEnd = 1)
    
    
    
    
    
    
    ## checks whether the start date is valid
    isValidStart <- function(){
        if((weekdays(startDate()) != "Sunday") & (weekdays(startDate()) != "Saturday") &
           !(startDate() %in% invalidDates) & (startDate() < endDate()))
            TRUE
        else
            FALSE
    }
    ## checks whether the end date is valid
    isValidEnd <- function(){
        if((weekdays(endDate()) != "Sunday") & (weekdays(endDate()) != "Saturday") &
           !(endDate() %in% invalidDates) & (endDate() > startDate()))
            TRUE
        else
            FALSE
    }

  
    
    
    
    
    
    ## The following observeEvent functions are used for keeping track of the previous dates
    ## so they can be used later. For example, if the user enters an invalid date, the graph
    ## does not dissapear or displays an error message. Instead, it shows the graph corresponding
    ## to the previous valid dates
    observeEvent(startDate(), {
        if(isValidStart()){
            if(rv$countStart == 1){
                rv$lastStart <- startDate()
                rv$currentStart <- startDate()
            }else{
                rv$lastStart <- rv$currentStart
                rv$currentStart <- startDate()
            }
        }    
            
        rv$countStart <- rv$countStart + 1
    })
    
    observeEvent(endDate(), {
        if(isValidEnd()){
            if(rv$countEnd == 1){
                rv$lastEnd <- endDate()
                rv$currentEnd <- endDate()
            }else{
                rv$lastEnd <- rv$currentEnd
                rv$currentEnd <- endDate() 
            }
        }
        rv$countEnd <- rv$countEnd + 1
    })
    
    
    
    
    ## If the user enters valid dates (those for which stock data is available), the 
    ## stock prices between those dates is shown. If the user enters an invalid date
    ## (weekend, holiday, etc.) a message is shown and the previous graph remains 
    ## in tact
    p1 <- eventReactive(c(input$button, input$date, input$MA, input$show), {
        X <- df()
        startDay <- rv$currentStart
        endDay <- rv$currentEnd
        
        
        ## there is a slight issue with the error messages, but it's minor and doesn't affect
        ## the functionality. It will have to be adjusted.
        if(!isValidEnd() | !isValidStart()){
            if(!isValidEnd() & (endDate() < rv$currentStart))
                showModal(dataModal3())
            else if(!isValidStart() & (startDate() > rv$currentEnd))
                showModal(dataModal2())
            else
                showModal(dataModal1())
        }
        
        X$movingAvg <- movingAvg(input$MA)
        startInd <- which(X[, "date"] == startDay)
        endInd <- which(X[, "date"] == endDay)
        modifiedAAPL <- X[startInd:endInd, ]
        p <- plot_ly(modifiedAAPL, x = ~date, y = ~close, type = "scatter",
                     mode = "lines+markers", name = "Closing Price", source = "source") %>% 
            layout(title = input$ticker, yaxis = list(title = "USD ($)"), 
                   xaxis = list(title = "Date"), showlegend = TRUE) %>%
            layout(dragmode = "select")
        
        if(input$show){
            p <- p %>% add_lines(y = ~movingAvg, name = paste(input$MA, "-day MA", sep = "")) %>%
                layout(dragmode = "select")
        }
        
        
        
        ## suppresses the warnings due to incompatibility between plotly and shiny
        p$elementId <- NULL
        p
     })

    
    
    
    

    
    output$graph <- renderPlotly({p1()})
    
    output$summary <- renderPrint({
        d <- event_data("plotly_selected", source = "source")
        if(is.null(d)) 
            "Click and drag"
        else
            summary(d[, c(4)])
    })
    
    ## These are pop-up messages displayed when the user enters an invalid day
    dataModal1 <- function(){
        modalDialog(span("The date you entered was invalid. Select another date."))
    }
    dataModal2 <- function(){
        modalDialog(span("Please select a start date before the end date."))
    }
    dataModal3 <- function(){
        modalDialog(span("Please select an end date after the start date."))
    }
    
})













# output$day1 <- renderPrint({paste("Current Date:", rv$currentStart)})
# output$day2 <- renderPrint({paste("Previous Date: ", rv$lastStart)})
# output$day3 <- renderPrint({paste("Current End Date:", rv$currentEnd)})
# output$day4 <- renderPrint({paste("Previous End Date: ", rv$lastEnd)})

# getSymbols("AAPL", src = "yahoo", from = "2016-01-01")
# aapl <- as.data.frame(AAPL)
# names(aapl) <- sub("^.*\\.", "", names(aapl))
# aapl <- cbind(rownames(aapl), aapl)
# n <- dim(aapl)[1]
# rownames(aapl) <- 1:n
# names(aapl)[1] <- "Date"
# 
# ## Computes the moving averages with a default value of 10
# n <- dim(aapl)[1]
# movingAvg <- function(x = 10){
#     ma <- c(rep(0, n - x + 1))
#     for(i in x:n){
#         ma[i] <- sum(aapl[(i - x + 1):i, "close"])/x
#     }
#     ma
# }

# ## Only the dates, closing prices, and volume are kept
# aapl <- aapl[, c(1, 5, 6)]
# aapl[, 1] <- as.Date(aapl[, 1])
# names(aapl) <- tolower(names(aapl))
# output$test <- renderText({df()[1, 1]})

