library(shiny)
library(shinythemes)
library(rsconnect)
library(data.table)
library(xtable)
#library(fmsb) # for the spider charts
#library(gdata) # to reorder the scenario names
#library(dplyr)
# Define UI for application that compares data from two eggs and estimates slope and intercept values
# Define UI -----
rsconnect::setAccountInfo(name='nutrientmodeling', token='D0257883A5409984C3DC39101FAACA2E', secret='xxBEXU7/AvSa7/10LHFCU7uzXVCHiHP+T7Q8fJDB')
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # Application title
  titlePanel("Generate calibration parameters"),
  # Sidebar with two csv imports 
  sidebarLayout(
    sidebarPanel(
      fileInput('master', 'Choose the master AQE CSV File',
                accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
      fileInput('slave', 'Choose the slave AQE CSV File',
                accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv'))
      #     conditionalPanel("output.fileUploaded",
      #      submitButton("Generate results")
    ),
    # Show a plot of the generated distribution
    mainPanel(width = "100%",
              fluidRow(align = "center",
                       column(
                         width = 6, div(tableOutput("coeffTableO3"), style = "font-size:80%")
                       ),
                       column(
                         width = 6, div(tableOutput("coeffTableNO2"), style = "font-size:80%")
                       )
              ),
              fluidRow(align = "center",
                       column(
                         width = 6, plotOutput("timeSeriesPlotO3")
                       ),column(
                         width = 6, plotOutput("timeSeriesPlotNO2")
                       )
              )
    )
  )
)

server <- function(input, output) {
  dataInput <- reactive({
    masterInfo <- input$master
    if (is.null(masterInfo))
      return(NULL)
    dt.master <- read.csv(masterInfo$datapath,
                          stringsAsFactors = FALSE,
                          na.strings = "---",
                          check.names = FALSE,
                          colClasses = c("character", rep("numeric",10)))
    dt.master <- as.data.table(dt.master)
    
    slaveInfo <- input$slave
    if (is.null(slaveInfo))
      return(NULL)
    
    dt.slave <- as.data.table(read.csv(slaveInfo$datapath,
                                       stringsAsFactors = FALSE,
                                       na.strings = "---",
                                       check.names = FALSE,
                                       colClasses = c("character", rep("numeric",10)))
    )
    
    timezone <- "America/Denver"
    newNames.master <- c("time", "temp", "humidity", "no2", "o3", "no2_we_v", "no2_aux_v",
                         "o3_v",  "lat", "long", "altitude")
    setnames(dt.master, old = names(dt.master), new = newNames.master)
    
    newNames.slave <- c("time", "temp.slave", "humidity.slave", "no2.slave", "o3.slave", "no2_we_v.slave", 
                        "no2_aux_v.slave", "o3_v.slave",  "lat.slave", "long.slave", "altitude.slave")
    setnames(dt.slave, old = names(dt.slave), new = newNames.slave)
    
    keepListCol <- c("time", "no2", "o3")
    dt.master <- dt.master[, keepListCol, with = FALSE]
    dt.master <- dt.master[complete.cases(dt.master),]
    # print("Master results")
    # print(summary(dt.master)) 
    # print(str(dt.master)) 
    
    keepListCol <- c("time", "no2.slave", "o3.slave")
    dt.slave <- dt.slave[, keepListCol, with = FALSE]
    dt.slave <- dt.slave[complete.cases(dt.slave),]
    # print("Slave results")
    # print(summary(dt.slave)) 
    # print(str(dt.slave)) 
    
    #aggregate to minutes; note that cut returns a factor 'time' becomes 'cut'
    dt.master[, time := as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S", tz = timezone)]
    dt.slave[, time :=  as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S", tz = timezone)]
    varsToAgg.master <- c("no2", "o3")
   # print(str(dt.master))
    dt.master.min <- dt.master[, lapply(.SD, mean), by = list(cut(dt.master$time, breaks = "min")), 
                               .SDcols = varsToAgg.master]
    
    varsToAgg.slave <- c("no2.slave", "o3.slave")
    # print(str(dt.slave))
    dt.slave.min  <- dt.slave[, lapply(.SD, mean),  by = list(cut(dt.slave$time, breaks = "min")), 
                              .SDcols = varsToAgg.slave]
    
    #aggregate to minutes; note that cut returns a factor 'time' becomes 'cut'
    varsToAgg.master <- c("no2", "o3")
    dt.master.min <- dt.master[, lapply(.SD, mean), by = list(cut(dt.master$time, breaks = "min")), .SDcols = varsToAgg.master]
    varsToAgg.slave <- c("no2.slave", "o3.slave")
    dt.slave.min <- dt.slave[, lapply(.SD, mean), by = list(cut(dt.slave$time, breaks = "min")), .SDcols = varsToAgg.slave]
    
    # combine the two
    combinedResults <- merge(dt.master.min, dt.slave.min, by = 'cut')
    # convert cut back to posix and change name to time
    combinedResults[, time := as.POSIXct(cut,format = "%Y-%m-%d %H:%M:%S", tz = timezone)]
    combinedResults[,cut := NULL]
    
    return(combinedResults)
     # plot(combinedResults$time, combinedResults$o3.slave, type="l", main = "O3 time series")
    # points(combinedResults$o3)
    # lines(combinedResults$o3)
  })
  
  # 
 
  output$timeSeriesPlotO3 <- renderPlot({
    combinedResults <- dataInput()
    if (is.null(combinedResults)) return(NULL)
    # generate time series plot
    plot(combinedResults$time, combinedResults$o3.slave, type = "l", main = "O3 time series")
    points(combinedResults$time, combinedResults$o3)
    lines(combinedResults$time, combinedResults$o3)
  })
  output$timeSeriesPlotNO2 <- renderPlot({
    combinedResults <- dataInput()
    if (is.null(combinedResults)) return(NULL)
    # generate time series plot
    plot(combinedResults$time, combinedResults$no2.slave, type = "l", main = "NO2 time series")
    points(combinedResults$time, combinedResults$no2)
    lines(combinedResults$time, combinedResults$no2)
  })
  
  output$coeffTableO3 <- renderTable({
    # generate regression results
    combinedResults <- dataInput()
    if (is.null(combinedResults)) return(NULL)
    combinedResults <- combinedResults[complete.cases(combinedResults),]
    lmResultsO3 <- lm(o3.slave ~ o3, combinedResults)
    coef(summary(lmResultsO3))
    # coef(summary(lmResultsO3))["o3","Estimate"]
    # coef(summary(lmResultsO3))["(Intercept)","Estimate"]
  })
  
    output$coeffTableNO2 <- renderTable({
      # generate regression results
      combinedResults <- dataInput()
      if (is.null(combinedResults)) return(NULL)
      combinedResults <- combinedResults[complete.cases(combinedResults),]
      lmResultsNO2 <- lm(no2.slave ~ no2, combinedResults)
       coef(summary(lmResultsNO2))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


