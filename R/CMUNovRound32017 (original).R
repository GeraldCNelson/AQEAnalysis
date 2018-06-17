#library(openxlsx)
library(data.table)
library(ggplot2)
#library(zip)
#library(tidyr)
#library(stringr)
#library(plotly)
#library(grid)
#library(gridExtra)
library(lattice)
library(caret)
source("R/eggdataRetrieval.R")

#' what time zone are the data from
tz <- "America/Denver"

#' flag to indicate whether results from an official O3 monitor are available. If true, download the data and process a bit.
official.o3.available <- TRUE
if (official.o3.available == TRUE) {
  # data from the official monitor
  officialFileName <- "20nov2017_49c.csv"
  o3_dataDir <- "CMUNov2017Round3" # directory beneath data-raw where the o3 data file is located
  o3_officialFileLoc <- paste("data-raw", o3_dataDir, officialFileName, sep = "/")
  o3_official <- as.data.table(read.csv(o3_officialFileLoc, na.strings = "---", 
                                        stringsAsFactors = FALSE, header = TRUE))
  o3_official[, timeStamp := as.POSIXct(timeStamp, format = "%Y-%m-%d-%H:%M:%S", tz = "America/Denver")]
  o3_official[, timeStamp := as.POSIXct(round(timeStamp, "mins"))]
  setnames(o3_official, old = "o3_compVal_ppb_official", new = "o3_compVal_ppb_o3_official") #needed to have the correct number of columns later on.
  setkey(o3_official, timeStamp)
  startDate <- as.Date(o3_official$timeStamp[1], tz = tz)
  startTime <- strftime(o3_official$timeStamp[1], format = "%H:%M:%S")
  endDate <- as.Date(o3_official$timeStamp[nrow(o3_official)], tz = tz)
  endTime <-   strftime(o3_official$timeStamp[nrow(o3_official)], format = "%H:%M:%S")
}
#' what eggs to get info for
eggSerialList <- c("egg00802aaa019b0111", "egg0080270b448b0153", "egg008028730d880112", "egg008020c0d89b0153", "egg0080228ba6080140")

#' what period to get info for. Update. The choice of start and end date and now from the official data
#' Note: date and time information must use this format, including 24 hour clock
# startDate <- "2017-11-20"
# startTime <- "14:20:00" #include two digits in hours, min, and seconds.
# endDate <- "2017-11-22"
# endTime <- "15:50:00" #include two digits in hours, min, and seconds.

#' folder to write data to
dataFolder <- "data/"

#' dateInfo is part of the file name so needed to be here rather than in eggdataRetrieval.R
dateInfo <- paste(gsub("-", "", startDate), "T",gsub(":", ".", startTime), "_", gsub("-", "", endDate), "T",gsub(":", ".", endTime) , sep = "")
eggNameList <- as.character()
for (eggSerial in eggSerialList) {
  #eggSerial <- "egg00802aaa019b0111"
  serialtailCt <- 6 #number of characters from end of serial number to use in eggName
  serialTail <- substr(eggSerial, nchar(eggSerial) - serialtailCt, nchar(eggSerial))
  sensorList <- sensors(eggSerial)
  eggType <- paste(sensorList, collapse = "")
  print(eggType)
  sensorList <- c(sensorList, "temperature", "humidity")
  serialtailCt <- 6 #number of characters from end of serial number to use in eggName
  serialTail <- substr(eggSerial, nchar(eggSerial) - serialtailCt, nchar(eggSerial))
  eggName <- paste(eggType, serialTail, sep = "_")
  eggFileName <- paste(dataFolder, eggName, "_", dateInfo, ".rds", sep = "")
  if (!file.exists(eggFileName)) {
    dataDownload(startDate, startTime, endDate, endTime, eggSerial, eggFileName, tz)
  }
  dt <- readRDS(file = eggFileName)
  # remove columns that are all NA
  dt <- Filter(function(x)!all(is.na(x)), dt)
  
  namesToChange <- names(dt)[!names(dt) %in% c("timeStamp")]
  setnames(dt, old = namesToChange, new = paste(namesToChange, eggName,sep = "_"))
  assign(eggName, dt)
  eggNameList <- c(eggNameList, eggName)
  
  # # read in the egg data and create  eggList. The suffix number is the number that James put on the bottom of each egg. 
  # eggPM.1 <- readRDS(file = "results/CMUNovRound22017_particulate_d880112.RDS")
  # eggList <- deparse(substitute(eggPM.1))
  # eggNO2CO.2 <- readRDS(file = "results/CMUNovRound22017_no2co_89b0153.RDS")
  # eggList <- c(eggList, deparse(substitute(eggNO2CO.2)))
  # eggNO2CO.5 <- readRDS(file = "results/CMUNovRound22017_no2co_6080140.RDS")
  # eggList <- c(eggList, deparse(substitute(eggNO2CO.5)))
  # eggNO2O3.4 <- readRDS(file = "results/CMUNovRound22017_no2o3_48b0153.RDS")
  # eggList <- c(eggList, deparse(substitute(eggNO2O3.4)))
  # eggNO2O3.3 <- readRDS(file = "results/CMUNovRound22017_no2o3_19b0111.RDS")
  # eggList <- c(eggList, deparse(substitute(eggNO2O3.3)))
  # 
}

#' combine all the egg data and the official monitor data into one data table
#' the commented out code is replaced by the mget statement below it.
# dtList <- list()
# for (k in 1:length(fulldtList)) {
#    dtList[[fulldtList[k]]] <- eval(parse(text = fulldtList[k]))
# }
dtList <- c(eggNameList, "o3_official")
dtList <- mget(dtList)
mergedDT <- Reduce(function(...) merge(..., all = TRUE, by = "timeStamp"), dtList)

# now average mergedDT to 15 minutes
x <- "10"
timeBreak <- paste0(x, " min")
varsToAve <-  names(mergedDT)[!names(mergedDT) %in% "timeStamp"]
mergedDT <- mergedDT[, lapply(.SD, mean, na.rm = TRUE ), 
                     by = list(cut.POSIXt(mergedDT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
setnames(mergedDT, old = "cut.POSIXt", new = "timeStamp")
mergedDT[, timeStamp := as.POSIXct(timeStamp)]

# drop N rows of the egg data; this means N * 15 minutes of data
N = 4
mergedDT <- mergedDT[-(1:N),] 

savedFileName <- paste("results/mergedDT", eggName, x, "min", startDate, sep = "_")

write.csv(mergedDT, file = gzfile(paste(savedFileName,"zip", sep = ".")))
write.csv(mergedDT, file = paste(savedFileName,"csv", sep = "."))
zip(zipfile = paste(savedFileName,"zip", sep = "."), files = c("results/mergedDT.15min.csv"))

# now do some analysis

mergedDT.long <- data.table::melt(
  mergedDT,
  id.vars = c("timeStamp")
)

mergedDT.long[, c("gas", "measurement", "units", "eggType", "eggSerial") := tstrsplit(variable, "_", fixed = TRUE)]
mergedDT.long[, variable := NULL]
mergedDT.long[measurement %in% "conVal", measurement := "compVal"] # temp and humidity are stored as conVals. We want all to be called compVals.

mergedDT.long.compVal <- mergedDT.long[measurement %in% "compVal",]
wideFormula <- "timeStamp + eggType + eggSerial ~ gas"
mergedDT.wide.compVal <- data.table::dcast(data = mergedDT.long.compVal,
                                           formula = wideFormula,
                                           value.var = "value")
mergedDT.wide.compVal[, eggName := paste(eggType, eggSerial, sep = "_")]

#do voltages and other raw values
mergedDT.long.rawVal <- mergedDT.long[measurement %in% c("rawVal")]
wideFormula <- "timeStamp + eggType + eggSerial ~ gas"
mergedDT.wide.rawVal <- data.table::dcast(data = mergedDT.long.rawVal,
                                          formula = wideFormula,
                                          value.var = "value")
mergedDT.wide.rawVal[, eggName := paste(eggType, eggSerial, sep = "_")]

# do NO2 second voltage. This one is called auxiliary electrode voltage; the first one is called working electrode voltage
mergedDT.long.rawValAux <- mergedDT.long[measurement %in% c("rawValAux")]
wideFormula <- "timeStamp + eggType + eggSerial ~ gas"
mergedDT.wide.rawValAux <- data.table::dcast(data = mergedDT.long.rawValAux,
                                             formula = wideFormula,
                                             value.var = "value")
mergedDT.wide.rawValAux[, eggName := paste(eggType, eggSerial, sep = "_")]
#'  plots
#'  First, do temperature, relative and absolute humidity from all eggs
temp.rawVal <- mergedDT.wide.rawVal[eggType %in% c("no2co", "no2o3", "particulate", "particulateco2pressure")]
varsToPlot <- c("temperature", "humidity", "ah")
plots <- list()
for (i in 1:length(varsToPlot)) local({
  i <- i
  if (varsToPlot[i] %in% "temperature") {
    gas <- "temperature"
    gasYLabel <- "Temperature ("~degree~"C)"
    gasinTitle <- "Temperature (internal)"
  }
  if (varsToPlot[i] %in% "humidity") {
    gasYLabel <- "RH (percent)"
    gasinTitle <- "Relative humidity (internal)"
  }
  if (varsToPlot[i] %in% "ah") {
    gasYLabel <- "Absolute humidity (g/"~m^3~")"
    gasinTitle <- "Absolute humidity (internal)"
  }
  
  p <- ggplot(data = temp.rawVal, aes(x = timeStamp, y = eval(parse(text = varsToPlot[i])), group = eggName, color = eggName)) + 
    labs(x = "Time", y = gasYLabel) +
    scale_y_continuous() +
    theme_bw() +
    ggtitle(gasinTitle) +
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    geom_line() +
    theme(legend.position = "bottom") + guides(colour = guide_legend(title.position = "top", title.hjust = 0.5))
    plots[[i]] <<- p
})
#' plot temp, rh and ah results
multiplot(plotlist = plots, cols = 2)

#' now do eggtype specific graphs
#' work on no2o3 eggs
temp.compval.no2o3 <- mergedDT.wide.compVal[eggType %in% c("no2o3") | eggSerial %in% "official"]
gasses <- c("no2", "o3")
plots.no2o3 <- list()
plots.no2o3.workingV <- list()
plots.no2o3.auxV <- list()
for (i in 1:length(gasses)) local({
  i <- i
  #' create  labels
  if (gasses[i] %in% "no2") {
    gas <- "no2"
    gasYLabel <- "NO"[2]~"(ppb)"
    gasinTitle <- "NO"[2]~"concentration"
  }
  if (gasses[i] %in% "o3") {
    gas <- "o3"
    gasYLabel <- "O"[3]~"(ppb)"
    gasinTitle <- O[3]~"concentration"
  }
  
  p <- ggplot(data = temp.compval.no2o3, aes(x = timeStamp, y = eval(parse(text = (gasses[i]))), group = eggName, color = eggName)) + 
    xlab("Time") +
    ylab(bquote(.(gasYLabel))) +
    scale_y_continuous() +
    theme_bw() +
    ggtitle(bquote(.(gasinTitle))) +
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    theme(legend.position = "bottom") + guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
    geom_line()
  #  print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
  plots.no2o3[[i]] <<- p
  
  # o3_v and no2_v
  temp <- mergedDT.wide.rawVal[eggType %in% c("no2o3")]
  if (gasses[i] %in% "no2") {
    gas <- "no2"
    gasYLabel <- "volts"
    gasinTitle <- "NO"[2]~" working electrode voltage"
  }
  if (gasses[i] %in% "o3") {
    gasYLabel <- "volts"
    gasinTitle <- "O"[3]~" working electrode voltage"
  }
  
  p.workingV <- ggplot(data = temp, aes(x = timeStamp, y = eval(parse(text = (gasses[i]))), group = eggName, color = eggName)) + 
    labs(x = "Time", y = gasYLabel) +
    scale_y_continuous() +
    theme_bw() +
    ggtitle(bquote(.(gasinTitle))) +
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    theme(legend.position = "bottom") + guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
  #    guides(size = guide_legend(title.position = "top", title.hjust = 0.5)) +
    geom_line()
  #print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
  plots.no2o3.workingV[[i]] <<- p.workingV
  
  # no2_v2 - auxiliary electrode
  if (gasses[i] %in% "no2") {
    temp <- mergedDT.wide.rawValAux[eggType %in% c("no2o3")]
  gas <- "no2"
  gasYLabel <- "volts"
  gasinTitle <- "NO"[2]~" auxiliary electrode voltage"
  
  p.auxV <- ggplot(data = temp, aes(x = timeStamp, y = eval(parse(text = (gasses[i]))), group = eggName, color = eggName)) + 
    labs(x = "Time", y = gasYLabel) +
    scale_y_continuous() +
    theme_bw() +
    ggtitle(bquote(.(gasinTitle))) +
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    theme(legend.position = "bottom") + guides(colour = guide_legend(title.position = "top", title.hjust = 0.5)) +
    geom_line()
  # print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
  plots.no2o3.auxV[[i]] <<- p.auxV
  }
})
multiplot(plotlist = plots.no2o3, cols = 2)
multiplot(plotlist = plots.no2o3.workingV, cols = 2)
multiplot(plotlist = plots.no2o3.auxV, cols = 1)

# start of regression stuff

#' scatter plots 
#' #' no2 voltage values
#' mergedDT[, no2vdiff1 := no2_rawVal_volt_no2o3_19b0111  - no2_rawVal_volt_no2o3_48b0153]
#' mergedDT[, no2vdiff2 := no2_rawValAux_volt_no2o3_19b0111 - no2_rawValAux_volt_no2o3_48b0153]
#' 
#' # NO2 working electrode voltage scatter plot
#' p.no2_we_scatter <- ggplot(data = mergedDT,aes(x = no2_rawVal_volt_no2o3_19b0111, 
#'                                                y = no2_rawVal_volt_no2o3_48b0153
#' )) +
#'   geom_point(alpha = 0.2) + 
#'   theme_bw() +
#'   ggtitle("NO<sub>2</sub> working electrode voltage values scatter plot") +
#'   theme(plot.title = element_text(hjust = 0.5)) # center title
#' ggplotly(p.no2_we_scatter,  dynamicTicks = TRUE)

# # NO2 auxiliary electrod voltage scatter plot
# p.no2_v2_scatter <- ggplot(data = mergedDT,aes(x = no2_rawValAux_volt_no2o3_19b0111, 
#                                                y = no2_rawValAux_volt_no2o3_48b0153
# )) +
#   geom_point(alpha = 0.2) + 
#   theme_bw() +
#   ggtitle("NO<sub>2</sub> auxiliary electrode voltage values scatter plot") +
#   theme(plot.title = element_text(hjust = 0.5)) # center title
# ggplotly(p.no2_v2_scatter,  dynamicTicks = TRUE)

# # NO2 working electrode voltage difference plot
# p.no2_v_diff <- ggplot(data = mergedDT, aes(x = timeStamp, y = no2vdiff1)) + 
#   labs(x = "time", y = "voltage") +
#   scale_y_continuous() +
#   theme_bw() +
#   ggtitle("Difference of NO<sub>2</sub> working electrode voltage values") +
#   theme(plot.title = element_text(hjust = 0.5)) + # center title
#   geom_line()
# ggplotly(p.no2_v_diff, tooltip = c("timeStamp", "no2vdiff1"), dynamicTicks = TRUE)
# 
# # NO2 aux electrode voltage difference plot
# p.no2_v_diff <- ggplot(data = mergedDT, aes(x = timeStamp, y = no2vdiff2)) + 
#   labs(x = "time", y = "voltage") +
#   scale_y_continuous() +
#   theme_bw() +
#   ggtitle("Difference of NO<sub>2</sub> auxiliary electrode voltage values") +
#   theme(plot.title = element_text(hjust = 0.5)) + # center title
#   geom_line()
# ggplotly(p.no2_v_diff, tooltip = c("timeStamp", "no2vdiff2"), dynamicTicks = TRUE)

# # NO2 48b0153 concentration versus voltage 1 scatter plot
# p.no2comp_v1_scatter <- ggplot(data = mergedDT,aes(x = no2_compVal_ppb_no2o3_48b0153, 
#                                                    y = no2_rawVal_volt_no2o3_48b0153
# )) +
#   geom_point(alpha = 0.2) + 
#   theme_bw() +
#   ggtitle("NO<sub>2</sub> concentration versus working electrode voltage values scatter plot, egg 48b0153") +
#   theme(plot.title = element_text(hjust = 0.5)) # center title
# ggplotly(p.no2comp_v1_scatter,  dynamicTicks = TRUE)
# 
# # NO2 19b0111 concentration versus working electrode voltage scatter plot
# p.no2comp_v1_scatter <- ggplot(data = mergedDT,aes(x = no2_compVal_ppb_no2o3_19b0111, 
#                                                    y = no2_rawVal_volt_no2o3_19b0111
# )) +
#   geom_point(alpha = 0.2) + 
#   theme_bw() +
#   ggtitle("NO<sub>2</sub> concentration versus working electrode voltage values scatter plot, egg 19b0111") +
#   theme(plot.title = element_text(hjust = 0.5)) # center title
# ggplotly(p.no2comp_v1_scatter,  dynamicTicks = TRUE)

# no2v.fit <- lm(data = mergedDT, no2_compVal_ppb_no2o3_19b0111 ~ no2_compVal_ppb_no2o3_48b0153)
# summary(no2v.fit)
# plot(no2v.fit$fitted.values)

# no2v.fit <- lm(data = mergedDT, no2_compVal_ppb_no2o3_19b0111 ~ no2_compVal_ppb_no2o3_48b0153 + humidity_conVal_percent_no2o3_48b0153 + o3_rawVal_volt_no2o3_48b0153)
# summary(no2v.fit)
# plot(no2v.fit$fitted.values)

#' #' o3 voltage values
#' mergedDT[,no2vdiff := no2_compVal_ppb_no2o3_19b0111, no2_compVal_ppb_no2o3_48b0153]
#' ggplot(data = mergedDT,aes(x = no2_compVal_ppb_no2o3_19b0111, 
#'                            y = no2_compVal_ppb_no2o3_48b0153
#' )) +
#'   geom_point(alpha = 0.2)
#' 
#' p.no2_v_diff <- ggplot(data = mergedDT, aes(x = timeStamp, y = no2vdiff)) + 
#'   labs(x = "Time", y = "voltage") +
#'   scale_y_continuous() +
#'   theme_bw() +
#'   ggtitle("Difference of NO<sub>2</sub> voltage values") +
#'   theme(plot.title = element_text(hjust = 0.5)) + # center title
#'   geom_line()
#' ggplotly(p.no2_v_diff, tooltip = c("timeStamp", "no2vdiff"), dynamicTicks = TRUE)

# fit polynomial regressions
mergedDT.noNA <- na.omit(mergedDT) # needed to do polynomial regressions
for (i in 1:length(eggSerialList)) {
  # browser()
  # work just on o3 for now
  eggSerial <- eggSerialList[i]
  sensorList <- sensors(eggSerial)
  eggType <- paste(sensorList, collapse = "")
  eggName <- eggNameList[i]
  
  # model fitting http://topepo.github.io/caret/model-training-and-tuning.html
  if (eggType %in% "no2o3") {
    polyNo <- 3
    variableList <- c("o3_rawVal_volt", "no2_rawVal_volt", "no2_rawValAux_volt", "temperature_rawVal_degC", "ah_rawVal_g.m3")
    formulaRHS <- paste("poly(", variableList, "_", eggName, ",", polyNo, ") + ", sep = "", collapse = "")
    formulaRHS <- substr(formulaRHS, 1, nchar(formulaRHS) - 3)
    formula1 <- paste("o3_compVal_ppb_o3_official ~ ",formulaRHS)
    print(formula1)
    # out of sample useful video - https://www.youtube.com/watch?v=uswmGsGmDZc
    fit <- lm(formula = formula1, data = mergedDT.noNA[1:round(nrow(mergedDT.noNA)/2),]) # na.action=na.exclude keeps the na rows around; probably not needed with na.omit code above
    predicted <- predict(fit, mergedDT.noNA[(round(nrow(mergedDT.noNA)/2) + 1):nrow(mergedDT.noNA),], type = "response")
    actual <- mergedDT.noNA[(round(nrow(mergedDT.noNA)/2) + 1:nrow(mergedDT.noNA)),o3_compVal_ppb_o3_official]
    sqrt(mean(predicted - actual)^2)
    print(summary(fit))
    mergedDT.noNA[, predicted := predict(fit) ][, residuals := residuals(fit)]  
    startTime <- as.character(mergedDT.noNA$timeStamp[1])
    endTime <- as.character(mergedDT.noNA$timeStamp[nrow(mergedDT.noNA)])
    p.o3pred.noNA <- ggplot(mergedDT.noNA, aes(x = timeStamp)) +
      #  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + #linear fit
      geom_line(aes(y = o3_compVal_ppb_o3_official, color = "monitor")) +
      geom_line(aes(y = predicted, color = "predicted")) + 
      geom_line(aes(y = residuals, color = "resids")) + 
      labs(x = "date and time", y = "O3 (ppb)", title = paste("Ozone, actual (green) and predicted (red), egg ", eggName, "\n", paste("<sub>", startTime, " to ", endTime, "</sub>", sep = ""),  sep = "")) + 
      #      ggtitle(label = paste("Ozone, actual (green) and predicted (red), egg ", eggName, sep = ""), subtitle = paste(startTime, " to ", endTime, sep = "")) +
      # > Color adjustments made here...
      scale_color_manual(
        name = "Ozone source",
        values = c(monitor = "darkgreen", predicted = "red", resids = "gray"),
        labels = c("monitor", "predicted", "residuals")) +
      theme(legend.position = 'top') +
      #  guides(color = FALSE) +  # Color legend removed
      #  geom_segment(aes(x = timeStamp, xend = timeStamp, y = o3_compVal_ppb_official, yend = predicted),  color = "lightgrey") + # connect measure value and calculated value
      theme_bw()
    #    p.o3pred.noNA
    print(ggplotly(p.o3pred.noNA, tooltip = c("source", "value")))
    
    # use the first half the data for estimation and the other half for prediction to see how good the coeffients are
    mergedDT.noNA.inSample <- mergedDT.noNA[1:round(nrow(mergedDT.noNA)/2),]
    mergedDT.noNA.outSample <- mergedDT.noNA[(round(nrow(mergedDT.noNA)/2) + 1):nrow(mergedDT.noNA),]
    fit.inSample <- lm(formula = formula1, data = mergedDT.noNA.inSample, 
                       na.action = na.exclude)
    print(summary(fit.inSample))
    test.in <- as.data.table(predict.lm(fit.inSample, mergedDT.noNA.inSample, se.fit = TRUE, type = "response"))
    test.out <- as.data.table(predict.lm(fit.inSample, mergedDT.noNA.outSample, se.fit = TRUE, type = "response"))
  }
}

# use caret to do regression training

createDataPartition(mergedDT$o3_compVal_ppb_o3_official, times = 2, p = 50)
