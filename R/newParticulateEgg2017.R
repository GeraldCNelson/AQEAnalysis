Sys.setenv(TZ = "America/Denver") # needed to get rid of a warning message
library(openxlsx)
library(data.table)
library(ggplot2)
library(zip)
library(tidyr)
library(stringr)
library(plotly)
library(gridExtra)
source("R/eggdataRetrieval.R")

#' what time zone are the data from
tz <- "America/Denver"

#' This code is just to read in and display egg data.

#' what eggs to get info for
eggSerialList <- c("egg00802e63038b0122", "egg00802d56461b0133", "egg00802e31aba80123", "egg00802e6306880122", "egg00802e6305980122")

# egg00802e63038b0122
# egg00802d56461b0133
# egg00802e31aba80123
# egg00802e6306880122
# egg00802e6305980122
#' what period to get info for. Update. The choice of start and end date and now from the official data
#' Note: date and time information must use this format, including 24 hour clock
startDate <- "2017-12-19"
startTime <- "10:00:00" #include two digits in hours, min, and seconds.
endDate <- "2017-12-19"
endTime <- "11:10:00" #include two digits in hours, min, and seconds.

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
  sensorList <- c(sensorList, "temperature", "humidity")
  serialtailCt <- 6 #number of characters from end of serial number to use in eggName
  serialTail <- substr(eggSerial, nchar(eggSerial) - serialtailCt, nchar(eggSerial))
  eggName <- paste(eggType, serialTail, sep = "_")
  eggFileName <- paste(dataFolder, eggName, "_", dateInfo, ".rds", sep = "")
  if (file.exists(eggFileName)) {
    returnMessage <- "Success"
  }else{
    returnMessage <- dataDownload(startDate, startTime, endDate, endTime, eggSerial, eggFileName, tz)
  }
  if (returnMessage %in% "Success") {
    dt <- readRDS(file = eggFileName)
    sensorDT <-  as.data.table(read.csv(file = paste("data/sensorInfo_", eggName, ".csv", sep = "")))
    # remove columns that are all NA
    dt <- Filter(function(x)!all(is.na(x)), dt)
    namesToChange <- names(dt)[!names(dt) %in% "timeStamp"]
    setnames(dt, old = namesToChange, new = paste(namesToChange, eggName,sep = "_"))
    assign(eggName, dt)
    eggNameList <- c(eggNameList, eggName)
  }
}

#' combine all the egg data 
#' the commented out code is replaced by the mget statement below it.
# dtList <- list()
# for (k in 1:length(fulldtList)) {
#    dtList[[fulldtList[k]]] <- eval(parse(text = fulldtList[k]))
# }
if (length(eggNameList > 1)) { # if only one egg, nothing to merge
  dtList <- c(eggNameList)
  dtList <- mget(dtList)
  mergedDT <- Reduce(function(...) merge(..., all = TRUE, by = "timeStamp"), dtList)
}else{mergedDT <- eval(parse(text = eggNameList))}

# now average mergedDT to 15 minutes
x <- "15"
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
if (eggType %in% "no2o3") {
  mergedDT.long.rawValAux <- mergedDT.long[measurement %in% c("rawValAux")]
  wideFormula <- "timeStamp + eggType + eggSerial ~ gas"
  mergedDT.wide.rawValAux <- data.table::dcast(data = mergedDT.long.rawValAux,
                                               formula = wideFormula,
                                               value.var = "value")
  mergedDT.wide.rawValAux[, eggName := paste(eggType, eggSerial, sep = "_")]
}

#'  plots
temp.overall <- mergedDT.wide.rawVal[eggType %in% c("no2co","no2o3", "particulate", "particulateco2pressure"),]
for (j in eggNameList) {
  #' first work on temp, rh, and ah
  plotsList <- c("temperature", "humidity", "ah")
  plots1 <- list()
  for (i in 1:length(plotsList)) local({
    i <- i #needed to make i local
    #' create some labels
    DT <- temp.overall[eggName %in% j, c("timeStamp", plotsList[i]), with = FALSE]
    if (plotsList[i] %in% "temperature") {
      gas <- "temperature"
      gasYLabel <- "Temperature ("~degree~"C)"
      
      #   if (unique(userInfoDT[sensor %in% "tempUnits", sensorPartNum]) %in% "degC") gasYLabel <- "Temperature (~degree~C)"
      gasinTitle <- "Temperature (internal)"
    }
    if (plotsList[i] %in% "humidity") {
      gasYLabel <- "RH ("~percent~")"
      gasinTitle <- "Relative humidity (internal)"
    }
    if (plotsList[i] %in% "ah") {
      gasYLabel <- "Absolute humidity (g/"~m^3~")"
      gasinTitle <- "Absolute humidity (internal)"
    }
    
    p <- ggplot(data = DT, aes(x = timeStamp, y = eval(parse(text = (plotsList[i]))))) + 
      #      p <- ggplot(data = DT, aes(x = timeStamp, y = eval(parse(text = (plotsList[i]))), group = eggName, color = eggName)) + 
      #     p <- ggplot(data = DT, aes(x = timeStamp, y = eval(parse(text = (plotsList[i]))), group = eval(parse(text = j)), color = eval(parse(text = j)), parse = TRUE)) + 
      xlab("Time") +
      ylab(bquote(.(gasYLabel))) +
      scale_y_continuous() +
      theme_bw() +
      ggtitle(sprintf("%s \negg %s", gasinTitle, eggName)) +
      theme(plot.title = element_text(hjust = 0.5)) + # center title
      geom_line(aes(color = "red")) +
      theme(legend.position = "none")
    # print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
    #  print(p)
    plots1[[i]] <<- p
  })
  
  multiplot(plotlist = plots1, cols = 2)
  
  # particulates
  userInfoDT <- as.data.table(read.csv(file = paste("data/userInfo_", j, ".csv", sep = "")))
  temp <- mergedDT.wide.compVal[eggType %in% c("particulateco2pressure") & eggName == j, ]
  # plot for each particulate type
  cols.pm1 <- c("timeStamp", "eggName", "pm1p0", "pm1p0a", "pm1p0b")
  cols.pm25 <- c("timeStamp", "eggName", "pm2p5", "pm2p5a", "pm2p5b")
  cols.pm10 <- c("timeStamp", "eggName", "pm10p0", "pm10p0a", "pm10p0b")
  pmList <- c("cols.pm1", "cols.pm25", "cols.pm10")
  plots.pm <- list()
  for (i in 1:length(pmList)) local({
    i <- i
    DT <- temp[, eval(parse(text = (pmList[i]))), with = FALSE ]
    DT.long <- data.table::melt(
      DT,
      id.vars = c("timeStamp", "eggName"),
      variable.factor = FALSE  
    )
    yLabelList <- c("PM1", "PM2.5", "PM10")
    gasYLabel <- "(µg/"~m^3~")"
    gasinTitle <- yLabelList[i]
    p <- ggplot(data = DT.long, aes(x = timeStamp, y = value, group = variable, color = variable)) + 
      xlab("Time") +
      ylab(bquote(.(gasYLabel))) +
      scale_y_continuous() +
      theme_bw() +
      ggtitle(sprintf("%s\n egg %s", gasinTitle, eggName)) +
      theme(plot.title = element_text(hjust = 0.5)) + # center title
      geom_line()
    #   print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
    plots.pm[[i]] <<- p
  })
  multiplot(plotlist = plots.pm, cols = 2)
}

# o3
temp <- mergedDT.wide.compVal[eggType %in% c("no2o3")]
temp[eggName %in% "official_NA", eggName := official]
if (official.o3.available == TRUE) temp <- mergedDT.wide.compVal[eggType %in% c("no2o3", "official")]
p.o3 <- ggplot(data = temp, aes(x = timeStamp, y = o3, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "O<sub>3</sub> (ppb))") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of O<sub>3</sub> values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.o3, tooltip = c("timeStamp", "o3"), dynamicTicks = TRUE)

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
  if (eggType %in% "no2o3") {
    polyNo <- 3
    variableList <- c("o3_rawVal_volt", "no2_rawVal_volt", "no2_rawValAux_volt", "temperature_rawVal_degF", "ah_rawVal_g.m3")
    formulaRHS <- paste("poly(", variableList, "_", eggName, ",", polyNo, ") + ", sep = "", collapse = "")
    formulaRHS <- substr(formulaRHS, 1, nchar(formulaRHS) - 3)
    formula1 <- paste("o3_compVal_ppb_official ~ ",formulaRHS)
    print(formula1)
    fit <- lm(formula = formula1, data = mergedDT.noNA, 
              na.action = na.exclude) # na.action=na.exclude keeps the na rows around; probably not needed with na.omit code above
    print(summary(fit))
    mergedDT.noNA[, predicted := predict(fit) ][, residuals := residuals(fit)]  
    startTime <- as.character(mergedDT.noNA$timeStamp[1])
    endTime <- as.character(mergedDT.noNA$timeStamp[nrow(mergedDT.noNA)])
    p.o3pred.noNA <- ggplot(mergedDT.noNA, aes(x = timeStamp)) +
      #  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + #linear fit
      geom_line(aes(y = o3_compVal_ppb_official, color = "monitor")) +
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
    
  }
}


#plot(o3fit_19b0111$residuals,xlab = "time", ylab="regression residuals")
# 
# # see https://drsimonj.svbtle.com/visualising-residuals for residuals graphing discussion
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(o3fit_19b0111)

#add fit and residuals to mergedDT
mergedDT[, predicted := predict(fit) ][, residuals := residuals(fit)]  
ggplot(mergedDT, aes(x = timeStamp, y = o3_compVal_ppb_official)) +
  #  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + #linear fit
  geom_point(fill = "green", color = "green") +
  geom_point(aes(y = predicted), shape = 1) + 
  # > Color adjustments made here...
  geom_point(aes(color = abs(residuals))) + # Color mapped to abs(residuals)
  scale_color_continuous(low = "black", high = "red") +  # Colors to use here
  guides(color = FALSE) +  # Color legend removed
  geom_segment(aes(x = timeStamp, xend = timeStamp, y = o3_compVal_ppb_official, yend = predicted),  color = "lightgrey") + # connect measure value and calculated value
  theme_bw()

startTime <- as.character(mergedDT$timeStamp[1])
endTime <- as.character(mergedDT$timeStamp[nrow(mergedDT)])
p.o3pred <- ggplot(mergedDT, aes(x = timeStamp)) +
  #  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + #linear fit
  geom_line(aes(y = o3_compVal_ppb_official, color = "monitor")) +
  geom_line(aes(y = predicted, color = "predicted")) + 
  labs(x = "date and time", y = "O3 (ppb)", title = paste("Ozone, actual (green) and predicted (red),\n ", startTime, " to ", endTime, sep = "")) +
  # > Color adjustments made here...
  scale_color_manual(
    name = "Ozone source",
    values = c(monitor = "darkgreen", predicted = "red"),
    labels = c("monitor", "predicted")) +
  #  guides(color = FALSE) +  # Color legend removed
  #  geom_segment(aes(x = timeStamp, xend = timeStamp, y = o3_compVal_ppb_official, yend = predicted),  color = "lightgrey") + # connect measure value and calculated value
  theme_bw()
ggplotly(p.o3pred)

coeff.poly3 <- coef(o3fit_19b0111.poly)
coeff.poly3["Intercept"]


