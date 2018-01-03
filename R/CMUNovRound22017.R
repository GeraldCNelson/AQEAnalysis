Sys.setenv(TZ = "America/Denver") # needed to get rid of a warning message
library(openxlsx)
library(data.table)
library(ggplot2)
library(zip)
library(tidyr)
library(stringr)
library(plotly)
source("R/eggdataRetrieval.R")

#' flag to indicate whether results from an official O3 monitor are available
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
  
  setkey(o3_official, timeStamp)
  startDate <- as.Date(o3_official$timeStamp[1], tz = tz)
  startTime <- strftime(o3_official$timeStamp[1], format = "%H:%M:%S")
  endDate <- as.Date(o3_official$timeStamp[nrow(o3_official)], tz = tz)
  endTime <-   strftime(o3_official$timeStamp[nrow(o3_official)], format = "%H:%M:%S")
}

#' what eggs to get info for
eggSerialList <- c("egg00802aaa019b0111", "egg0080270b448b0153", "egg008028730d880112", "egg008020c0d89b0153", "egg0080228ba6080140")

#' what time zone
tz <- "America/Denver"

#' #' what period to get info for. This has been replaced by information extracted from the official monitor data
#' #' Note: date and time information must use this format, including 24 hour clock
#' startDate <- "2017-11-15"
#' startTime <- "15:00:00" #include two digits in hours, min, and seconds.
#' endDate <- "2017-11-20"
#' endTime <- "14:20:00" #include two digits in hours, min, and seconds.

#' folder to write data to
dataFolder <- "data/"

o3_officialFileLoc <- paste("data-raw/CMUNov2017Round3", officialFileName, sep = "/")

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
  eggName <- paste(eggType, serialTail, sep = "_")
  eggFileName <- paste(dataFolder, eggName, "_", dateInfo, ".RDS", sep = "")
  if (!file.exists(eggFileName)) {
    dataDownload(startDate, startTime, endDate, endTime, eggSerial, eggFileName, tz)
  }
  dt <- readRDS(file = eggFileName)
  # remove columns that are all NA
  dt <- Filter(function(x)!all(is.na(x)), dt)
  # delete misc variables
  deleteList.inst <- names(dt)[str_detect(names(dt), "Inst")] #get rid of variables of instantaneous values
  deleteList.lon <- names(dt)[str_detect(names(dt), "_lon")] #get rid of variables of longitude
  deleteList.lat <- names(dt)[str_detect(names(dt), "_lat")] #get rid of variables of longitude
  deleteList.alt <- names(dt)[str_detect(names(dt), "_alt")] #get rid of variables of longitude
  if (length(deleteList.inst > 0)) dt[, (deleteList.inst) := NULL]
  if (length(deleteList.lon > 0)) dt[, (deleteList.lon) := NULL]
  if (length(deleteList.lat > 0)) dt[, (deleteList.lat) := NULL]
  if (length(deleteList.alt > 0))  dt[, (deleteList.alt) := NULL]
  namesToChange <- names(dt)[!names(dt) %in% "timeStamp"]
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

#' the commented out code is replaced by the mget statement below it.
# dtList <- list()
# for (k in 1:length(fulldtList)) {
#    dtList[[fulldtList[k]]] <- eval(parse(text = fulldtList[k]))
# }
dtList <- mget(eggNameList)
mergedDT <- Reduce(function(...) merge(..., all = TRUE, by = "timeStamp"), dtList)

#' comments from Vic
#' For temperature and humidity, converted-value may be of interest, if you are going to try and apply your own 
#' temperature compensation algorithm, as it is a better reflection of the temperature inside the Egg.
#' 
#' Bottom line is you should just ignore converted-value as it pertains to the gas sensors, it's purely academic / diagnostic.
#' GCN. converted value for all the gas and particulate sensors in curltest.R

if (official.o3.available == TRUE) {
  # data from the official monitor need to be handled differently.
  o3_officialFileLoc <- paste("data-raw/CMUNov2017Round2", officialFileName, sep = "/")
  o3_official <- as.data.table(read.csv(o3_officialFileLoc, na.strings = "---", 
                                        stringsAsFactors = FALSE, header = TRUE))
  o3_official[, timeStamp := as.POSIXct(timeStamp, format = "%Y-%m-%d-%H:%M:%S", tz = "America/Denver")]
  o3_official[, timeStamp := as.POSIXct(round(timeStamp, "mins"))]
  
  setkey(o3_official, timeStamp)
  mergedDT <- merge(mergedDT, o3_official, by = "timeStamp", all = TRUE)
}

# average to every x minutes
# first average everything to 1 minute to get all eggs on the same time
# commented out because this is done in the eggdataRetrieval script
# x <- "1"
# timeBreak <- paste0(x, " min")
# for (i in fulldtList) {
#   dtTemp <- eval(parse(text = i))
#   varsToAve <-  names(dtTemp)[!names(dtTemp) %in% "timeStamp"]
#   dtTemp <- dtTemp[, lapply(.SD, mean, na.rm = FALSE ), 
#                    by = list(cut.POSIXt(dtTemp$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
#   setnames(dtTemp, old = "cut.POSIXt", new = "timeStamp")
#   dtTemp[, timeStamp := as.POSIXct(timeStamp)]
#   namesToChange <- names(dtTemp)[!names(dtTemp) %in% "timeStamp"]
#   if (!i %in% "o3_official") setnames(dtTemp, old = namesToChange, new = paste(namesToChange, i,sep = "_"))
#   assign(i, dtTemp)
# }

# now average mergedDT to 15 minutes
x <- "15"
timeBreak <- paste0(x, " min")
varsToAve <-  names(mergedDT)[!names(mergedDT) %in% "timeStamp"]
mergedDT <- mergedDT[, lapply(.SD, mean, na.rm = TRUE ), 
                     by = list(cut.POSIXt(mergedDT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
setnames(mergedDT, old = "cut.POSIXt", new = "timeStamp")
mergedDT[, timeStamp := as.POSIXct(timeStamp)]

# drop N rows of the egg data; this means N * 15 minutes of data
N = 10
mergedDT <- mergedDT[-(1:N),] 

write.csv(mergedDT, file = gzfile("results/mergedDT.15min.zip"))
write.csv(mergedDT, file = "results/mergedDT.15min.csv")
zip(zipfile = "results/mergedDT.15min.zip", files = c("results/mergedDT.15min.csv"))

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

#do voltages
mergedDT.long.rawVal <- mergedDT.long[measurement %in% c("rawVal")]
wideFormula <- "timeStamp + eggType + eggSerial ~ gas"
mergedDT.wide.rawVal <- data.table::dcast(data = mergedDT.long.rawVal,
                                          formula = wideFormula,
                                          value.var = "value")
mergedDT.wide.rawVal[, eggName := paste(eggType, eggSerial, sep = "_")]

# do NO2 second voltage. This one is called auxiliary electrode voltage; the first one is called working electrode voltage
mergedDT.long.rawVal2 <- mergedDT.long[measurement %in% c("rawVal2")]
wideFormula <- "timeStamp + eggType + eggSerial ~ gas"
mergedDT.wide.rawVal2 <- data.table::dcast(data = mergedDT.long.rawVal2,
                                          formula = wideFormula,
                                          value.var = "value")
mergedDT.wide.rawVal2[, eggName := paste(eggType, eggSerial, sep = "_")]


#'  plots
#' rh
#temp <- mergedDT.wide.compVal[eggName %in% c("eggNO2CO.2","eggNO2CO.5", "eggNO2O3.3", "eggNO2O3.4", "eggPM.1")]
temp <- mergedDT.wide.compVal[eggType %in% c("no2co","no2o3", "particulate")]
p.rh <- ggplot(data = temp, aes(x = timeStamp, y = humidity, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "RH (percent)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of relative humidity values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.rh, tooltip = c("timeStamp", "humidity", dynamicTicks = TRUE))
#p.rh

'absolute humidity'
temp <- mergedDT.wide.compVal[eggType %in% c("no2co","no2o3", "particulate")]
p.ah <- ggplot(data = temp, aes(x = timeStamp, y = ah, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "absolute humidity (g/m^3)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of absolute humidity values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.ah, tooltip = c("timeStamp", "humidity", dynamicTicks = TRUE))
#p.ah

#' temp
#temp <- mergedDT.wide.compVal[eggName %in% c("eggNO2CO.2","eggNO2CO.5", "eggNO2O3.3", "eggNO2O3.4", "eggPM.1")]
temp <- mergedDT.wide.compVal[eggType %in% c("no2co","no2o3", "particulate")]
p.temp <- ggplot(data = temp, aes(x = timeStamp, y = temperature, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "Temperature (degrees F)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of temperature values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.temp, tooltip = c("timeStamp", "temperature", dynamicTicks = TRUE))
# p.temp

# no2
#temp <- mergedDT.wide.compVal[eggType %in% c("no2co","no2o3")]
temp <- mergedDT.wide.compVal[eggType %in% c("no2o3")]
p.no2 <- ggplot(data = temp, aes(x = timeStamp, y = no2, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "NO<sub>2</sub> (ppb)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of NO<sub>2</sub> values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.no2, tooltip = c("timeStamp", "no2"), dynamicTicks = TRUE)
#p.no2

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
#p.o3

# o3_v
temp <- mergedDT.wide.rawVal[eggType %in% c( "no2o3")]
p.o3_v <- ggplot(data = temp, aes(x = timeStamp, y = o3, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "voltage") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of O<sub>3</sub> voltage values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.o3_v, tooltip = c("timeStamp", "o3"), dynamicTicks = TRUE)
#p.o3_v

# no2_v1 - working electrode
temp <- mergedDT.wide.rawVal[eggType %in% c("no2o3")]
p.no2_v <- ggplot(data = temp, aes(x = timeStamp, y = no2, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "voltage") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of NO<sub>2</sub> working electrode voltage values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.no2_v, tooltip = c("timeStamp", "no2"), dynamicTicks = TRUE)
#p.no2_v

# no2_v2 - auxiliary electrode
temp <- mergedDT.wide.rawVal2[eggType %in% c("no2o3")]
p.no2_v2 <- ggplot(data = temp, aes(x = timeStamp, y = no2, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "voltage") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of NO<sub>2</sub> auxiliary electrode voltage values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.no2_v2, tooltip = c("timeStamp", "no2"), dynamicTicks = TRUE)
#p.no2_v


# start of regression stuff

#' scatter plots 
#' no2 voltage values
mergedDT[, no2vdiff1 := no2_rawVal_volt_no2o3_19b0111  - no2_rawVal_volt_no2o3_48b0153]
mergedDT[, no2vdiff2 := no2_rawVal2_volt_no2o3_19b0111 - no2_rawVal2_volt_no2o3_48b0153]

# NO2 voltage 1 scatter plot
p.no2_v1_scatter <- ggplot(data = mergedDT,aes(x = no2_rawVal_volt_no2o3_19b0111, 
                         y = no2_rawVal_volt_no2o3_48b0153
)) +
  geom_point(alpha = 0.2) + 
  theme_bw() +
  ggtitle("NO<sub>2</sub> working electrode voltage values scatter plot") +
  theme(plot.title = element_text(hjust = 0.5)) # center title
ggplotly(p.no2_v1_scatter,  dynamicTicks = TRUE)

# NO2 voltage 2 scatter plot
p.no2_v2_scatter <- ggplot(data = mergedDT,aes(x = no2_rawVal2_volt_no2o3_19b0111, 
                           y = no2_rawVal2_volt_no2o3_48b0153
)) +
  geom_point(alpha = 0.2) + 
  theme_bw() +
  ggtitle("NO<sub>2</sub> auxiliary electrode voltage values scatter plot") +
  theme(plot.title = element_text(hjust = 0.5)) # center title
ggplotly(p.no2_v2_scatter,  dynamicTicks = TRUE)

# NO2 measured vs calculated scatter plot, egg 19b0111
p.no2_ppb_scatter.19b0111 <- ggplot(data = mergedDT,aes(x = no2_estVal_ppb_no2o3_19b0111, 
                                               y = no2_compVal_ppb_no2o3_19b0111
)) +
  geom_point(alpha = 0.2) + 
  theme_bw() +
  ggtitle("NO<sub>2</sub> estimated versus calculated concentration scatter plot, egg 19b0111") +
  theme(plot.title = element_text(hjust = 0.5)) # center title
ggplotly(p.no2_ppb_scatter.19b0111,  dynamicTicks = TRUE)


# NO2 measured vs calculated scatter plot, egg 48b0153
p.no2_ppb_scatter.48b0153 <- ggplot(data = mergedDT,aes(x = no2_estVal_ppb_no2o3_48b0153, 
                                                        y = no2_compVal_ppb_no2o3_48b0153
)) +
  geom_point(alpha = 0.2) + 
  theme_bw() +
  ggtitle("NO<sub>2</sub> estimated versus calculated concentration scatter plot, egg 48b0153") +
  theme(plot.title = element_text(hjust = 0.5)) # center title
ggplotly(p.no2_ppb_scatter.48b0153,  dynamicTicks = TRUE)


# NO2 voltage 1 difference plot
p.no2_v_diff <- ggplot(data = mergedDT, aes(x = timeStamp, y = no2vdiff1)) + 
  labs(x = "time", y = "voltage") +
  scale_y_continuous() +
  theme_bw() +
ggtitle("Difference of NO<sub>2</sub> working electrode voltage values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.no2_v_diff, tooltip = c("timeStamp", "no2vdiff1"), dynamicTicks = TRUE)

# NO2 voltage 2 difference plot
p.no2_v_diff <- ggplot(data = mergedDT, aes(x = timeStamp, y = no2vdiff2)) + 
  labs(x = "time", y = "voltage") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Difference of NO<sub>2</sub> auxiliary electrode voltage values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.no2_v_diff, tooltip = c("timeStamp", "no2vdiff2"), dynamicTicks = TRUE)

# NO2 48b0153 concentration versus voltage 1 scatter plot
p.no2comp_v1_scatter <- ggplot(data = mergedDT,aes(x = no2_compVal_ppb_no2o3_48b0153, 
                                               y = no2_rawVal_volt_no2o3_48b0153
)) +
  geom_point(alpha = 0.2) + 
  theme_bw() +
  ggtitle("NO<sub>2</sub> concentration versus working electrode voltage values scatter plot, egg 48b0153") +
  theme(plot.title = element_text(hjust = 0.5)) # center title
ggplotly(p.no2comp_v1_scatter,  dynamicTicks = TRUE)

# NO2 19b0111 concentration versus voltage 1 scatter plot
p.no2comp_v1_scatter <- ggplot(data = mergedDT,aes(x = no2_compVal_ppb_no2o3_19b0111, 
                                                   y = no2_rawVal_volt_no2o3_19b0111
)) +
  geom_point(alpha = 0.2) + 
  theme_bw() +
  ggtitle("NO<sub>2</sub> concentration versus working electrode voltage values scatter plot, egg 19b0111") +
  theme(plot.title = element_text(hjust = 0.5)) # center title
ggplotly(p.no2comp_v1_scatter,  dynamicTicks = TRUE)


no2v.fit <- lm(data = mergedDT, no2_compVal_ppb_no2o3_19b0111 ~ no2_compVal_ppb_no2o3_48b0153)
summary(no2v.fit)
plot(no2v.fit$fitted.values)

no2v.fit <- lm(data = mergedDT, no2_compVal_ppb_no2o3_19b0111 ~ no2_compVal_ppb_no2o3_48b0153 + humidity_conVal_percent_no2o3_48b0153 + o3_rawVal_volt_no2o3_48b0153)
summary(no2v.fit)
plot(no2v.fit$fitted.values)

#' o3 voltage values
mergedDT[,no2vdiff := no2_compVal_ppb_no2o3_19b0111, no2_compVal_ppb_no2o3_48b0153]
ggplot(data = mergedDT,aes(x = no2_compVal_ppb_no2o3_19b0111, 
                           y = no2_compVal_ppb_no2o3_48b0153
)) +
  geom_point(alpha = 0.2)

p.no2_v_diff <- ggplot(data = mergedDT, aes(x = timeStamp, y = no2vdiff)) + 
  labs(x = "Time", y = "voltage") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Difference of NO<sub>2</sub> voltage values") +
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  geom_line()
ggplotly(p.no2_v_diff, tooltip = c("timeStamp", "no2vdiff"), dynamicTicks = TRUE)

# work just on egg00802aaa019b0111 because it has newer calibration
o3fit_19b0111 <- lm(data = mergedDT, o3_compVal_ppb_official ~ 
                      o3_rawVal_volt_no2o3_19b0111 + 
                      I(o3_rawVal_volt_no2o3_19b0111^2) + 
                      no2_rawVal_volt_no2o3_19b0111 + 
                      I(no2_rawVal_volt_no2o3_19b0111^2) +
                      no2_rawVal2_volt_no2o3_19b0111 +
                      I(no2_rawVal2_volt_no2o3_19b0111^2) +
                      temperature_conVal_degF_no2o3_19b0111 + 
                      I(temperature_conVal_degF_no2o3_19b0111^2) +
                      ah_rawVal_g.m3_no2o3_19b0111 +
                     I(ah_rawVal_g.m3_no2o3_19b0111^2)
                   , na.action=na.exclude) # na.action=na.exclude keeps the na rows around
summary(o3fit_19b0111)
#plot(o3fit_19b0111$residuals,xlab = "time", ylab="regression residuals")
# 
# # see https://drsimonj.svbtle.com/visualising-residuals for residuals graphing discussion
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(o3fit_19b0111)

#add fit and residuals to mergedDT
mergedDT[, predicted := predict(o3fit_19b0111) ][, residuals := residuals(o3fit_19b0111)]  
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
    values = c(monitor = "darkgreen", predicted = "red")) +
#  guides(color = FALSE) +  # Color legend removed
#  geom_segment(aes(x = timeStamp, xend = timeStamp, y = o3_compVal_ppb_official, yend = predicted),  color = "lightgrey") + # connect measure value and calculated value
  theme_bw()
ggplotly(p.o3pred)

