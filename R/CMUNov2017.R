Sys.setenv(TZ = "America/Denver") # needed to get rid of a warning message
library(openxlsx)
library(data.table)
library(ggplot2)
library(zip)
library(tidyr)
library(stringr)

# read in the egg data and create  eggList. The suffix number is the number that James put on the bottom of each egg. 
eggPM.1 <- readRDS(file = "results/CMUNov2017_particulate_d880112.RDS")
eggList <- deparse(substitute(eggPM.1))
eggNO2CO.2 <- readRDS(file = "results/CMUNov2017_no2co_89b0153.RDS")
eggList <- c(eggList, deparse(substitute(eggNO2CO.2)))
eggNO2CO.5 <- readRDS(file = "results/CMUNov2017_no2co_6080140.RDS")
eggList <- c(eggList, deparse(substitute(eggNO2CO.5)))
eggNO2O3.4 <- readRDS(file = "results/CMUNov2017_no2o3_48b0153.RDS")
eggList <- c(eggList, deparse(substitute(eggNO2O3.4)))
eggNO2O3.3 <- readRDS(file = "results/CMUNov2017_no2o3_19b0111.RDS")
eggList <- c(eggList, deparse(substitute(eggNO2O3.3)))

# remove columns that are all NA
for (i in eggList) {
  dt <- Filter(function(x)!all(is.na(x)), eval(parse(text = i)))
  assign(i, dt)
}

# delete misc variables
for (i in eggList) {
  dt <- eval(parse(text = i))
  deleteList.inst <- names(dt)[str_detect(names(dt), "Inst")] #get rid of variables of instantaneous values
  deleteList.lon <- names(dt)[str_detect(names(dt), "_lon")] #get rid of variables of longitude
  deleteList.lat <- names(dt)[str_detect(names(dt), "_lat")] #get rid of variables of longitude
  deleteList.alt <- names(dt)[str_detect(names(dt), "_alt")] #get rid of variables of longitude
  dt[, (deleteList.inst) := NULL]
  dt[, (deleteList.lon) := NULL]
  dt[, (deleteList.lat) := NULL]
  dt[, (deleteList.alt) := NULL]
  assign(i, dt)
}

#' For temperature and humidity, converted-value may be of interest, if you are going to try and apply your own 
#' temperature compensation algorithm, as it is a better reflection of the temperature inside the Egg.
#' 
#' Bottom line is you should just ignore converted-value as it pertains to the gas sensors, it's purely academic / diagnostic.
#' I'm going to remove converted value for all the gas and particulate sensors in curltest.R


# data from the official monitor need to be handled differently.
o3_official <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/CMUNov2017/3nov2017_49c.csv", na.strings = "---", 
                                      stringsAsFactors = FALSE, header = TRUE))
o3_official[, timeStamp := gsub(":23", ":00", timeStamp)]
o3_official[, timeStamp := as.POSIXct(timeStamp, format = "%Y-%m-%d-%H:%M:%S", tz = "America/Denver")]

setkey(o3_official, timeStamp)
write.csv(o3_official, file = "results/CMUNov2017.o3_official.csv")
zipfileList <- paste("results/CMUNov2017", "o3_official", "csv", sep = ".")
# 
# # now average o3.official to 5 minutes
# x <- "5"
# timeBreak <- paste0(x, " min")
# varsToAve <-  names(o3_official)[!names(o3_official) %in% "timeStamp"]
# o3_official <- o3_official[, lapply(.SD, mean, na.rm = FALSE ), 
#                            by = list(cut.POSIXt(o3_official$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
# setnames(o3_official, old = "cut.POSIXt", new = "timeStamp")
# o3_official[, timeStamp := as.POSIXct(timeStamp)]
# 
# 
# drop 15 minutes of the egg data
N = 15
for (i in eggList) {
  dt <- eval(parse(text = i))
  dt <- dt[N + 1:(nrow(dt) - N),]
  assign(i, dt)
  outFile <- paste("results/CMUNov2017", i, "csv", sep = ".")
  zipfileList <- c(zipfileList, outFile)
  write.csv(dt, file = outFile)
}

zip(zipfile = "results/CMUNov2017.zip", files = zipfileList)


# average to every x minutes
# first average everything to 1 minute to get all eggs on the same time
x <- "1"
timeBreak <- paste0(x, " min")
for (i in c(eggList, "o3_official")) {
  dtTemp <- eval(parse(text = i))
  varsToAve <-  names(dtTemp)[!names(dtTemp) %in% "timeStamp"]
  dtTemp <- dtTemp[, lapply(.SD, mean, na.rm = FALSE ), 
                   by = list(cut.POSIXt(dtTemp$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
  setnames(dtTemp, old = "cut.POSIXt", new = "timeStamp")
  dtTemp[, timeStamp := as.POSIXct(timeStamp)]
  namesToChange <- names(dtTemp)[!names(dtTemp) %in% "timeStamp"]
  if (!i %in% "o3_official") setnames(dtTemp, old = namesToChange, new = paste(namesToChange, i,sep = "_"))
#  setkey(dtTemp, timeStamp)
  assign(i, dtTemp)
}

fulldtList <- c("o3_official", eggList)
#' the commented out code is replaced by the mget statement below it.
# dtList <- list()
# for (k in 1:length(fulldtList)) {
#    dtList[[fulldtList[k]]] <- eval(parse(text = fulldtList[k]))
# }
dtList <- mget(fulldtList)
mergedDT <- Reduce(function(...) merge(..., all = TRUE, by = "timeStamp"), dtList)

# now average mergedDT to 15 minutes
x <- "15"
timeBreak <- paste0(x, " min")
varsToAve <-  names(mergedDT)[!names(mergedDT) %in% "timeStamp"]
mergedDT <- mergedDT[, lapply(.SD, mean, na.rm = TRUE ), 
                     by = list(cut.POSIXt(mergedDT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
setnames(mergedDT, old = "cut.POSIXt", new = "timeStamp")
mergedDT[, timeStamp := as.POSIXct(timeStamp)]

write.csv(mergedDT, file = gzfile("results/mergedDT.15min.zip"))
write.csv(mergedDT, file = "results/mergedDT.15min.csv")
zip(zipfile = "results/mergedDT.15min.zip", files = c("results/mergedDT.15min.csv"))

mergedDT.long <- data.table::melt(
  mergedDT,
  id.vars = c("timeStamp")
)

mergedDT.long[, c("gas", "measurement", "units", "eggName") := tstrsplit(variable, "_", fixed = TRUE)]
mergedDT.long[, variable := NULL]
mergedDT.long[measurement %in% "conVal", measurement := "compVal"] # temp and humidity are stored as conVals. We want all to be called compVals.

mergedDT.long.compVal <- mergedDT.long[measurement %in% "compVal",]
wideFormula <- "timeStamp + eggName ~ gas"
mergedDT.wide.compVal <- data.table::dcast(data = mergedDT.long.compVal,
                                   formula = wideFormula,
                                   value.var = "value")

#do voltages
mergedDT.long.rawVal <- mergedDT.long[measurement %in% "rawVal",]
wideFormula <- "timeStamp + eggName ~ gas"
mergedDT.wide.rawVal <- data.table::dcast(data = mergedDT.long.rawVal,
                                   formula = wideFormula,
                                   value.var = "value")


#  plots
theme_update(plot.title = element_text(hjust = 0.5)) # center title
# rh
temp <- mergedDT.wide[eggName %in% c("eggNO2CO.2","eggNO2CO.5", "eggNO2O3.3", "eggNO2O3.4", "eggPM.1")]
p.rh <- ggplot(data = temp, aes(x = timeStamp, y = humidity, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "RH (percent)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of relative humidity values") +
  geom_line()
p.rh

# temp
temp <- mergedDT.wide[eggName %in% c("eggNO2CO.2","eggNO2CO.5", "eggNO2O3.3", "eggNO2O3.4", "eggPM.1")]
p.temp <- ggplot(data = temp, aes(x = timeStamp, y = temperature, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "Temperature (degrees F)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of temperature values") +
  geom_line()
p.temp

# no2
temp <- mergedDT.wide[eggName %in% c("eggNO2CO.2","eggNO2CO.5", "eggNO2O3.3", "eggNO2O3.4")]
p.no2 <- ggplot(data = temp, aes(x = timeStamp, y = no2, group = eggName, color = eggName)) + 
  labs(x="Time", y="NO2 (ppb)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of NO2 values") +
  geom_line()
p.no2

# o3
temp <- mergedDT.wide[eggName %in% c( "eggNO2O3.3", "eggNO2O3.4", "official")]
p.o3 <- ggplot(data = temp, aes(x = timeStamp, y = o3, group = eggName, color = eggName)) + 
  labs(x="Time", y="O3 (ppb)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of O3 values") +
  geom_line()
p.o3

# o3_v
temp <- mergedDT.wide.rawVal[eggName %in% c( "eggNO2O3.3", "eggNO2O3.4")]
p.o3_v <- ggplot(data = temp, aes(x = timeStamp, y = o3, group = eggName, color = eggName)) + 
  labs(x="Time", y="voltage") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of O3 voltage values") +
  geom_line()
p.o3_v

# no2_v
temp <- mergedDT.wide.rawVal[eggName %in% c( "eggNO2O3.3", "eggNO2O3.4")]
p.no2_v <- ggplot(data = temp, aes(x = timeStamp, y = no2, group = eggName, color = eggName)) + 
  labs(x="Time", y="voltage") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of NO2 voltage values") +
  geom_line()
p.no2_v



