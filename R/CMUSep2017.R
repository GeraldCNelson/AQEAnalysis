Sys.setenv(TZ = "America/Denver") # needed to get rid of a warning message
library(openxlsx)
library(data.table)
library(ggplot2)
library(zip)
library(tidyr)

# eggNO2CO_2 has about 2 days data less than the other eggs

eggPM <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/CMUSept2017/egg1_15sep17.csv", na.strings = "---", stringsAsFactors=FALSE))
eggNO2CO_1 <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/CMUSept2017/egg2_15sep17.csv", na.strings = "---", stringsAsFactors=FALSE))
# eggNO2CO_2 <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/CMUSept2017/egg5_15sep17.csv", na.strings = "---", stringsAsFactors=FALSE))
eggNO2O3_1 <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/CMUSept2017/egg3_15sep17.csv", na.strings = "---", stringsAsFactors=FALSE))
eggNO2O3_2 <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/CMUSept2017/egg4_15sep17.csv", na.strings = "---", stringsAsFactors=FALSE))
setnames(eggNO2CO_1, old = names(eggNO2CO_1), new =  c("timeStamp", "temp.eggNO2CO_1", "rh.eggNO2CO_1", "no2.eggNO2CO_1", "co.eggNO2CO_1", "no2_v.eggNO2CO_1", "co_v.eggNO2CO_1", "lat", "long", "alt"))
#setnames(eggNO2CO_2, old = names(eggNO2CO_2), new =  c("timeStamp", "temp.eggNO2CO_2", "rh.eggNO2CO_2", "no2.eggNO2CO_2", "co.eggNO2CO_2", "no2_v.eggNO2CO_2", "co_v.eggNO2CO_2", "lat", "long", "alt"))
setnames(eggPM, old = names(eggPM), new =  c("timeStamp", "temp.eggPM", "rh.eggPM", "pm.eggPM", "pm_v.eggPM", "lat", "long", "alt"))
setnames(eggNO2O3_1, old = names(eggNO2O3_1), new =  c("timeStamp", "temp.eggNO2O3_1", "rh.eggNO2O3_1", "no2.eggNO2O3_1","o3.eggNO2O3_1", "no2_wev.eggNO2O3_1", "no2_auxv.eggNO2O3_1", "o3_v.eggNO2O3_1", "lat", "long", "alt"))
setnames(eggNO2O3_2, old = names(eggNO2O3_2), new =  c("timeStamp", "temp.eggNO2O3_2", "rh.eggNO2O3_2", "no2.eggNO2O3_2","o3.eggNO2O3_2", "no2_wev.eggNO2O3_2", "no2_auxv.eggNO2O3_2", "o3_v.eggNO2O3_2", "lat", "long", "alt"))

# convert egg 4 temp to fahrenheit; forgot to change the setting ahead of time. This fixes it.
eggNO2O3_2[, temp.eggNO2O3_2 := temp.eggNO2O3_2 * 9/5 + 32]

# remove lat, long, alt columns
eggNO2CO_1[,  c("lat", "long", "alt") := NULL]
#eggNO2CO_2[,  c("lat", "long", "alt") := NULL]
eggNO2O3_1[,  c("lat", "long", "alt") := NULL]
eggNO2O3_2[,  c("lat", "long", "alt") := NULL]
eggPM[,  c("lat", "long", "alt") := NULL]

# Both the particulates and one of the NO2CO eggs (eggNO2CO_2) include real time clocks so their start dates and time should be the same. 
# in fact they differ by about a minute. The next two lines of code use egg eggPM as the starting time to be used with 
# the eggs that don't have a clock 
timeBase <- as.POSIXct( eggPM$timeStamp[1], format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")
eggNO2CO_1[,timeStamp := as.POSIXct(timeStamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")]
eggNO2CO_1[,timeStamp := timeStamp + (timeBase - eggNO2CO_1$timeStamp[1])]

#eggNO2CO_2[,timeStamp := as.POSIXct(timeStamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")] # has clock so don't need to adjust

eggNO2O3_1[,timeStamp := as.POSIXct(timeStamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")]
eggNO2O3_1[,timeStamp := timeStamp + (timeBase - timeStamp[1])]

eggNO2O3_2[,timeStamp := as.POSIXct(timeStamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")]
eggNO2O3_2[,timeStamp := timeStamp + (timeBase - timeStamp[1])]

eggPM[,timeStamp := as.POSIXct(timeStamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")] # has clock so don't need to adjust

# data from the official monitor need to be handled differently.
o3_official <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/CMUSept2017/18sep17_49c.csv", na.strings = "---", stringsAsFactors=FALSE, header = FALSE))
setnames(o3_official, old = names(o3_official),  new = c("o3.o3_official", "timeStamp"))
o3_official[, rh.o3_official := NA][, temp.o3_official := NA]

#drop first 7 rows of o2.official because no egg data
o3_official <- o3_official[-(1:7),]
o3_official[, timeStamp := as.POSIXct(timeStamp, format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")] 

# drop 15 minutes of the egg data
N = 15
eggNO2CO_1 <- eggNO2CO_1[N + 1:(nrow(eggNO2CO_1)-N),]
#eggNO2CO_2 <- eggNO2CO_2[N + 1:(nrow(eggNO2CO_2)-N),]
eggNO2O3_1 <- eggNO2O3_1[N + 1:(nrow(eggNO2O3_1)-N),]
eggNO2O3_2 <- eggNO2O3_2[N + 1:(nrow(eggNO2O3_2)-N),]
eggPM <- eggPM[N + 1:(nrow(eggPM)-N),]

write.csv(eggNO2CO_1, file = "results/CMUSep2017.eggNO2CO_1.csv")
#write.csv(eggNO2CO_2, file = "results/CMUSep2017.eggNO2CO_2.csv")
write.csv(eggNO2O3_1, file = "results/CMUSep2017.eggNO2O3_1.csv")
write.csv(eggNO2O3_2, file = "results/CMUSep2017.eggNO2O3_2.csv")
write.csv(eggPM, file = "results/CMUSep2017.eggPM.csv")
write.csv(o3_official, file = "results/CMUSep2017.o3_official.csv")
zip(zipfile = "results/CMUSep2017.zip", files = c("results/CMUSep2017.eggNO2CO_1.csv", #, "results/CMUSep2017.eggNO2CO_2.csv"
                                                  "results/CMUSep2017.eggNO2O3_1.csv", "results/CMUSep2017.eggNO2O3_2.csv", 
                                                  "results/CMUSep2017.eggPM.csv", "results/CMUSep2017.o3_official.csv"))


# average to every x minutes
# first average everything to 1 minute to get all eggs on the same time
x <- "1"
timeBreak <- paste0(x, " min")
for (i in c("eggNO2CO_1",  "eggNO2O3_1", "eggNO2O3_2", "eggPM", "o3_official")) { #"eggNO2CO_2",
  dtTemp <- eval(parse(text = i))
  varsToAve <-  names(dtTemp)[!names(dtTemp) %in% "timeStamp"]
  dtTemp <- dtTemp[, lapply(.SD, mean, na.rm = TRUE ), 
                           by = list(cut.POSIXt(dtTemp$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
  setnames(dtTemp, old = "cut.POSIXt", new = "timeStamp")
  dtTemp[, timeStamp := as.POSIXct(timeStamp)]
  assign(i, dtTemp)
}

setkey(eggNO2CO_1, timeStamp)
#setkey(eggNO2CO_2, timeStamp)
setkey(eggNO2O3_1, timeStamp)
setkey(eggNO2O3_2, timeStamp)
setkey(eggPM, timeStamp)
setkey(o3_official, timeStamp)
mergedDT <- eggNO2CO_1[eggNO2O3_1,][eggNO2O3_2,][eggPM,][o3_official,]

mergedDT[, rh.o3_official := NA]
mergedDT[, temp.o3_official := NA]

# remove NAs because there are 4 rows with NAs for all eggs. A bit of a kludge to do this
mergedDT <- mergedDT[!is.na(temp.eggNO2CO_1),]

# now average mergedDT to 15 minutes
x <- "15"
timeBreak <- paste0(x, " min")

  varsToAve <-  names(mergedDT)[!names(mergedDT) %in% "timeStamp"]
  mergedDT <- mergedDT[, lapply(.SD, mean, na.rm = FALSE ), 
                   by = list(cut.POSIXt(mergedDT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
  setnames(mergedDT, old = "cut.POSIXt", new = "timeStamp")
  mergedDT[, timeStamp := as.POSIXct(timeStamp)]
  assign(i, dtTemp)

  write.csv(mergedDT, file = gzfile("results/mergedDT.15min.zip"))
  write.csv(mergedDT, file = "results/mergedDT.15min.csv")
  zip(zipfile = "results/mergedDT.15min.zip", files = c("results/mergedDT.15min.csv"))
  
mergedDT.long <- data.table::melt(
  mergedDT,
  id.vars = c("timeStamp")
)

mergedDT.long[, c("gas", "eggName") := tstrsplit(variable, ".", fixed=TRUE)]
mergedDT.long[, variable:= NULL]
wideFormula <- "timeStamp + eggName ~ gas"
mergedDT.wide <- data.table::dcast(data = mergedDT.long,
                                                 formula = wideFormula,
                                                 value.var = "value")

  # test plots
# rh
temp <- mergedDT.wide[eggName %in% c("eggNO2CO_1", "eggNO2O3_1", "eggNO2O3_2", "eggPM")]
p.rh <- ggplot(data = temp, aes(x = timeStamp, y = rh, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "RH (percent)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of relative humidity values") +
  geom_line()
p.rh

# temp
temp <- mergedDT.wide[eggName %in% c("eggNO2CO_1", "eggNO2O3_1", "eggNO2O3_2", "eggPM")]
p.temp <- ggplot(data = temp, aes(x = timeStamp, y = temp, group = eggName, color = eggName)) + 
  labs(x = "Time", y = "Temperature (degrees F)") +
  scale_y_continuous() +
  theme_bw() +
  ggtitle("Combined plots of temperature (F) values") +
  geom_line()
p.temp

# no2
temp <- mergedDT.wide[eggName %in% c("eggNO2CO_1", "eggNO2O3_1", "eggNO2O3_2")]
  p.no2 <- ggplot(data = temp, aes(x = timeStamp, y = no2, group = eggName, color = eggName)) + 
    labs(x="Time", y="NO2 (ppb)") +
    scale_y_continuous() +
    theme_bw() +
    ggtitle("Combined plots of NO2 values") +
  geom_line()
  p.no2

# o3
  temp <- mergedDT.wide[eggName %in% c( "eggNO2O3_1", "eggNO2O3_2", "o3_official")]
  p.o3 <- ggplot(data = temp, aes(x = timeStamp, y = o3, group = eggName, color = eggName)) + 
    labs(x="Time", y="O3 (ppb)") +
    scale_y_continuous() +
    theme_bw() +
    ggtitle("Combined plots of O3 values") +
    geom_line()
  p.o3
  
  # o3_v
  temp <- mergedDT.wide[eggName %in% c( "eggNO2O3_1", "eggNO2O3_2")]
  p.o3 <- ggplot(data = temp, aes(x = timeStamp, y = o3_v, group = eggName, color = eggName)) + 
    labs(x="Time", y="voltage") +
    scale_y_continuous() +
    theme_bw() +
    ggtitle("Combined plots of O3 voltage values") +
    geom_line()
  p.o3
 
  
