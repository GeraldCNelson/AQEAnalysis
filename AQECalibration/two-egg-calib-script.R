#library(readr)
library(shiny)
library(shinythemes)
library(rsconnect)
library(data.table)
library(xtable)
timezone <- "America/Denver"
newNames.master <- c("time", "temp", "humidity", "no2", "o3", "no2_we_v", "no2_aux_v",
              "o3_v",  "lat", "long", "altitude")
# setAs("character","myDate", function(from) as.Date(from, format = "%d/%m/%Y %H:%M:%S"))
# setClass('myDate')

egg00802aaa019b0111_compensated <- read.csv("data/egg00802aaa019b0111-compensated.csv",
                                            stringsAsFactors = FALSE,
                                            na.strings = "---",
                                            check.names = FALSE,
                                            colClasses = c("character",rep("numeric",10)))
                                           
# `time` = col_datetime(format = "%m/%d/%Y %H:%M:%S"),
dt.master <- as.data.table(egg00802aaa019b0111_compensated)
setnames(dt.master, old = names(dt.master), new = newNames.master)

egg0080270b448b0153_compensated <- read.csv("data/egg0080270b448b0153-compensated.csv", 
                                            stringsAsFactors = FALSE,
                                            na.strings = "---",
                                            check.names = FALSE,
                                            colClasses = c("character",rep("numeric",10)))


dt.slave <- as.data.table(egg0080270b448b0153_compensated)

newNames.slave <- c("time", "temp.slave", "humidity.slave", "no2.slave", "o3.slave", "no2_we_v.slave", 
              "no2_aux_v.slave", "o3_v.slave",  "lat.slave", "long.slave", "altitude.slave")
setnames(dt.slave, old = names(dt.slave), new = newNames.slave)

keepListCol <- c("time", "no2", "o3")
dt.master <- dt.master[, (keepListCol), with = FALSE]
dt.master <- dt.master[complete.cases(dt.master),]

keepListCol <- c("time", "no2.slave", "o3.slave")
dt.slave <- dt.slave[, (keepListCol), with = FALSE]
dt.slave <- dt.slave[complete.cases(dt.slave),]

dt.master[, time := as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S", tz = timezone)]
dt.slave[, time :=  as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S", tz = timezone)]

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

lmResultsO3 <- lm(o3 ~ o3.slave, combinedResults)
coef(summary(lmResultsO3))["o3.slave","Estimate"]
coef(summary(lmResultsO3))["(Intercept)","Estimate"]

plot(combinedResults$time, combinedResults$o3.slave, type = "l", main = "O3 time series")
points(combinedResults$time, combinedResults$o3)
lines(combinedResults$time, combinedResults$o3)

