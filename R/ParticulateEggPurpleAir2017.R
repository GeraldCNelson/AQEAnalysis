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

#' what egg to get info for
eggSerialList <- c("egg00802e31aba80123")

# egg00802e63038b0122
# egg00802d56461b0133
# egg00802e31aba80123
# egg00802e6306880122
# egg00802e6305980122
#' what period to get info for. 
#' Note: date and time information must use this format, including 24 hour clock
startDate <- "2018-01-04"
startTime <- "08:00:00" #include two digits in hours, min, and seconds.
endDate <- "2018-01-06"
endTime <- "14:00:00" #include two digits in hours, min, and seconds.

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
  # for this comparison make the egg name simpler
  #    eggName <- paste(eggType, serialTail, sep = "_")
  eggName <- "egg"
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

ZoneAPrimary <- "Palisade Buffer Zone (39.09728751040862 -108.39195368919371) Primary 12_19_2017 01_01_2018.csv"
ZoneBPrimary <- "Palisade Buffer Zone B (39.09728751040862 -108.39195368919371) Primary 12_19_2017 01_01_2018.csv"
ZoneASecondary <- "Palisade Buffer Zone (39.09728751040862 -108.39195368919371) Primary 12_19_2017 01_01_2018.csv"
ZoneBSecondary <- "Palisade Buffer Zone B (39.09728751040862 -108.39195368919371) Primary 12_19_2017 01_01_2018.csv"
location <- "~/Documents/workspace/AQEAnalysis/data-raw/purpleAirTestDec2017/"
# now get purple air data
Palisade.Buffer.Zone.A.Primary <-   as.data.table(read.csv(paste0(location,ZoneAPrimary), stringsAsFactors=FALSE))
Palisade.Buffer.Zone.A.Secondary <- as.data.table(read.csv(paste0(location,ZoneASecondary), stringsAsFactors=FALSE))
Palisade.Buffer.Zone.B.Primary <-   as.data.table(read.csv(paste0(location,ZoneBPrimary), stringsAsFactors=FALSE))
Palisade.Buffer.Zone.B.Secondary <- as.data.table(read.csv(paste0(location,ZoneBSecondary), stringsAsFactors=FALSE))
deleteListCol.Primary <-   c("entry_id", "UptimeMinutes", "RSSI_dbm", "PM1.0_CF_ATM_ug.m3",  "PM2.5_CF_ATM_ug.m3",  "PM10.0_CF_ATM_ug.m3", "X")
#deleteListCol.Secondary <- c("entry_id", "X0.3um.dl", "X0.5um.dl", "X1.0um.dl", "X2.5um.dl", "X5.0um.dl", "X10.0um.dl", "X")
deleteListCol.Secondary <- c("entry_id", "X")

Palisade.Buffer.Zone.A.Primary[, (deleteListCol.Primary) := NULL]
Palisade.Buffer.Zone.B.Primary[, (deleteListCol.Primary) := NULL]
Palisade.Buffer.Zone.A.Secondary[, (deleteListCol.Secondary) := NULL]
Palisade.Buffer.Zone.B.Secondary[, (deleteListCol.Secondary) := NULL]
setnames(Palisade.Buffer.Zone.A.Primary, 
         old = c("created_at", "Temperature_F", "Humidity_.", "PM2.5_CF_1_ug.m3"),
         new = c("timeStamp", "temperature_degF_pa", "humidity_percent_pa", "pm2p5a_ug.m3_pa"))
setnames(Palisade.Buffer.Zone.A.Secondary, 
#         old = c("created_at", "PM1.0_CF_1_ug.m3", "PM10_CF_1_ug.m3" ),
         old = c("created_at", "PM1.0_CF_ATM_ug.m3", "PM10.0_CF_ATM_ug.m3" ),
         new = c("timeStamp", "pm1p0a_ug.m3_pa", "pm10p0a_ug.m3_pa"))

setnames(Palisade.Buffer.Zone.B.Primary, 
         old = c("created_at", "Temperature_F", "Humidity_.", "PM2.5_CF_1_ug.m3"),
         new = c("timeStamp", "temperature_degF_pa", "humidity_percent_pa", "pm2p5b_ug.m3_pa"))
Palisade.Buffer.Zone.B.Primary[, c("temperature_degF_pa", "humidity_percent_pa") := NULL] # the columns are there but are all NAs
setnames(Palisade.Buffer.Zone.B.Secondary, 
 #         old = c("created_at", "PM1.0_CF_1_ug.m3", "PM10_CF_1_ug.m3" ),
         old = c("created_at", "PM1.0_CF_ATM_ug.m3", "PM10.0_CF_ATM_ug.m3" ),
         new = c("timeStamp", "pm1p0a_ug.m3_pa", "pm10p0a_ug.m3_pa"))

# convert timeStamp to posixct
Palisade.Buffer.Zone.A.Primary[, timeStamp := as.POSIXct(timeStamp, tz = "UTC")]
Palisade.Buffer.Zone.B.Primary[, timeStamp := as.POSIXct(timeStamp, tz = "UTC")]
Palisade.Buffer.Zone.A.Secondary[, timeStamp := as.POSIXct(timeStamp, tz = "UTC")]
Palisade.Buffer.Zone.B.Secondary[, timeStamp := as.POSIXct(timeStamp, tz = "UTC")]

# now convert to local time
Palisade.Buffer.Zone.A.Primary[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]
Palisade.Buffer.Zone.B.Primary[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]
Palisade.Buffer.Zone.A.Secondary[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]
Palisade.Buffer.Zone.B.Secondary[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]

# now average to 1 minute
x <- "1"
timeBreak <- paste0(x, " min")
PAlist <- c("Palisade.Buffer.Zone.A.Primary", "Palisade.Buffer.Zone.B.Primary", "Palisade.Buffer.Zone.A.Secondary", "Palisade.Buffer.Zone.B.Secondary")
for (i in PAlist) {
  DT <- eval(parse(text = i))
  varsToAve <-  names(DT)[!names(DT) %in% "timeStamp"]
  DT <- DT[, lapply(.SD, mean, na.rm = FALSE ), 
           by = list(cut.POSIXt(DT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
  setnames(DT, old = "cut.POSIXt", new = "timeStamp")
  DT[, timeStamp := as.POSIXct(timeStamp)]
  assign(i, DT)
}

#merge the egg and PA data together
listOfNames <- c("Palisade.Buffer.Zone.A.Primary", "Palisade.Buffer.Zone.B.Primary", 
                 "Palisade.Buffer.Zone.A.Secondary", "Palisade.Buffer.Zone.B.Secondary",
                 "mergedDT")
dtList <- mget(listOfNames)
mergedDT <- Reduce(function(...) merge(..., all = TRUE, by = "timeStamp"), dtList)

# get rid of some extraneous egg data
deleteListCol <- c("pm1p0_compVal_ug.m3_egg", "pm2p5_compVal_ug.m3_egg", "pm10p0_compVal_ug.m3_egg", "compVal_ppm_egg")
mergedDT[, (deleteListCol) := NULL]

# Purple air data starts at midnight. 
# delete time before the start date for the eggs
mergedDT <- mergedDT[timeStamp > as.POSIXct(paste(startDate, startTime)),]
# delete time after the end date for the eggs
mergedDT <- mergedDT[timeStamp < as.POSIXct(paste(endDate, endTime)),]

#convert egg temp to F and remove C  version
mergedDT[, temperature_compVal_degF_egg := (temperature_compVal_degC_egg * 9/5) + 32]
mergedDT[, temperature_rawVal_degF_egg := (temperature_rawVal_degC_egg * 9/5) + 32]
mergedDT[, c("temperature_rawVal_degC_egg", "temperature_compVal_degF_egg") := NULL]

mergedDT[, `:=` (pm1p0Delta_pa = pm1p0a_ug.m3_pa - pm1p0b_ug.m3_pa,
                 pm2p5Delta_pa = pm2p5a_ug.m3_pa - pm2p5b_ug.m3_pa,
                 pm10p0Delta_pa = pm10p0a_ug.m3_pa - pm10p0b_ug.m3_pa,
                 pm1p0Delta_egg = pm1p0a_compVal_ug.m3_egg - pm1p0b_compVal_ug.m3_egg,
                 pm2p5Delta_egg = pm2p5a_compVal_ug.m3_egg - pm2p5b_compVal_ug.m3_egg,
                 pm2p5Delta_eggRev = - pm2p5a_compVal_ug.m3_egg + pm2p5b_compVal_ug.m3_egg,
                 pm10p0Delta_egg = pm10p0a_compVal_ug.m3_egg - pm10p0b_compVal_ug.m3_egg
                 )]

# average to 5 minutes
x <- "5"
timeBreak <- paste0(x, " min")
varsToAve <-  names(mergedDT)[!names(mergedDT) %in% "timeStamp"]
mergedDT <- mergedDT[, lapply(.SD, mean, na.rm = TRUE ), 
         by = list(cut.POSIXt(mergedDT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
setnames(mergedDT, old = "cut.POSIXt", new = "timeStamp")
mergedDT[, timeStamp := as.POSIXct(timeStamp)]

# now get ready for graphing
mergedDT.long <- data.table::melt(
  mergedDT,
  id.vars = c("timeStamp")
)
mergedDT.long[, variable := as.character(variable)]

temperatureVars <- c("temperature_degF_pa", "temperature_rawVal_degF_egg")
rhVars <-          c("humidity_percent_pa", "humidity_rawVal_percent_egg")
pmVars <- c("pm1p0a_ug.m3_pa", "pm2p5a_ug.m3_pa", "pm10p0a_ug.m3_pa", 
            "pm1p0b_ug.m3_pa",  "pm2p5b_ug.m3_pa", "pm10p0b_ug.m3_pa",
            "pm1p0a_compVal_ug.m3_egg", "pm2p5a_compVal_ug.m3_egg", "pm10p0a_compVal_ug.m3_egg",
            "pm1p0b_compVal_ug.m3_egg", "pm2p5b_compVal_ug.m3_egg", "pm10p0b_compVal_ug.m3_egg")
pm1Vars <-   c("pm1p0a_ug.m3_pa", "pm1p0b_ug.m3_pa", "pm1p0a_compVal_ug.m3_egg", "pm1p0b_compVal_ug.m3_egg")
pm2.5Vars <- c("pm2p5a_ug.m3_pa", "pm2p5b_ug.m3_pa", "pm2p5a_compVal_ug.m3_egg", "pm2p5b_compVal_ug.m3_egg")
pm10Vars <-  c("pm1p0a_ug.m3_pa", "pm1p0b_ug.m3_pa", "pm10p0a_compVal_ug.m3_egg", "pm10p0b_compVal_ug.m3_egg")
pm1Delta <- c("pm1p0Delta_pa", "pm1p0Delta_egg")
pm2.5Delta <- c("pm2p5Delta_pa",  "pm2p5Delta_eggRev") #"pm2p5Delta_egg",
pm10Delta <- c("pm10p0Delta_pa", "pm10p0Delta_egg")

plots.pm <- list()
# plots
for (i in c("temperatureVars", "rhVars", "pm1Vars", "pm1Delta", 
            "pm2.5Vars", "pm2.5Delta", "pm10Vars", "pm10Delta")) local({
  DT <- mergedDT.long[variable %in% eval(parse(text = i)), ]
  i <- i
  if (i %in% "temperatureVars") {
    gasYLabel <- "Temperature ("~degree~"F)"
    gasinTitle <- "Temperature"
  }
  if (i %in% "rhVars") {
    gasYLabel <- "RH ("~percent~")"
    gasinTitle <- "Relative humidity"
  }
  if (i %in% "pm1Vars") {
    gasYLabel <- "(µg/"~m^3~")"
    gasinTitle <- "Particulates: PM 1.0"
  }
  if (i %in% "pm2.5Vars") {
    gasYLabel <- "(µg/"~m^3~")"
    gasinTitle <- "Particulates: PM 2.5"
  }
  if (i %in% "pm10Vars") {
    gasYLabel <- "(µg/"~m^3~")"
    gasinTitle <- "Particulates: PM 10"
  }
  if (i %in% "pm1Delta") {
    gasYLabel <- "(µg/"~m^3~")"
    gasinTitle <- "PM 1 A and B Sensor Differences"
  } 
  if (i %in% "pm2.5Delta") {
    gasYLabel <- "(µg/"~m^3~")"
    gasinTitle <- "PM 2.5 A and B Sensor Differences"
  } 
  if (i %in% "pm10Delta") {
    gasYLabel <- "(µg/"~m^3~")"
    gasinTitle <- "PM 10 A and B Sensor Differences"
  }
  
  p <- ggplot(data = DT, aes(x = timeStamp, y = value, group = variable, color = variable)) + 
    xlab("Time") +
    ylab(bquote(.(gasYLabel))) +
    scale_y_continuous() +
    theme_bw() +
    #  ggtitle(sprintf("%s\n egg %s", gasinTitle, eggName)) +
    ggtitle(gasinTitle) +
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))+ 
    theme(legend.title=element_blank())+ 
    geom_line()
  #   print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
  print(p)
  plots.pm[[i]] <<- p
})
multiplot(plotlist = plots.pm, cols = 2)


