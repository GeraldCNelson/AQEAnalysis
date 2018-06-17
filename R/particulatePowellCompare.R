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
startTime <- "15:00:00" #include two digits in hours, min, and seconds.
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

PitkenShelter <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/pitkenShelterJan2018/Powell 001M 180108.csv", stringsAsFactors=FALSE))

# convert timeStamp to posixct
PitkenShelter[, timeStamp := as.POSIXct(timeStamp, format = "%d-%b-%Y %H:%M",  tz = tz)]

# # now average to 1 minute
# x <- "1"
# timeBreak <- paste0(x, " min")
# PAlist <- c("Palisade.Buffer.Zone.A.Primary", "Palisade.Buffer.Zone.B.Primary", "Palisade.Buffer.Zone.A.Secondary", "Palisade.Buffer.Zone.B.Secondary")
# for (i in PAlist) {
#   DT <- eval(parse(text = i))
#   varsToAve <-  names(DT)[!names(DT) %in% "timeStamp"]
#   DT <- DT[, lapply(.SD, mean, na.rm = FALSE ), 
#            by = list(cut.POSIXt(DT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
#   setnames(DT, old = "cut.POSIXt", new = "timeStamp")
#   DT[, timeStamp := as.POSIXct(timeStamp)]
#   assign(i, DT)
# }

#merge the egg and PA data together
listOfNames <- c("PitkenShelter", "mergedDT")
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

# calculate some new varaibles
mergedDT[, `:=` ( pm1p0Delta_egg = pm1p0a_compVal_ug.m3_egg - pm1p0b_compVal_ug.m3_egg,
                  pm2p5Delta_egg = pm2p5a_compVal_ug.m3_egg - pm2p5b_compVal_ug.m3_egg,
                # pm2p5Delta_eggRev = - pm2p5a_compVal_ug.m3_egg + pm2p5b_compVal_ug.m3_egg,
                  pm10p0Delta_egg = pm10p0a_compVal_ug.m3_egg - pm10p0b_compVal_ug.m3_egg,
                  pm10tot_a_egg = pm10p0a_compVal_ug.m3_egg + pm2p5a_compVal_ug.m3_egg + pm1p0a_compVal_ug.m3_egg, 
                  pm10tot_b_egg = pm10p0b_compVal_ug.m3_egg + pm2p5b_compVal_ug.m3_egg + pm1p0b_compVal_ug.m3_egg,
                  pm2p5tot_a_egg = pm2p5a_compVal_ug.m3_egg + pm1p0a_compVal_ug.m3_egg,
                  pm2p5tot_b_egg = pm2p5b_compVal_ug.m3_egg + pm1p0b_compVal_ug.m3_egg
)]

mergedDT[, `:=` ( pm2p5totAve_egg = (pm2p5tot_a_egg + pm2p5tot_b_egg)/2,
                  pm10totAve_egg = (pm10tot_a_egg + pm10tot_b_egg)/2
)]

mergedDT[, `:=` ( pm2p5diff = pmfine - pm2p5totAve_egg,
                  pm10diff = pm10ltp - pm10totAve_egg
)]
# average to X minutes
x <- "15"
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

# temperatureVars <- c("temperature_degF_pa", "temperature_rawVal_degF_egg")
# rhVars <-          c("humidity_percent_pa", "humidity_rawVal_percent_egg")
pmVars <- c( "pm1p0a_compVal_ug.m3_egg", "pm2p5a_compVal_ug.m3_egg", "pm10p0a_compVal_ug.m3_egg",
             "pm1p0b_compVal_ug.m3_egg", "pm2p5b_compVal_ug.m3_egg", "pm10p0b_compVal_ug.m3_egg")
pm1Vars <-   c("pm1p0a_compVal_ug.m3_egg", "pm1p0b_compVal_ug.m3_egg")
pm2.5Vars <- c("pmfine", "pm2p5totAve_egg", "pm2p5diff")
pm10Vars <-  c("pm10ltp", "pm10totAve_egg", "pm10diff")
#pm1Delta <- c("pm1p0Delta_pa", "pm1p0Delta_egg")
#pm2.5Delta <- c("pm2p5Delta_pa",  "pm2p5Delta_eggRev") #"pm2p5Delta_egg",
#pm10Delta <- c("pm10p0Delta_pa", "pm10p0Delta_egg")

plots.pm <- list()
# plots
# for (i in c("temperatureVars", "rhVars", "pm1Vars", "pm1Delta", 
#             "pm2.5Vars", "pm2.5Delta", "pm10Vars", "pm10Delta")) local({
for (i in c("pm1Vars", "pm2.5Vars", "pm10Vars")) local({
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

# generate nice looking summary data
dt.mergedDT.summary <- as.data.table(summary(mergedDT))
dt.mergedDT.summary[, V1 := NULL]
dt.mergedDT.summary <- dt.mergedDT.summary[!V2 %in% "  timeStamp", ] # not sure where the spaces in timeStamp come from
sumMeasures <- c("type", "value")
dt.mergedDT.summary <- dt.mergedDT.summary[, (sumMeasures) := data.table::tstrsplit(N, ":", fixed = TRUE)]
dt.mergedDT.summary[, N := NULL]

formula.wide <- "type ~ V2"
dt.mergedDT.summary.wide <- data.table::dcast(
  data = dt.mergedDT.summary,
  formula = formula.wide,
  value.var = "value")

#reorder the cols
setcolorder(dt.mergedDT.summary.wide, c("type", "pcGDPX0_woGlobe", "pcGDPX0_wGlobe", "budget_woGlobe",   "budget_wGlobe",  "incShare_woGlobe",  "incShare_wGlobe"))

#reorder the rows
dt.mergedDT.summary.wide <- dt.mergedDT.summary.wide[c(6,1,4,5,2,3), ]
