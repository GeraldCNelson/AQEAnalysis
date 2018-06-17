library(openxlsx)
library(data.table)
library(ggplot2)
library(zip)
library(tidyr)
library(stringr)
library(plotly)
library(gridExtra)
source("R/eggdataRetrieval.R")
source("/Users/gcn/Documents/workspace/nutmod/R/nutrientModFunctions.R")

#' #' what time zone are the data from
 tz <- "America/Denver"

#' what eggs to get info for
eggSerialList <- c("egg00802e31aba80123", "egg00802aaa019b0111", "egg0080270b448b0153") # particulates and 2 NO2/O3 eggs
eggSerialList <- c("egg00802aaa019b0111", "egg0080270b448b0153") #  2 NO2/O3 eggs

#' what period to get info for. 
#' Note: date and time information must use this format, including 24 hour clock
startDate <- "2018-02-23"
startTime <- "15:00:00" #include two digits in hours, min, and seconds.
endDate <- "2018-03-11"
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
  eggName <- paste("egg", eggType, serialTail, sep = "_")
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

CMUO3NO2 <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/CMUMar2018/CMUO3O2.csv", stringsAsFactors=FALSE))
setnames(CMUO3NO2, old = c("O3..ppb.", "NO..ppb.", "NO2..ppb.", "NOx..ppb.", "Time.and.Date"), 
new = c("o3", "no", "no2", "nox", "timeStamp"))
# convert timeStamp to posixct
CMUO3NO2[, timeStamp := as.POSIXct(timeStamp, format = " %a %b %d %H:%M:%S %Y",  tz = tz)]

# # now average CMUO3NO2 to 1 minute
x <- "1"
timeBreak <- paste0(x, " min")
  varsToAve <-  names(CMUO3NO2)[!names(CMUO3NO2) %in% "timeStamp"]
  CMUO3NO2 <- CMUO3NO2[, lapply(.SD, mean, na.rm = FALSE ),
           by = list(cut.POSIXt(CMUO3NO2$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
  setnames(CMUO3NO2, old = "cut.POSIXt", new = "timeStamp")
  CMUO3NO2[, timeStamp := as.POSIXct(timeStamp)]

#merge the egg and PA data together
listOfNames <- c("CMUO3NO2", "mergedDT")
dtList <- mget(listOfNames)
mergedDT <- Reduce(function(...) merge(..., all = TRUE, by = "timeStamp"), dtList)

# # get rid of some extraneous egg data
# deleteListCol <- c("pm1p0_compVal_ug.m3_egg", "pm2p5_compVal_ug.m3_egg", "pm10p0_compVal_ug.m3_egg", "compVal_ppm_egg")
# mergedDT[, (deleteListCol) := NULL]

# # Purple air data starts at midnight. 
# # delete time before the start date for the eggs
# mergedDT <- mergedDT[timeStamp > as.POSIXct(paste(startDate, startTime)),]
# # delete time after the end date for the eggs
# mergedDT <- mergedDT[timeStamp < as.POSIXct(paste(endDate, endTime)),]

#convert egg temp to F and remove C  version
# tempBegin.raw <- "temperature_rawVal"
# tempBegin.comp <- "temperature_compVal"

tempVars.degC <- c("temperature_rawVal_degC_egg_no2o3_48b0153", "temperature_compVal_degC_egg_no2o3_48b0153",
                  "temperature_compVal_degC_egg_no2o3_19b0111", "temperature_rawVal_degC_egg_no2o3_19b0111")
tempVars.degF <- c("temperature_rawVal_degF_egg_no2o3_48b0153", "temperature_compVal_degF_egg_no2o3_48b0153",
                   "temperature_compVal_degF_egg_no2o3_19b0111", "temperature_rawVal_degF_egg_no2o3_19b0111")

mergedDT[, (tempVars.degF) := (eval(parse(text = tempVars.degC)) * 9/5) + 32]
# mergedDT[, temperature_compVal_degF_egg := (temperature_compVal_degC_egg * 9/5) + 32]
# mergedDT[, temperature_rawVal_degF_egg := (temperature_rawVal_degC_egg * 9/5) + 32]
mergedDT[, (tempVars.degC) := NULL]

# average to X minutes
x <- "15"
timeBreak <- paste0(x, " min")
varsToAve <-  names(mergedDT)[!names(mergedDT) %in% "timeStamp"]
mergedDT <- mergedDT[, lapply(.SD, mean, na.rm = TRUE ), 
                     by = list(cut.POSIXt(mergedDT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
setnames(mergedDT, old = "cut.POSIXt", new = "timeStamp")
mergedDT[, timeStamp := as.POSIXct(timeStamp)]
inDT <- mergedDT
outname <- "CMUMarch"
cleanup(inDT, outname, destDir = "data")

# now get ready for graphing
mergedDT.long <- data.table::melt(
  mergedDT,
  id.vars = c("timeStamp")
)
mergedDT.long[, variable := as.character(variable)]

# temperatureVars <- c("temperature_degF_pa", "temperature_rawVal_degF_egg")
# rhVars <-          c("humidity_percent_pa", "humidity_rawVal_percent_egg")
o3vars <- c("o3", "o3_rawVal_volt_egg_no2o3_19b0111", "o3_compVal_ppb_egg_no2o3_19b0111",
            "o3_rawVal_volt_egg_no2o3_48b0153", "o3_compVal_ppb_egg_no2o3_48b0153")
no2vars <- c("no2", "no2_rawVal_volt_egg_no2o3_19b0111", "no2_rawValAux_volt_egg_no2o3_19b0111", "no2_compVal_ppb_egg_no2o3_19b0111", 
             "no2_rawVal_volt_egg_no2o3_48b0153", "no2_rawValAux_volt_egg_no2o3_48b0153", "no2_compVal_ppb_egg_no2o3_48b0153")
noxvars <- c("no", "no2", "nox")
write.csv(mergedDT, file = "combinedDataFeb23Mar11.csv")
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
