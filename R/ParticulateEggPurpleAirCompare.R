library(openxlsx)
library(data.table)
library(ggplot2)
#library(zip)
library(tidyr)
library(stringr)
library(plotly)
library(gridExtra)
#source("R/eggdataRetrieval.R")

tz = "America/Denver"

CCA_Powell_A_Primary <- as.data.table(read.csv("data-raw/purpleAirApril2017/CCA Powell (39.063823 -108.561336) Primary 04_01_2018 04_07_2018.csv", stringsAsFactors = FALSE))
CCA_Powell_A_Secondary <- as.data.table(read.csv("data-raw/purpleAirApril2017/CCA Powell (39.063823 -108.561336) Secondary 04_01_2018 04_07_2018.csv", stringsAsFactors = FALSE))
CCA_Powell_B_Primary <- as.data.table(read.csv("data-raw/purpleAirApril2017/CCA Powell B (39.063823 -108.561336) Primary 04_01_2018 04_07_2018.csv", stringsAsFactors = FALSE))
CCA_Powell_B_Secondary <- as.data.table(read.csv("data-raw/purpleAirApril2017/CCA Powell B (39.063823 -108.561336) Secondary 04_01_2018 04_07_2018.csv", stringsAsFactors = FALSE))

# deleteListCol.Primary <-   c("entry_id", "UptimeMinutes", "RSSI_dbm", "PM1.0_CF_ATM_ug.m3",  "PM2.5_CF_ATM_ug.m3",  "PM10.0_CF_ATM_ug.m3", "X")
# #deleteListCol.Secondary <- c("entry_id", "X0.3um.dl", "X0.5um.dl", "X1.0um.dl", "X2.5um.dl", "X5.0um.dl", "X10.0um.dl", "X")
# deleteListCol.Secondary <- c("entry_id", "X")

keepListColAPrimary <- c("created_at", "Temperature_F", "Humidity_.", "PM2.5_CF_1_ug.m3")
keepListColASecondary <- c("created_at", "PM1.0_CF_1_ug.m3", "PM10_CF_1_ug.m3" )
keepListColBPrimary <- c("created_at", "Temperature_F", "Humidity_.", "PM2.5_CF_1_ug.m3")
keepListColBSecondary <- c("created_at", "PM1.0_CF_1_ug.m3", "PM10_CF_1_ug.m3" )

CCA_Powell_A_Primary[, setdiff(names(CCA_Powell_A_Primary), keepListColAPrimary) := NULL]
CCA_Powell_B_Primary[, setdiff(names(CCA_Powell_B_Primary), keepListColBPrimary) := NULL]
CCA_Powell_A_Secondary[, setdiff(names(CCA_Powell_A_Secondary), keepListColASecondary) := NULL]
CCA_Powell_B_Secondary[, setdiff(names(CCA_Powell_B_Secondary), keepListColBSecondary) := NULL]

# CCA_Powell_A_Primary[, (deleteListCol.Primary) := NULL]
# CCA_Powell_B_Primary[, (deleteListCol.Primary) := NULL]
# CCA_Powell_A_Secondary[, (deleteListCol.Secondary) := NULL]
# CCA_Powell_B_Secondary[, (deleteListCol.Secondary) := NULL]

setnames(CCA_Powell_A_Primary, 
         old = c("created_at", "Temperature_F",      "Humidity_.",          "PM2.5_CF_1_ug.m3"),
         new = c("timeStamp", "temperature_degF_pa", "humidity_percent_pa", "pm2p5a_ug.m3_pa"))

setnames(CCA_Powell_A_Secondary, 
         old = c("created_at", "PM1.0_CF_1_ug.m3", "PM10_CF_1_ug.m3" ),
         new = c("timeStamp",   "pm1p0a_ug.m3_pa",  "pm10p0a_ug.m3_pa"))

setnames(CCA_Powell_B_Primary, 
         old = c("created_at", "Temperature_F",       "Humidity_.",          "PM2.5_CF_1_ug.m3"),
         new = c("timeStamp",  "temperature_degF_pa", "humidity_percent_pa", "pm2p5b_ug.m3_pa"))

#CCA_Powell_B_Primary[, c("temperature_degF_pa", "humidity_percent_pa") := NULL] # the columns are there but are all NAs
setnames(CCA_Powell_B_Secondary, 
         old = c("created_at", "PM1.0_CF_1_ug.m3",   "PM10_CF_1_ug.m3" ),
         new = c("timeStamp",   "pm1p0b_ug.m3_pa",    "pm10p0b_ug.m3_pa"))

# convert timeStamp to posixct
CCA_Powell_A_Primary[, timeStamp := as.POSIXct(timeStamp, tz = "UTC")]
CCA_Powell_B_Primary[, timeStamp := as.POSIXct(timeStamp, tz = "UTC")]
CCA_Powell_A_Secondary[, timeStamp := as.POSIXct(timeStamp, tz = "UTC")]
CCA_Powell_B_Secondary[, timeStamp := as.POSIXct(timeStamp, tz = "UTC")]

# convert timeStamp to posixct
CCA_Powell_A_Primary[, timeStamp := as.POSIXct(timeStamp, tz = tz)]
CCA_Powell_B_Primary[, timeStamp := as.POSIXct(timeStamp, tz = tz)]
CCA_Powell_A_Secondary[, timeStamp := as.POSIXct(timeStamp, tz = tz)]
CCA_Powell_B_Secondary[, timeStamp := as.POSIXct(timeStamp, tz = tz)]


# now convert to local time
CCA_Powell_A_Primary[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]
CCA_Powell_B_Primary[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]
CCA_Powell_A_Secondary[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]
CCA_Powell_B_Secondary[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]

# now average to 1 minute
x <- "1"
timeBreak <- paste0(x, " min")
PAlist <- c("CCA_Powell_A_Primary", "CCA_Powell_B_Primary", 
            "CCA_Powell_A_Secondary", "CCA_Powell_B_Secondary")
for (i in PAlist) {
  DT.1_min <- eval(parse(text = i))
  varsToAve <-  names(DT.1_min)[!names(DT.1_min) %in% "timeStamp"]
  DT.1_min <- DT.1_min[, lapply(.SD, mean, na.rm = FALSE ), 
                       by = list(cut.POSIXt(DT.1_min$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
  setnames(DT.1_min, old = "cut.POSIXt", new = "timeStamp")
  DT.1_min[, timeStamp := as.POSIXct(timeStamp)]
  assign(paste0(i, ".min"), DT.1_min)
}

# now average to 1 hour
x <- "1"
timeBreak <- paste0(x, " hour")
for (i in PAlist) {
  DT.1_hour <- eval(parse(text = i))
  varsToAve <-  names(DT.1_hour)[!names(DT.1_hour) %in% "timeStamp"]
  DT.1_hour <- DT.1_hour[, lapply(.SD, mean, na.rm = FALSE ), 
                         by = list(cut.POSIXt(DT.1_hour$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
  setnames(DT.1_hour, old = "cut.POSIXt", new = "timeStamp")
  DT.1_hour[, timeStamp := as.POSIXct(timeStamp)]
  assign(paste0(i, ".hour"), DT.1_hour)
}

# import CDPHE Powell data, its already been averaged to 1 hour

library(readxl)
Powell_PM_Mar_April_2018 <- as.data.table(read_excel("data-raw/Powell Particulates Mar - April 2018/Powell PM Mar-April 2018.xlsx", 
                                                     col_types = c("date", "numeric", "numeric")))
Powell_PM_Mar_April_2018[, timeStamp := timeStamp + 3600 ] # add one hour to correct for DST which starts 12th March 2018
Powell_PM_Mar_April_2018[, timeStamp := timeStamp + 3600 * 7 ] # add 7 hour to correct for DST which starts 12th March 2018
Powell_PM_Mar_April_2018 <- Powell_PM_Mar_April_2018[timeStamp > "2018-03-31 18:00:00"]
startTime <- format(CCA_Powell_A_Primary[1,1], usetz = FALSE)
endTime <- format(CCA_Powell_A_Primary[nrow(CCA_Powell_A_Primary),1], usetz = FALSE)

#merge the CDPHE and PA data averaged to 1 hour 
listOfNames <- c(paste0(PAlist, ".hour"), "Powell_PM_Mar_April_2018")
dtList <- mget(listOfNames)
mergedDT <- Reduce(function(...) merge(..., all = TRUE, by = "timeStamp"), dtList)
mergedDT <- mergedDT[timeStamp %between% c(startTime, endTime) ] 
# mergedDT[, `:=` (pm1p0Delta_pa = pm1p0a_ug.m3_pa - pm1p0b_ug.m3_pa,
#                  pm2p5Delta_pa = pm2p5a_ug.m3_pa - pm2p5b_ug.m3_pa,
#                  pm10p0Delta_pa = pm10p0a_ug.m3_pa - pm10p0b_ug.m3_pa,
#                  pm1p0Delta_egg = pm1p0a_compVal_ug.m3_egg - pm1p0b_compVal_ug.m3_egg,
#                  pm2p5Delta_egg = pm2p5a_compVal_ug.m3_egg - pm2p5b_compVal_ug.m3_egg,
#                  pm2p5Delta_eggRev = - pm2p5a_compVal_ug.m3_egg + pm2p5b_compVal_ug.m3_egg,
#                  pm10p0Delta_egg = pm10p0a_compVal_ug.m3_egg - pm10p0b_compVal_ug.m3_egg
#                  )]

mergedDT[,pm25p0Delta_pa := pm2p5a_ug.m3_pa - pmfine]
# now get ready for graphing
mergedDT.long <- data.table::melt(
  mergedDT,
  id.vars = c("timeStamp")
)
mergedDT.long[, variable := as.character(variable)]

# temperatureVars <- c("temperature_degF_pa", "temperature_rawVal_degF_egg")
# rhVars <-          c("humidity_percent_pa", "humidity_rawVal_percent_egg")
# pmVars <- c("pm1p0a_ug.m3_pa", "pm2p5a_ug.m3_pa", "pm10p0a_ug.m3_pa", 
#             "pm1p0b_ug.m3_pa",  "pm2p5b_ug.m3_pa", "pm10p0b_ug.m3_pa",
#             "pm1p0a_compVal_ug.m3_egg", "pm2p5a_compVal_ug.m3_egg", "pm10p0a_compVal_ug.m3_egg",
#             "pm1p0b_compVal_ug.m3_egg", "pm2p5b_compVal_ug.m3_egg", "pm10p0b_compVal_ug.m3_egg")
# pm1Vars <-   c("pm1p0a_ug.m3_pa", "pm1p0b_ug.m3_pa", "pm1p0a_compVal_ug.m3_egg", "pm1p0b_compVal_ug.m3_egg")
# pm2.5Vars <- c("pm2p5a_ug.m3_pa", "pm2p5b_ug.m3_pa", "pm2p5a_compVal_ug.m3_egg", "pm2p5b_compVal_ug.m3_egg")
# pm10Vars <-  c("pm1p0a_ug.m3_pa", "pm1p0b_ug.m3_pa", "pm10p0a_compVal_ug.m3_egg", "pm10p0b_compVal_ug.m3_egg")
# pm1Delta <- c("pm1p0Delta_pa", "pm1p0Delta_egg")
# pm2.5Delta <- c("pm2p5Delta_pa",  "pm2p5Delta_eggRev") #"pm2p5Delta_egg",
# pm10Delta <- c("pm10p0Delta_pa", "pm10p0Delta_egg")

pm2.5Vars <- c("pm2p5a_ug.m3_pa", "pm2p5b_ug.m3_pa","pmfine")
pm10Vars <-  c("pm10p0a_ug.m3_pa", "pm10p0b_ug.m3_pa", "pm10ltp")

plots.pm <- list()
# plots
for (i in c("pm2.5Vars",  "pm10Vars")) local({
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
  if (i %in% "pm10p0Delta_pa") {
    gasYLabel <- "(µg/"~m^3~")"
    gasinTitle <- "PM 10 B  and CDPHE Sensor Differences"
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
    
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m %d %y") +
  
    geom_line()
#     print(ggplotly(p, tooltip = c("timeStamp", i), dynamicTicks = TRUE))
  print(p)
  plots.pm[[i]] <<- p
})
multiplot(plotlist = plots.pm, cols = 2)


