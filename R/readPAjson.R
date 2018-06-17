#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
#' @title Import IMPACT data from a gdx file
#' @name readPAjson.R
#' @include nutrientModFunctions.R

#source("R/nutrientModFunctions.R")

library(data.table)
library(twilio)
library(anytime)
sourceFile <- "readPAjson.R"
library(jsonlite)
Sys.setenv(TWILIO_SID = "ACe27e0525a8ea129277c806913dc01d27")
Sys.setenv(TWILIO_TOKEN = "1787363e2fecfcac2091766621ec455c")

monitorList <- c("CCA Redlands", "CCA Alpine Bank Mesa Mall", "Palisade Buffer Zone", "CCA LaPaloma", "CCA South Peach Street", 
                 "Phillips", "CCA North Fruita Desert", "CCA Alpine Bank Fruita", "CCA Sunny Meadow", "CCA Alpine Bank Downtown GJ", 
                 "CCA Allergy and Asthma Center", "CCA Emily Drive", "Phillips", "CCA Laddie", "CCA 9 Road", "CCA POWELL", 
                 "CCA Quail Run", "CCA Alpine Bank Horizon Drive", "CCA Cross Orchards", "CCA Elberta Ave", 
                 "CCA Alpine Bank Clifton", "CCA 24 and J", "CCA Airport West", "CCA Alamar Stables", 
                 "CCA Kannah Creek", "CCA Northridge", "Downtown Palisade")
temp <- fromJSON("https://www.purpleair.com/json", flatten = TRUE)
padata <- as.data.table(temp$results)
padata <- padata[Label %in% monitorList]
numCols <- c("Lat", "Lon", "PM2_5Value", "temp_f", "humidity", "pressure")
padata[,(numCols):= lapply(.SD, as.numeric), .SDcols = numCols]
warningVal = 1
for (i in 1:nrow(padata)) {
  if (padata$PM2_5Value[i] > warningVal) {
    timeStamp <- anytime(padata$LastSeen[i])
    print(paste0(padata$Label[i], "PM2.5 AQI value of ", padata$PM2_5Value[i]," is greater than the warning value of ", warningVal, ". Time is ", anytime(padata$LastSeen[i])))
  }
}

KSCell <- "+19702421054"
TPcell <- "+19704249270"
BLcell <- "+14063814051"
To <- BLcell
From <- "+12177335065" # special twilio number
Body <- paste("A test. Purple Air monitor ", padata$Label[i], "has an AQI value of ", padata$PM2_5Value[i], ". Time is ", as.POSIXct(timeStamp))
tw_send_message(from = From, to = To, body = Body)

# PurpleAir JSON fields description and example values:
#   The following is a list of the fields and a description of their values contained in the JSON data:
#   "ID":1234, // PurpleAir sensor ID
# "ParentID":null, // The PurpleAir sensor ID of the "parent" entry in the case of Channel B
# "THINGSPEAK_PRIMARY_ID":"1234", // The Thingspeak channel ID for primary data of this sensor
# "THINGSPEAK_PRIMARY_ID_READ_KEY":"XXXX", // The Thingspeak read key for primary data of this sensor
# "Label":"name", // The "name" that appears on the map for this sensor
# "Lat":null, // Latitude position info
# "Lon":null, // Longitude position info
# "PM2_5Value":"1.07", // Current PM2.5 value (based on the 
#                                              "State":null,  // Unused variable
#                                              "Type":"TYPE",  // Sensor type (PMS5003, PMS1003, BME280 etc)
#                                              "Hidden":"true", // Hide from public view on map: true/false
#                                              "Flag":null, // Data flagged for unusually high readings
#                                              "DEVICE_BRIGHTNESS":"1", // LED brightness (if hardware is present)
#                                              "isOwner":1, // Currently logged in user is the sensor owner
#                                              "A_H":null, // true if the sensor output has been downgraded or marked for attention due to suspected hardware issues
#                                              "temp_f":"xx",  // Current temperature in F
#                                              "humidity":"xx", // Current humidity in %
#                                              "pressure":"xx", // Current pressure in Millibars
#                                              "AGE":29831, // Sensor data age (when data was last received) in minutes
#                                              "THINGSPEAK_SECONDARY_ID":"1234", // The Thingspeak channel ID for secondary data of this sensor
#                                              "THINGSPEAK_SECONDARY_ID_READ_KEY":"XXXX", // The Thingspeak read key for secondary data of this sensor
#                                              "LastSeen":1490309930, // Last seen data time stamp in UTC
#                                              "Version":"2.47c", // Current version of sensor firmware
#                                              "LastUpdateCheck":1490308331, // Last update checked at time stamp in UTC
#                                              "Uptime":"5210", // Sensor uptime in seconds
#                                              "RSSI":"-68", // Sensor's WiFi signal strength in dBm
#                                              
#                                              "Stats": // Statistics for PM2.5
#                                              "{
#                                              \"v\":1.07, // Real time or current PM2.5 Value
#                                              \"v1\":1.3988595758168765, // Short term (10 minute average)
#                                              \"v2\":10.938131480857114, // 30 minute average
#                                              \"v3\":15.028685608345926, // 1 hour average
#                                              \"v4\":6.290537580116773, // 6 hour average
#                                              \"v5\":1.8393146177050788, // 24 hour average
#                                              \"v6\":0.27522764912064507, // One week average
#                                              \"pm\":1.07, // Real time or current PM2.5 Value
#                                              \"lastModified\":1490309930933, // Last modified time stamp for calculated average statistics
#                                              \"timeSinceModified\":69290 // Time between last two readings in milliseconds
#                                              }"
# }

