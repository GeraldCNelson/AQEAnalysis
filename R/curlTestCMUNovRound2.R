Sys.setenv(TZ = "America/Denver") # needed to get rid of a warning message
library(curl)
library(httr)
library(jsonlite)
library(dplyr)
library(data.table)
library(urltools)

#' create the jwt for the GET command; commented out because we can read a jwt from the local disk
# CurlbodyforJWT <- list(name = "nelson.gerald.c@gmail.com", password = "MGN1adn2")
# temp <- POST(url =  AQElogin, body = CurlbodyforJWT, encode = "json")
# jwt <- paste("Bearer ", content(temp)$jwt, sep = "")
# saveRDS(jwt, file = "data-raw/jwt.RDS")
jwt <- readRDS(file = "data-raw/jwt.RDS")

#' user input data
eggSerialList <- c("egg00802aaa019b0111", "egg0080270b448b0153", "egg008028730d880112", "egg008020c0d89b0153", "egg0080228ba6080140")
for (eggSerial in eggSerialList) {
  print(paste0("working on ", eggSerial))
  if (eggSerial %in% "egg00802aaa019b0111") sensorList <- c("no2","o3")
  if (eggSerial %in% "egg0080270b448b0153") sensorList <- c("no2","o3")
  if (eggSerial %in% "egg008028730d880112") sensorList <- c("particulate")
  if (eggSerial %in% "egg008020c0d89b0153") sensorList <- c("no2", "co")
  if (eggSerial %in% "egg0080228ba6080140") sensorList <- c("no2", "co")
  
  eggType <- capture.output(cat(sensorList, sep = ""))
  sensorList <- c(sensorList, "temperature", "humidity")
  serialtailCt <- 6 #number of characters from end of serial number to use in eggName
  serialTail <- substr(eggSerial, nchar(eggSerial) - serialtailCt, nchar(eggSerial))
  
  eggName <- paste(eggType, serialTail, sep = "_")
  
  #' Note: date and time information must use this format
  startDate <- "2017-11-16"
  startTime <- "16:00:00" 
  endDate <- "2017-11-18"
  endTime <- "14:00:00"
  
  startTime <- url_encode(startTime)
  endTime <- url_encode(endTime)
  
  #' variables constructed for httr commands
  startDateTime <- paste(startDate, "T", startTime, "Z", sep = "")
  endDateTime <- paste(endDate, "T", endTime, "Z", sep = "")
  dateRange <- paste("?start-date=", startDateTime, "&end-date=", endDateTime, sep = "")
  AQElogin <- "https://airqualityegg.wickeddevice.com/api/v1/login"
  AQEEggInfoURL <- paste("https://airqualityegg.wickeddevice.com/api/v1", "/messages/device/", eggSerial, dateRange, sep = "")
  
  # see this https://stackoverflow.com/questions/22668144/how-correctly-use-request-header-with-api-data-requests and
  # https://www.r-bloggers.com/using-the-httr-package-to-retrieve-data-from-apis-in-r/
  eggData <- GET(url = AQEEggInfoURL, add_headers(Authorization = jwt))
  eggData.content <- as.data.table(jsonlite::fromJSON(rawToChar(eggData$content), flatten = TRUE))   
  eggData.content[, topic := sub("/orgs/wd/aqe/", "", topic)]
  eggData.content[, topic := sub(paste0("/", eggSerial), "", topic)]
  eggData.content[, timeStamp := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%S", tz = "America/Denver")]
  eggData.content[, c("date", "payload.serial-number", "payload.firmware-version", "payload.publishes", "payload.counter", "payload.sensor-part-number") := NULL]
  setnames(eggData.content, old = names(eggData.content), new = gsub("payload.", "", names(eggData.content)))
  setnames(eggData.content, old = names(eggData.content), new = gsub("__location.", "", names(eggData.content)))
  
  payloadNames <- names(eggData.content)
  print("names of eggData.content")
  cat(names(eggData.content), sep = ", ")
  
  
  for (i in sensorList) {
    print(paste0("working on sensor ", i))
    DT <- eggData.content[topic %in% i,]
    convertedUnits <- as.character(unique(DT[,"converted-units"]))
    rawUnits <- as.character(unique(DT[,"raw-units"])) 
    # delete converted-value for sensors other than temperature and humidity
    if (!i %in% c("temperature", "humidity") ) {
      DT[, c("converted-value", "converted-units") := NULL]
      setnames(DT, old = c("compensated-value"), new = paste(c("compVal"), convertedUnits, sep = "_"))
    }else{
      setnames(DT, old = c("converted-value", "compensated-value"), new = paste(c("conVal", "compVal"), convertedUnits, sep = "_"))
    }
    if (eggSerial %in% c("egg00802aaa019b0111", "egg0080270b448b0153")) {
      setnames(DT, old = c("raw-value", "raw-instant-value",  "raw-value2", "raw-instant-value2"), 
               new = paste(c("rawVal", "rawInstVal",  "rawVal2", "rawInstVal2"), rawUnits, sep = "_"))
    }else{
      setnames(DT, old = c("raw-value", "raw-instant-value"), 
               new = paste(c("rawVal", "rawInstVal"), rawUnits, sep = "_"))
    }
    
    DT[, c("topic", "converted-units", "raw-units") := NULL]
    oldNamesToChange <- names(DT)[!names(DT) %in% "timeStamp"]
    setnames(DT, old = oldNamesToChange, new = paste(i, oldNamesToChange, sep = "_"))
    assign(i, DT)
  }
  
  # average to every x minutes
  # first average everything to 1 minute to get all eggs on the same time
  x <- "1"
  timeBreak <- paste0(x, " min")
  for (i in sensorList){
    DT <- eval(parse(text = i))
    varsToAve <-  names(DT)[!names(DT) %in% c("timeStamp", paste(i, "converted-units", sep = "."), 
                                              paste(i, "raw-units", sep = "."))]
    DT <- DT[, lapply(.SD, mean, na.rm = TRUE ), 
             by = list(cut.POSIXt(DT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
    setnames(DT, old = "cut.POSIXt", new = "timeStamp")
    DT[, timeStamp := as.POSIXct(timeStamp)]
    setkey(DT, timeStamp)
    assign(i, DT)
  }
  nvars <- length(sensorList)
  temp <- sensorList[1:(nvars - 2)]
  suffix <- "[temperature,][humidity,]"
  if (nvars - 2 == 1) {
    finalLayout <- paste(temp, suffix, sep = "")
    final <- eval(parse(text = finalLayout))
  }else if (nvars - 2 == 2) {
    temp[2] <- paste("[", temp[2], ",]", sep = "")
    finalLayout <- paste(temp[1], temp[2], suffix, collapse = "", sep = "")
    final <- eval(parse(text = finalLayout))
  }
  saveRDS(final, file = paste0("results/CMUNovRound22017", "_", eggName, ".RDS", sep = ""))
}

# old code
# myCurlHandle = new_handle()
# myKey <- paste("api-key","cb9287bb-352b-4af8-8d93-2201d49f9dc4", sep = " ")
# handle_setheaders(myCurlHandle,
#                   "Accept" = "application/json",
#                   "Authorization" = myKey)
# ptm <- proc.time()
# URLbegin <- "https://api.opensensors.io"
# myURL <- paste0(URLbegin, "/v1/messages/device/",eggSerial, "?start-date=",startDate,"T",url_encode(startTime), "Z&",
#                 "end-date=",endDate,"T",url_encode(endTime), "Z")
# 
# urlDownload <- function(myURL) {
#   done <- FALSE
#   dt <- data.table(device = character(), owner = character(), topic = character(), date = character(), serial_number = character(),
#                    converted_value = numeric(), converted_units = character(), raw_value = numeric(),
#                    raw_instant_value = numeric(), raw_units = character(),
#                    sensor_part_number = character(), raw_value2 = numeric(), raw_instant_value2 = numeric(),
#                    compensated_value = numeric())
#   setnames(dt, old = names(dt), new = gsub("_", "-", names(dt)))
#   
#   while (done == FALSE) {
#     #   curl_download(url = myURL, destfile = "curlDownloadTest.csv", mode = "w", handle = myCurlHandle)
#     req <- curl_fetch_memory(myURL, handle = myCurlHandle)
#     init <- fromJSON(rawToChar(req$content))
#     print(init)
#     temp <- as.data.table(bind_cols(
#       select(init$messages, device, owner, topic, date),
#       stream_in(textConnection(init$messages$payload$text), flatten = TRUE)))
#     dt <- rbindlist(list(dt,temp), fill = TRUE)
#     nextstream <- init[["next"]]
#     
#     if (!is.null(nextstream)) {
#       myURL <- paste0(URLbegin, nextstream)
#       #      print(myURL)
#     } else {
#       print("Done")
#       done <- TRUE
#     }
#   }
#   return(dt)
# }
# 