# Sys.setenv(TZ = "America/Denver") # needed to get rid of a warning message. Maybe not now.
library(curl)
library(httr)
library(jsonlite)
library(dplyr)
library(urltools)

#' create the jwt for the GET command; commented out because we can read a jwt from the local disk
# CurlbodyforJWT <- list(name = "nelson.gerald.c@gmail.com", password = "MGN1adn2")
# temp <- POST(url =  AQElogin, body = CurlbodyforJWT, encode = "json")
# jwt <- paste("Bearer ", content(temp)$jwt, sep = "")
# saveRDS(jwt, file = "data-raw/jwt.RDS")
jwt <- readRDS(file = "data-raw/jwt.RDS")
AQElogin <- "https://airqualityegg.wickeddevice.com/api/v1/login"

sensors <- function(eggSerial) {
  if (eggSerial %in% c("egg00802aaa019b0111", "egg0080270b448b0153")) sensorList <- c("no2","o3")
  if (eggSerial %in% c("egg00802e63038b0122", "egg00802d56461b0133", "egg00802e31aba80123", 
                       "egg00802e6306880122", "egg00802e6305980122")) sensorList <- c("particulate", "co2", "pressure")
  if (eggSerial %in% c("egg008020c0d89b0153", "egg0080228ba6080140")) sensorList <- c("no2", "co")
  if (eggSerial %in% c("egg008028730d880112")) sensorList <- c("particulate")
  return(sensorList)
}

dataDownload <- function(startDate, startTime, endDate, endTime, eggSerial, eggFileName, tz) {
  print(paste("working on egg ", eggSerial, sep = ""))
  sensorList <- sensors(eggSerial)
  eggType <- paste(sensorList, collapse = "")
  #serialtailCt <- 6 #number of characters from end of serial number to use in eggName
  # serialTail <- substr(eggSerial, nchar(eggSerial) - serialtailCt, nchar(eggSerial))
  
  # eggName <- paste(eggType, serialTail, sep = "_")
  
  # egg data on the mqtt server are in GMT, need to convert start date and time to GMT when requesting the data
  timeStamp.start <- paste(startDate, startTime, sep = " ")
  timeStamp.end <- paste(endDate, endTime, sep = " ")
  timeStamp.start.px <- as.POSIXct(timeStamp.start, tz = tz, usetz = TRUE)
  timeStamp.end.px <- as.POSIXct(timeStamp.end, tz = tz, usetz = TRUE)
  
  # convert time display to UTC
  attr(timeStamp.start.px, "tzone") <- "UTC" 
  attr(timeStamp.end.px, "tzone") <- "UTC" 
  
  #' variables constructed for httr commands
  startDateTime <- url_encode(format(timeStamp.start.px, format = "%Y-%m-%dT%H:%M:%SZ"))
  endDateTime <- url_encode(format(timeStamp.end.px, format = "%Y-%m-%dT%H:%M:%SZ"))
  dateRange <- paste("?start-date=", startDateTime, "&end-date=", endDateTime, sep = "")
  AQEEggInfoURL <- paste("https://airqualityegg.wickeddevice.com/api/v1", "/messages/device/", eggSerial, dateRange, sep = "")
  
  # see this https://stackoverflow.com/questions/22668144/how-correctly-use-request-header-with-api-data-requests and
  # https://www.r-bloggers.com/using-the-httr-package-to-retrieve-data-from-apis-in-r/
  eggData <- GET(url = AQEEggInfoURL, add_headers(Authorization = jwt))
  eggData.content <- as.data.table(jsonlite::fromJSON(rawToChar(eggData$content), flatten = TRUE))   
  if (length(eggData.content) == 0) {
    missingDataMessage <- paste("egg ", eggSerial, " appears to have no data for the time period ", startDateTime, " to ", endDateTime)
    print(missingDataMessage)
    return("missingData")
  }else{
    eggData.content[, topic := sub("/orgs/wd/aqe/", "", topic)]
    eggData.content[, topic := sub(paste0("/", eggSerial), "", topic)]
    eggData.content[, timeStamp := as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")]
    eggData.content[, timeStamp := as.POSIXct(format(timeStamp, tz = tz, usetz = TRUE))]
    
    if ("payload.__location.lat" %in% names(eggData.content)) {
      lat <- unique(eggData.content[, payload.__location.lat])
      lat <- lat[!is.na(lat)]
    }else{lat <- NA}
    if ("payload.__location.lon" %in% names(eggData.content)) {
      lon <- unique(eggData.content[, payload.__location.lon])
      lon <- lon[!is.na(lon)]
    }else{lon <- NA}
    if ("payload.__location.alt" %in% names(eggData.content)) {
      alt <- unique(eggData.content[, payload.__location.alt])
      alt <- alt[!is.na(alt)]
    }else{alt <- NA}
    
    eggData.content[, c("date", "payload.serial-number", "payload.firmware-version", "payload.publishes", "payload.counter") := NULL]
    setnames(eggData.content, old = names(eggData.content), new = gsub("payload.", "", names(eggData.content)))
    setnames(eggData.content, old = names(eggData.content), new = gsub("__location.", "", names(eggData.content)))
    tempUnits <- as.character(unique(eggData.content[topic %in% "temperature", "converted-units"]))
    
    # payloadNames <- names(eggData.content)
    print("names of eggData.content")
    cat(names(eggData.content), sep = ", ")
    print(" ")
    
    #' work on "temperature" and "humidity". they are in all eggs
    for (i in c("temperature", "humidity") ) {
      print(paste0("   working on sensor ", i, " in ", eggSerial))
      DT <- eggData.content[topic %in% i,]
      convertedUnits <- as.character(unique(DT[,"converted-units"]))
      sensorPartNum <- unique(DT[,`sensor-part-number`])
      rawUnits <- as.character(unique(DT[,"raw-units"])) 
      keepListCol <- c("converted-value", "raw-value", "timeStamp")
      DT <- DT[, (keepListCol), with = FALSE]
      setnames(DT, old = c("raw-value"), new = paste(i, "rawVal", rawUnits, sep = "_"))
      setnames(DT, old = c("converted-value"), new = paste(i, "compVal", convertedUnits, sep = "_"))
      assign(i, DT)
      assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
    }
    
    #' work on topics other than humidity, temperature, co, and particulates. Note that the no2 sensors differ in no2co and no2o3 eggs.
    #' and that particulate sensors differ between the particulate egg type and the particulateco2pressure egg type.
    
    if (eggType %in% "particulateco2pressure") {
      for (i in sensorList) {
        print(paste0("   working on sensor ", i, " in ", eggSerial))
        DT <- eggData.content[topic %in% i,]
        sensorPartNum <- unique(DT[,`sensor-part-number`])
        convertedUnits <- as.character(unique(DT[,"converted-units"])) #same as units for compensated-value
        rawUnits <- as.character(unique(DT[,"raw-units"])) 
        
        if (i %in% "particulate") {
          # this egg has two particulate sensors a and b. Each collects readings on pm 1, pm 2.5, and pm 10. These are currently average to give "pm1p0", "pm2p5", "pm10p0"
          print(paste0("   working on sensor ", i, " in ", eggSerial))
          keepListCol <- c("pm1p0_a", "pm2p5_a", "pm10p0_a", "pm1p0_b", "pm2p5_b", "pm10p0_b", "pm1p0", "pm2p5", "pm10p0", "timeStamp")
          DT <- DT[, (keepListCol), with = FALSE]
          convertedUnits <- "ug.m3"
          oldNames <- c("pm1p0_a", "pm2p5_a", "pm10p0_a", "pm1p0_b", "pm2p5_b", "pm10p0_b", "pm1p0", "pm2p5", "pm10p0")
          newNames <- gsub("_", "", oldNames)
          setnames(DT, old = oldNames, 
                   new = paste(newNames, "compVal", convertedUnits, sep = "_"))
          assign(i, DT)
          assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
        }
        
        if (i %in% "co2") {
          keepListCol <- c("compensated-value", "timeStamp")
          DT <- DT[, (keepListCol), with = FALSE]
          setnames(DT, old = c("compensated-value") , new = paste("compVal", convertedUnits, sep = "_"))
          assign(i, DT)
          assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
        }
        if (i %in% "pressure") {
          convertedUnits <- as.character(unique(DT[,"pressure-units"])) #same as units for compensated-value
          keepListCol <- c("pressure", "timeStamp")
          DT <- DT[, (keepListCol), with = FALSE]
          setnames(DT, old = c("pressure") , new = paste("pressure", convertedUnits, sep = "_"))
          assign(i, DT)
          assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
        }
      }
    }
    
    if (eggType %in% "no2o3") {
      for (i in sensorList) {
        print(paste0("   working on sensor ", i, " in ", eggSerial))
        DT <- eggData.content[topic %in% i,]
        sensorPartNum <- unique(DT[,`sensor-part-number`])
        convertedUnits <- as.character(unique(DT[,"converted-units"])) #same as units for compensated-value
        rawUnits <- as.character(unique(DT[,"raw-units"])) 
        if (i %in% "no2") {
          keepListCol <- c("raw-value", "raw-value2", "compensated-value", "timeStamp")
          DT <- DT[, (keepListCol), with = FALSE]
          setnames(DT, old = "compensated-value", new = paste(i, "compVal", convertedUnits, sep = "_"))
          setnames(DT, old = c("raw-value"), 
                   new = paste(i, c("rawVal"), rawUnits, sep = "_"))
          setnames(DT, old = c("raw-value2"), 
                   new = paste(i, c( "rawValAux"), rawUnits, sep = "_"))
          assign(i, DT)
          assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
        }
        if (i %in% "o3") {
          keepListCol <- c("raw-value", "compensated-value", "timeStamp")
          DT <- DT[, (keepListCol), with = FALSE]
          setnames(DT, old = "compensated-value", new = paste(i, "compVal", convertedUnits, sep = "_"))
          setnames(DT, old = c("raw-value"), 
                   new = paste(i, c("rawVal"), rawUnits, sep = "_"))
          assign(i, DT)
          assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
        }
      }
    }
    
    if (eggType %in% "particulate") {
      for (i in sensorList) {
        print(paste0("   working on sensor ", i, " in ", eggSerial))
        DT <- eggData.content[topic %in% i,]
        sensorPartNum <- unique(DT[,`sensor-part-number`])
        convertedUnits <- as.character(unique(DT[,"converted-units"])) #same as units for compensated-value
        rawUnits <- as.character(unique(DT[,"raw-units"])) 
        if (i %in% "particulate") {
          keepListCol <- c("raw-value", "compensated-value", "timeStamp")
          DT <- DT[, (keepListCol), with = FALSE]
          convertedUnits <- "ug.m3"
          setnames(DT, old = "compensated-value", new = paste(i, "compVal", convertedUnits, sep = "_"))
          setnames(DT, old = c("raw-value"), 
                   new = paste(i, "rawVal", rawUnits, sep = "_"))
          assign(i, DT)
          assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
        }
      }
    }
    
    if (eggType %in% "no2co") {
      for (i in sensorList) {
        print(paste0("   working on sensor ", i, " in ", eggSerial))
        DT <- eggData.content[topic %in% i,]
        sensorPartNum <- unique(DT[,`sensor-part-number`])
        convertedUnits <- as.character(unique(DT[,"converted-units"])) #same as units for compensated-value
        rawUnits <- as.character(unique(DT[,"raw-units"])) 
        if (i %in% "no2") {
          keepListCol <- c("raw-value", "compensated-value", "timeStamp")
          DT <- DT[, (keepListCol), with = FALSE]
          setnames(DT, old = "compensated-value", new = paste(i, "compVal", convertedUnits, sep = "_"))
          setnames(DT, old = c("raw-value"), 
                   new = paste(i, c("rawVal"), rawUnits, sep = "_"))
          assign(i, DT)
          assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
        }
        if (i %in% "co") {
          keepListCol <- c("raw-value", "compensated-value", "timeStamp")
          DT <- DT[, (keepListCol), with = FALSE]
          setnames(DT, old = "compensated-value", new = paste(i, "compVal", convertedUnits, sep = "_"))
          setnames(DT, old = c("raw-value"), 
                   new = paste(i, c("rawVal"), rawUnits, sep = "_"))
          assign(i, DT)
          assign(paste("sensorPartNum", i, sep = "."), sensorPartNum)
        }
      }
    }
    
    # write a file with the sensor part number information and lat, long, altitude
    # number of sensors, including temp and rh
    sensorPartList <- paste("sensorPartNum", sensorList, sep = ".")
    sensorDT <- data.table(sensor = character(), sensorPartNum = character(), stringsAsFactors = FALSE)
    userInfoDT <- data.table(measure = character(), value = character(), stringsAsFactors = FALSE)
    for (i in 1:length(sensorPartList)) {
      sensorInfo <- c(sensorList[i], eval(parse(text = sensorPartList[i])))
      sensorDT <- rbindlist(list(sensorDT, as.list(sensorInfo)), use.names = TRUE)
    }
    userInfoDT <- rbind(userInfoDT, list("latitude", lat)) 
    userInfoDT <- rbind(userInfoDT, list("longitude", lon)) 
    userInfoDT <- rbind(userInfoDT, list("altitude", alt)) 
    userInfoDT <- rbind(userInfoDT, list("tempUnits", tempUnits)) 
    
    write.csv(sensorDT, file =  paste("data/sensorInfo_", eggName, ".csv", sep = ""), row.names = FALSE)
    write.csv(userInfoDT, file = paste("data/userInfo_", eggName, ".csv", sep = ""), row.names = FALSE)
    
    # average to every x minutes
    # first average everything to 1 minute to get all egg topics on the same time
    x <- "1"
    timeBreak <- paste0(x, " min")
    for (i in  c(sensorList, "humidity", "temperature")) {
      DT <- eval(parse(text = i)) # now read in all the topics (temp, humidity, etc and average to  1 minute)
      varsToAve <-  names(DT)[!names(DT) %in% c("timeStamp", paste(i, "converted-units", sep = "."), 
                                                paste(i, "raw-units", sep = "."))]
      DT <- DT[, lapply(.SD, mean, na.rm = TRUE ), 
               by = list(cut.POSIXt(DT$timeStamp, breaks = timeBreak)), .SDcols = varsToAve]
      setnames(DT, old = "cut.POSIXt", new = "timeStamp")
      DT[, timeStamp := as.POSIXct(timeStamp)]
      setkey(DT, timeStamp)
      # rename the DT back to the name of the sensor
      assign(i, DT)
    }
    
    sensorList <- c(sensorList, "humidity", "temperature")
    dtList <- mget(sensorList)
    mergedDT <- Reduce(function(...) merge(..., all = TRUE, by = "timeStamp"), dtList)
    
    #' add absolute humidity
    # ---- absolute humidity equation, from https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/ 
    #' in the formula below, temperature needs to be in degrees celsius. Since I've been saving it in degrees F, these need to be converted. Rather than have temperatures saved in different units
    #' I'm going to convert all temps to degC and then report in degF if that is what the user wants
    if (tempUnits %in% "degF") {
      mergedDT[, temperature_compVal_degC := (temperature_rawVal_degF - 32) * 5/9]
      mergedDT[, temperature_rawVal_degC := (temperature_compVal_degF - 32) * 5/9]
      deleteFunits <- c("temperature_rawVal_degF", "temperature_compVal_degF")
      mergedDT[, (deleteFunits) := NULL]
    }
    mergedDT[, ah_rawVal_g.m3  := (6.112 * exp((17.67 * temperature_rawVal_degC)/(temperature_rawVal_degC +  243.5)) * humidity_rawVal_percent *  2.1674)/(273.15 + temperature_rawVal_degC)]
    mergedDT[, ah_compVal_g.m3 := (6.112 * exp((17.67 * temperature_compVal_degC)/(temperature_compVal_degC +  243.5)) * humidity_compVal_percent *  2.1674)/(273.15 + temperature_compVal_degC)]
    #delete C values, if data in F degrees
    # if (tempUnits %in% "degF") {
    #   colsToDelete <- c("temp.raw_degC", "temp.comp_degC")
    #   mergedDT[, (colsToDelete) := NULL]
    # }
    
    saveRDS(mergedDT, file = eggFileName)
    return("Success")
  }
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


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct locationhea
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# copied here to potentially use for getting the appropriate userInfo file. Needs to take into account date range.
getNewestVersion <- function(fileShortName, directory, fileType) {
  if (missing(directory)) {mData <- fileloc("mData")} else {mData <- directory}
  if (missing(fileType)) {fileType <- "rds"}
  
  # see
  # http://stackoverflow.com/questions/7381641/regex-matching-beginning-and-end-strings
  # for an explanation of this regex expression
  # regExp <- paste("(?=^", fileShortName, ")(?=.*rawData$)", sep = "")
  #  regExp <- paste("(?=^", fileShortName, ")(?=.*", fileType, "$)", sep = "")
  # stringChar <- unlist(strsplit(list.files(mData)[1], ".", fixed = TRUE))
  # # this is still a potential problem. If the first file in the directory doesn't have the same date
  # dateOfFirst <- stringChar[length(stringChar) - 1]
  # tailLength <- 15 # to remove data and the period and csv or rds
  # if (fileType == "xlsx") tailLength <- 16 # for xlsx files
  # fillIn <- paste('.{', tailLength, '}$', sep = "")
  fileShortNameTest <- paste(fileShortName,"_2", sep = "") # this should get rid of the multiple files problem
  filesofFileType <- list.files(mData)[grep(fileType,list.files(mData))]
  fileLongName <- filesofFileType[grep(fileShortNameTest, filesofFileType, fixed = TRUE)]
  #  temp <- gsub(fillIn, "", list.files(mData))
  # filesList <-
  #   grep(regExp,
  #        list.files(mData),
  #        value = TRUE,
  #        perl = TRUE)
  # print(filesList)
  # newestFile <- filesList[length(filesList)]
  
  #  if (length(newestFile) == 0) {
  # check to see if the short file name is in the list from the relevant directory
  # print(paste("fileShortName is ", fileShortName))
  #  print(fileLongName)
  if (length(fileLongName) == 0) {
    stop(sprintf("There is no file  '%s' in directory %s", fileShortName, mData))
  } else {
    #   print(fileLongName)
    outFile = readRDS(paste(mData, fileLongName, sep = "/"))
    return(outFile)
  }
}
