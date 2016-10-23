library(curl)
library(jsonlite)
library(dplyr)
library(data.table)
library(urltools)

myCurlHandle = new_handle()
myKey <- paste("api-key","cb9287bb-352b-4af8-8d93-2201d49f9dc4", sep = " ")
handle_setheaders(myCurlHandle,
                  "Accept" = "application/json",
                  "Authorization" = myKey)
ptm <- proc.time()
URLbegin <- "https://api.opensensors.io"
eggSerial <- "egg00802aaa019b0111" # NO2/O3
#eggSerial <- "egg008028730d880112" # particulates
startDate <- "2016-10-18"
startTime <- "17:30" #GMT
endDate <- "2016-10-23"
endTime <- "17:30" #GMT
myURL <- paste0(URLbegin, "/v1/messages/device/",eggSerial, "?start-date=",startDate,"T",url_encode(startTime), "Z&",
                "end-date=",endDate,"T",url_encode(endTime), "Z")

urlDownload <- function(myURL) {
  done <- FALSE
  dt <- data.table(device = character(), owner = character(), topic = character(), date = character(), serial_number = character(),
                   converted_value = numeric(), converted_units = character(), raw_value = numeric(),
                   raw_instant_value = numeric(), raw_units = character(),
                   sensor_part_number = character(), raw_value2 = numeric(), raw_instant_value2 = numeric(),
                   compensated_value = numeric())
  setnames(dt, old = names(dt), new = gsub("_", "-", names(dt)))

  while (done == FALSE) {
 #   curl_download(url = myURL, destfile = "curlDownloadTest.csv", mode = "w", handle = myCurlHandle)
    req <- curl_fetch_memory(myURL, handle = myCurlHandle)
    init <- fromJSON(rawToChar(req$content))
    temp <- as.data.table(bind_cols(
      select(init$messages, device, owner, topic, date),
      stream_in(textConnection(init$messages$payload$text), flatten = TRUE)))
    dt <- rbindlist(list(dt,temp), fill = TRUE)
    nextstream <- init[["next"]]
    
    if (!is.null(nextstream)) {
      myURL <- paste0(URLbegin, nextstream)
#      print(myURL)
    } else {
      print("Done")
      done <- TRUE
    }
  }
  return(dt)
}

dt <- urlDownload(myURL)
print(proc.time() - ptm)

write.csv(dt, file = paste0(eggSerial,".downloadTest.csv"))

