setwd("~/Documents/workspace/AQEAnalysis")
library(rgdal)
library(spdep)
library(sp)
library(fields)
library(MBA)
library(maptools)
library(raster)
library(rgeos)
library(ggplot2)
library(chron)

#The Integrated Surface Database (ISD) consists of global hourly and synoptic 
#observations compiled from numerous sources into a single common ASCII format 
#and common data model. More info at https://www.ncdc.noaa.gov/isd Some better R
#code than what is below is at
#http://www.r-bloggers.com/accessing-cleaning-and-plotting-noaa-temperature-data/

#data(wrld_simpl)
file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
repeat {
  try(download.file(file, "data/ish-history.csv",
                    quiet = TRUE))
  
  if (file.info("data/ish-history.csv")$size > 0) {
    break
  }
}
st <- read.csv("data/ish-history.csv")
dim(st)
names(st)
names(st)[c(3, 9)] <- c("NAME", "ELEV")
#keep just US
st <- st[st$CTRY == "US", ]

#get into proper units
st$LAT <- st$LAT/1000
st$LON <- st$LON/1000
st$ELEV <- st$ELEV/10
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END <- as.numeric(substr(st$END, 1, 4))
dim(st)
sum(is.na(st$BEGIN))
sum(is.na(st$END))
sum(st$STATE == "")
co.list <- st[st$STATE == "CO" & (st$BEGIN <= 2013 &
                                    st$END >= 2013 & !is.na(st$BEGIN)), ]
dim(co.list)


outputs <- as.data.frame(matrix(NA, dim(co.list)[1],2))
names(outputs) <- c("FILE", "STATUS")

#note: need to install wget if using this on a mac
for (y in 2013:2013) {
  #get the names of the data files for the state
  y.co.list <- co.list[co.list$BEGIN <= y & co.list$END >= y, ]
  for (s in 1:dim(y.co.list)[1])
  {
    outputs[s, 1] <- paste(sprintf("%06d", y.co.list[s,
                                                     1]), "-", sprintf("%05d", y.co.list[s, 
                                                                                         2]), "-", y, ".gz", sep = "")
    #put the downloaded files into data/raw/
    wget <- paste("/usr/local/bin/wget -P data/raw ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",
                  y, "/", outputs[s, 1], sep = "")
    outputs[s, 2] <- try(system(wget, intern = FALSE, ignore.stderr = TRUE))
  }
}
head(outputs)
#if file is downloaded it's status is 0; if not downloaded it is 256
sum(outputs$STATUS == 256)
sum(outputs$STATUS == 0)

system("gunzip -r data/raw", intern = FALSE, ignore.stderr = TRUE)
# detailed description of data at ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf

files <- list.files("data/raw")
column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6,
                   1, 1, 1, 5, 1, 5, 1, 5, 1)
stations <- as.data.frame(matrix(NA, length(files),6))
names(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", "ELEV")

#need to create data/csv before running this next for loop
for (i in 1:length(files)) 
{data <- read.fwf(paste("data/raw/", files[i], sep = ""), column.widths)
 data <- data[, c(2:8, 10:11, 13, 16, 19, 29, 31, 33)]
 names(data) <- c("USAFID", "WBAN", "YR", "M",
                  "D", "HR", "MIN", "LAT", "LONG", "ELEV",
                  "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT",
                  "ATM.PRES")
 data$LAT <- data$LAT/1000
 data$LONG <- data$LONG/1000
 data$WIND.SPD <- data$WIND.SPD/10
 data$TEMP <- data$TEMP/10
 data$DEW.POINT <- data$DEW.POINT/10
 data$ATM.PRES <- data$ATM.PRES/10
 write.csv(data, file = paste("data/csv/", files[i], ".csv", sep = ""), row.names = FALSE)
 stations[i, 1:3] <- data[1, 1:3]
 stations[i, 4:6] <- data[1, 8:10]
}

write.csv(stations, file = "data/stations.csv", row.names = FALSE)
co.state <- readOGR("data/gis/colorado/.", layer = "co_state_bndy_gcdb_poly", verbose = FALSE)
spTransform(co.state,CRS("+proj=longlat +datum=WGS84")) 
co <- co.state[co.state$AREA > 0.01 ]
plot(co, xlab = "Degrees", ylab = "Degrees", ylim = "35", axes = T)
points(stations$LONG, stations$LAT, pch = 16, col = "red")

files <- list.files("data/csv")
#Note that the 90 needs to be replaced by the number of csv files
st <- read.csv(file = paste("data/csv/",  files[90], sep = ""))
head(st)

#count missing values
sum(st$TEMP == 999.9)
sum(st$WIND.SPD == 999.9)
sum(st$WIND.DIR == 999)
sum(st$DEW.POINT == 999.9)
sum(st$ATM.PRES == 9999.9)

#set the missing values to NA
st$TEMP[st$TEMP == 999.9] <- NA
st$WIND.SPD[st$WIND.SPD == 999.9] <- NA
st <- st[st$MIN == 0, ]
st$WIND.DIR <- st$DEW.POINT <- st$ATM.PRES <- NULL
dim(st)

st <- st[order(st$M, st$D, st$HR), ]
st$DATE <- as.Date(paste(st$YR, st$M, st$D, st$HR, sep = "-"), format = "%Y-%m-%d-%h")

#plot temp
#not sure I have the hourly calcs right
d.mean <- aggregate(st$TEMP, list(DATE = format(st$DATE, "%Y-%m-%d")), mean, na.rm = T)
m.mean <- aggregate(st$TEMP, list(DATE = format(st$DATE, "%Y-%m")), mean, na.rm = T)
h.mean <- aggregate(st$TEMP, list(DATE = format(st$DATE, "%Y-%m-%d %I:%M %p")), mean, na.rm = T)
d.mean$DATE <- as.Date(d.mean$DATE)
m.mean$DATE <- as.Date(paste(m.mean$DATE, "-15",sep = ""))
h.mean$DATE<- as.Date(h.mean$DATE)
plot(st$DATE, st$TEMP, main = "Temperature readings",
     ylab = "Temperature (Degrees C)", xlab = "Month",
     col = "grey")
points(d.mean$DATE, d.mean$x, col = "brown")
lines(m.mean$DATE, m.mean$x, type = "b", pch = 16)
legend("topleft", c("Hourly", "Daily mean", "Monthly mean"),
       inset = 0.02, pch = c(1, 1, 16), col = c("grey", "red", "black"))

#plot wind speed
d.mean <- aggregate(st$WIND.SPD, list(DATE = format(st$DATE, "%Y-%m-%d")), mean, na.rm = T)
m.mean <- aggregate(st$WIND.SPD, list(DATE = format(st$DATE, "%Y-%m")), mean, na.rm = T)
d.mean$DATE <- as.Date(d.mean$DATE)
m.mean$DATE <- as.Date(paste(m.mean$DATE, "-15", sep = ""))
plot(st$DATE, st$WIND.SPD, main = "Wind speed readings",
       ylab = "Wind Speed (meters/second)", xlab = "Month",
       col = "grey")
points(d.mean$DATE, d.mean$x, col = "brown")

chart.4 <- read.csv("~/Downloads/chart-4.csv")
PalisadeData <- read.table("~/Downloads/chart-4.csv", header = TRUE, sep = ",", colClasses = 
                             "POSIXct,numeric,POSIXct,numeric"
)
chart.4$DateTime2<- strptime(chart.4$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "MST")

