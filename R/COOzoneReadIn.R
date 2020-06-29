#Read in ozone and other data from the Colorado air quality website
library(tidyverse)
library(data.table)
#Set up date
year <- 2019
month <- 9
startDay <- 28
endDay <- 30

# this code is designed to read in the ozone data from a particular location (Palisade) where the CDPHE has a monitor
# But there are also commented out statements that could form the basis for reading in other data

#station ID aqsid = 080770020 is Palisade ozone; aqsid = 080770017 is Powell bldg particulates aqsid = 080770018 is Pitkin shelter CO
#Note that Powell just has 3 particulate measures - PM 2.5, PM 10, and coarse particulates
#Palisade has O3, wind direction, wind speed, relative humidity
#Pitkin has CO, wind direction, wind speed, relative humidity
# create an empty data.table to hold the data
final <- data.table(DateTime = as.POSIXct(character()), HR = character(),O3 = numeric(),TEMP = numeric(),WD = numeric(),WS = numeric())

#the pattern in the html page variable called thepage to look for
mypattern = '\t<td align=\"left\" class=\"cell\">([^<]*)</td>'

#this loop reads in the html pages from the CDPHE website one day at a time
#It pulls out the hourly data and writes it to the final data.table
for (d in startDay:endDay) {
  my.day <- paste(year,month,d,sep = "-")
  seeddate <- paste(month, "%2f", d, "%2f", year, sep = "")
  # assemble the url from which the data will come. If you post the url generated in COOzoneURL to a website it should display the complete web page
  COOzoneURL <- paste('https://www.colorado.gov/airquality/site.aspx?aqsid=080770020&seeddate=',seeddate, sep = "")
  #  COParticulatesURL <- paste('https://www.colorado.gov/airquality/site.aspx?aqsid = 080770017&seeddate = ',seeddate, sep = "")
  
  #read in the html page for the date d
  thepage = readLines(COOzoneURL)
  # throw out the first 135 lines of the web page. They don't hold the data
  datalines = grep(mypattern,thepage[136:length(thepage)],value = TRUE)
  getexpr = function(s,g)substring(s, g,g + attr(g,'match.length') - 1)
  gg = gregexpr(mypattern,datalines)
  matches = mapply(getexpr,datalines,gg)
  result = gsub(mypattern,'\\1',matches)
  # convert the result character variable into a data table. ncol is the number of columns which might vary for other data sources
  # basically write the first five elements into the first row, the next 5 elements into the second row ... 
  hourlyData = as.data.table(matrix(result,ncol = 5,byrow = TRUE), stringsAsFactors = FALSE) 
  # give the columns more useful names
  setnames(hourlyData, old = names(hourlyData), new = c('HR','O3','TEMP','WD','WS')) # for ozone  
  # convert the variables that are numeric to a numeric format
  convertToNumeric <- c("O3","TEMP", "WD", "WS")
  hourlyData <- hourlyData[,(convertToNumeric) := lapply(.SD, as.numeric), .SDcols = convertToNumeric]
  
  #next line is to move midnight to the right day. May not need it with %H instead of %I
  # hourlyData[HR = = "12:00 AM"] <- "11:59 PM"
  hourlyData <- hourlyData[HR %in% "12:00 AM", HR := "11:59 PM"]
  hourlyData[, HR := paste(my.day, HR)]
  # next line converts the time data to a dateTime format. This might be a problem with
  #CDPHE data because the data are ALWAYS in mountain standard time. 
  hourlyData[,DateTime := as.POSIXct(HR, 
                                     format = "%Y-%m-%d %I:%M %p", 
                                     tz = "America/Denver")]
  # <- as.POSIXlt(tt$time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")
  final <- rbind(final,hourlyData)
}
final[,HR := NULL]

#good explanation of strptime codes at http://php.net/manual/en/function.strftime.php
#final$DateTime <- strptime(final$DateTime, format = "%Y-%m-%d %H:%M %p", tz = "America/Denver")

plot(O3 ~ DateTime, data = final, type = "l", xaxt = "n", ylab = "ppb", xlab = "", main = "Ozone, Palisade Water Treatment Plant")
tck <- axis(1, labels = FALSE)
labels <- format(as.POSIXct(tck, origin = "1970-01-01"), format = "%b %d %H:00" )
text(tck, par("usr")[3], labels = labels, srt = 315,
     xpd = TRUE, adj = c(-0.2,1.2), cex = 0.9)

ticks <- seq(from = min(final$DateTime), by = '2 hours', length = 25)
# axis(side = 1, outer = TRUE, at = ticks, labels = lbl)
axis.POSIXct(1, final$DateTime, format = "%m/%d %H:%M",  at = ticks, cex.axis = 0.7, srt = 45)
plot(final$DateTime, final$WD, type = "l")
plot(final$DateTime, final$WS, type = "l")
plot(final$DateTime, final$Temp, type = "l")
#get hourly means
h.mean <- aggregate(final$Temp, list(DATE = format(final$Time, "%Y-%m-%d %I:%M %p")), mean, na.rm = T)
h.mean$DATE <- as.Date(h.mean$DATE)


