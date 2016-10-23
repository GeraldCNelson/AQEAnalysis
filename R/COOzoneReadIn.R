#Read in ozone and other data from the Colorado airquality website
#Set up seed date
month <- 9
startDay <- 8
year <- 2016
endDay <- 10
#setup final data frame
final <- data.frame(
  (DateTime = character()),
  (HR = character()),
  (O3 = numeric()),
  (TEMP = numeric()),
  (WD = numeric()),
  (WS = numeric()),
  row.names = NULL)
#station ID agsid = 080770020 is Palisade ozone; aqsid = 080770017 is Powell bldg particulates aqsid = 080770018 is Pitkin shelter CO
#Note that Powell just has 3 particulate measures - PM 2.5, PM 10, and coarse particulates
#Palisade has O3, 
#Pitkin has CO, wind direction, wind speed, relative humidity, 
final <- data.table(DateTime = as.POSIXct(character()), HR = character(),O3 = numeric(),TEMP = numeric(),WD = numeric(),WS = numeric())
for (d in startDay:endDay) {
  my.day <-paste(year,month,d,sep = "-")
  seeddate <- paste(month, "%2f", d, "%2f", year, sep = "")
  COOzoneURL <- paste('http://www.colorado.gov/airquality/site.aspx?aqsid=080770020&seeddate=',seeddate, sep="")
  COParticulatesURL <- paste('http://www.colorado.gov/airquality/site.aspx?aqsid=080770017&seeddate=',seeddate, sep="")
  #read in ozone data
  thepage = readLines(COOzoneURL)
  mypattern = '\t<td align=\"left\" class=\"cell\">([^<]*)</td>'
  datalines = grep(mypattern,thepage[136:length(thepage)],value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,datalines)
  matches = mapply(getexpr,datalines,gg)
  result = gsub(mypattern,'\\1',matches)
  result[1:10]
  hourlyData = as.data.table(matrix(result,ncol = 5,byrow = TRUE), stringsAsFactors = FALSE) 
  setnames(hourlyData, old = names(hourlyData), new = c('HR','O3','TEMP','WD','WS')) # for ozone  
  convertToNumeric <- c("O3","TEMP", "WD", "WS")
  hourlyData <- hourlyData[,(convertToNumeric) := lapply(.SD, as.numeric), .SDcols = convertToNumeric]
 
  #next line is to move midnight to the right day. May not need it with %H instead of %I
 # hourlyData[HR == "12:00 AM"] <- "11:59 PM"
  hourlyData <- hourlyData[HR == "12:00 AM", HR := "11:59 PM"][, HR := paste(my.day,HR)]
  hourlyData[,DateTime := as.POSIXct(HR, 
                                    format = "%Y-%m-%d %I:%M %p", 
                                    tz = "America/Denver")]
  # <- as.POSIXlt(tt$time, format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver")
 # hourlyData <- hourlyData[2:6]
#  print(hourlyData)
  final <- rbind(final,hourlyData)
}
final[,HR := NULL]

#good explanation of strptime codes at http://php.net/manual/en/function.strftime.php
#final$DateTime <- strptime(final$DateTime, format = "%Y-%m-%d %H:%M %p", tz = "America/Denver")

plot(O3 ~ DateTime, data = final, type = "l", xaxt = "n", ylab = "ppb", xlab = "", main = "Ozone, Palisade Water Treatment Plant")
tck <- axis(1, labels=FALSE)
labels <- format(as.POSIXct(tck, origin="1970-01-01"), format = "%b %d %H:00" )
text(tck, par("usr")[3], labels=labels, srt=315,
     xpd=TRUE, adj=c(-0.2,1.2), cex=0.9)

ticks <- seq(from = min(final$DateTime), by = '2 hours', length = 25)
# axis(side=1, outer=TRUE, at=ticks, labels=lbl)
axis.POSIXct(1, final$DateTime, format = "%m/%d %H:%M",  at=ticks, cex.axis = 0.7, srt= 45)
plot(final$DateTime, final$WD, type = "l")
plot(final$DateTime, final$WS, type = "l")
plot(final$DateTime, final$Temp, type = "l")
#get hourly means
h.mean <- aggregate(final$Temp, list(DATE = format(final$Time, "%Y-%m-%d %I:%M %p")), mean, na.rm = T)
h.mean$DATE <- as.Date(h.mean$DATE)


