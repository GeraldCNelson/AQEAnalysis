# orginal code from http://jasonneylon.wordpress.com/2013/05/08/plotting-air-quality-egg-data-using-r/

#install.packages('RCurl')
#install.packages("ggplot2")
library(ggplot2)
library(RCurl)

#How to get an api_key to use to download the egg data
#1. Create an account at xively.com.
#2. Log in and click on Settings
#3. Click on Master Keys
#4. Click on Add Master Key
#5. Click on Add Key
# Xively MyAQEggKey
# api_keyAQE1 <- '7DDhHL8dP9DQuVwH9579kKMP1JKOL77WOdUhm9LGCvI8jIOD'
# api_keyAQE2a0742 <- "obAnN6IkH2UULibvldsylfq5s5NQhaf4Qze3DT2gvWCXwNEL"

api_key <- "7DDhHL8dP9DQuVwH9579kKMP1JKOL77WOdUhm9LGCvI8jIOD"

# your feed id
# feed_id_AQE1 = '58540724'
# feed_id_AQE2a0742 = '58540724
# feed_id_AQE2a1716 = 109431716
#feed_id_AQE2a0742 = '109431716'
AQE2aEggs <- c("9544854","109431716","2122830268", "546125879")
# your location, find it at http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones
localTimeZone = "America/Denver"

start_date = as.POSIXct("2015-07-12 09:00","GMT")
end_date = as.POSIXct("2015-07-18 09:00","GMT")

#for AQE V1
#data stream choices and units
# CO_00-04-a3-d9-2b-3a_10  ppb
# CO_r0_00-04-a3-d9-2b-3a_1868876  ohms
# CO_raw_00-04-a3-d9-2b-3a_1803348	ohms
# Dust_00-04-a3-d6-9d-d4_00	pcs/283ml
# Dust_r0_00-04-a3-d6-9d-d4_0100	ohms
# Dust_raw_00-04-a3-d6-9d-d4_00	ohms
# Humidity_00-04-a3-d6-54-02_1	%
# NO2_00-04-a3-d9-2b-3a_026	ppb
# NO2_r0_00-04-a3-d9-2b-3a_03892	ohms
# NO2_raw_00-04-a3-d9-2b-3a_022397	ohms
# O3_00-04-a3-d6-62-9e_06	ppb
# O3_r0_00-04-a3-d6-62-9e_0316000	ohms
# O3_raw_00-04-a3-d6-62-9e_012281	ohms
# Temperature_00-04-a3-d6-54-02_0	deg C

dataStreamsV1 <- c("CO_00-04-a3-d9-2b-3a_1",
                 "Dust_00-04-a3-d6-9d-d4_0",
                 "Humidity_00-04-a3-d6-54-02_1",
                 "NO2_00-04-a3-d9-2b-3a_0", 
                 "O3_00-04-a3-d6-62-9e_0",
                 "Temperature_00-04-a3-d6-54-02_0")
dataStreamsUnitsV1 <- c("ppb",
                      "pcs/283ml",
                      "%",
                      "ppb",
                      "ppb",
                      "deg C")
dataStreamsShortNamesV1 <- c("CO",
                           "Dust",
                           "Humidity",
                           "NO2",
                           "O3",
                           "Temp")


#for AQE V2
dataStreamsV2 <- c("CO", "CO_raw", "Counter", "Heartbeat", "Humidity", 
                  "Internal_Humidity", "Internal_Temperature" , "NO2", "NO2_raw", "Temperature")
dataStreamsUnitsV2 <- c("unit_symbol", "ppm", "microVolts"," N/A", "N/A", "percent", "percent", 
                        "degC", "ppb", "microVolts", "degC")

#assign dataStreamsV1 for an AQE V1 to dataStreams; dataStreamsV2 for an AQE V2.
dataStreams <- c("CO","NO2","Humidity","Temperature")

streamCounter <- 1
for (feed_id in (AQE2aEggs))
  for (v in dataStreams){
    dfRaw <- data.frame(timeISO= as.Date(character()), value = as.numeric())
    start <- start_date
    while (start < end_date) {
      #Note: xively uses an ISO 8601 time stamp format format. See https://en.wikipedia.org/wiki/ISO_8601
      start_date_as_str = format(start, format="%Y-%m-%dT%H:%M:00Z")
      next_date = start + (2 * 60 * 60)
      next_date_as_str = format(next_date, format="%Y-%m-%dT%H:%M:00Z")
      #See below for a description of how the url is created
      xively_url = paste("https://api.xively.com/v2/feeds/", feed_id, "/datastreams/", v, 
                         ".csv?start=", start_date_as_str, "&end=", next_date_as_str,"&interval=0&limit=1000", 
                         sep="")    
      
      csv_data_for_period <- read.csv(
        text = getURL(xively_url, httpheader = c('X-ApiKey' = api_key)), 
        header = FALSE,
        stringsAsFactors = FALSE,
        #         colClasses = c("character","numeric"),
        col.names = c("timeISO", "value")
      )
      Sys.sleep(2) # pause for 2 seconds so the xively website doesn't complain
      dfRaw = rbind(dfRaw, csv_data_for_period)
      start = next_date
    }
    #clean up time
    dfRaw$timeGMT <- as.POSIXct(dfRaw$timeISO, "%FT%T", tz = "GMT")
    # convert GMT time to the local time zone
    #  tmp$temp = as.POSIXct(tmp$timestamp,"GMT")
    # dfRaw$timeLocal = as.POSIXct(format(dfRaw$timeGMT, tz=localTimeZone, usetz=TRUE))
    if (substr(v,1,4) == "Temp" ) {
      #convert C degrees to F degrees and add a column if v is the Temperature datastream
      dfRaw$valueF <- dfRaw$value*(9/5)+32
      dfMin$valueF <- dfMin$value*(9/5)+32
    }
    
    #aggregate data to the minute
    dfMin <- aggregate(dfRaw["value"],
                       by=list(cut(dfRaw$timeGMT,breaks="min")),
                       FUN=mean)
    names(dfMin)[names(dfMin)=="Group.1"] <- "timeGMT"
    dfMin$timeGMT <- as.POSIXct(dfMin$timeGMT, tz = "GMT")
    dfMin$timeLocal = as.POSIXct(format(dfMin$timeGMT, tz=localTimeZone, usetz=TRUE))
    
    assign(paste("dfRaw", feed_id, v, sep = '_'), dfRaw)
    assign(paste("dfMin", feed_id, v, sep = '_'), dfMin)
  }

# graphing stuff .
timeStart <- dfMin_109431716_NO2$timeGMT[1]
timeEnd <- dfMin_109431716_NO2$timeGMT[nrow(dfMin_109431716_NO2)]
rect1 <- data.frame (xmin=timeStart, xmax=timeEnd, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=timeStart, xmax=timeEnd, ymin=-Inf, ymax=Inf)

#plot function for the measurements. Gas can be NO2, CO, Temperature and Humidity
eggList <- AQE2aEggs
egg2comparison <- function(eggList,gas) {
  dfList <- c(
    paste("dfMin",eggList[1],gas,sep="_"),
    paste("dfMin",eggList[2],gas,sep="_"),
    paste("dfMin",eggList[3],gas,sep="_"),
    paste("dfMin",eggList[4],gas,sep="_"))
  p<- ggplot()
  p <- p +  geom_line(data=as.data.frame(eval(parse(text = (dfList[1])))), 
                      aes(x = timeLocal, y = value, color=paste("egg",dfList[1],sep="")))
  p <- p +  geom_line(data=as.data.frame(eval(parse(text = (dfList[2])))), 
                      aes(x = timeLocal, y = value, color=paste("egg",dfList[2],sep="")))
  p <- p +  geom_line(data=as.data.frame(eval(parse(text = (dfList[3])))), 
                      aes(x = timeLocal, y = value, color=paste("egg",dfList[3],sep="")))
  p <- p +  geom_line(data=as.data.frame(eval(parse(text = (dfList[4])))), 
                      aes(x = timeLocal, y = value, color=paste("egg",dfList[4],sep="")))
  p <- p + scale_colour_manual("", 
                               breaks = c(
                                 paste("egg",dfList[1],sep=""),
                                 paste("egg",dfList[2],sep=""), 
                                 paste("egg",dfList[3],sep=""), 
                                 paste("egg",dfList[4],sep="")
                               ),
                               values = c("red", "green", "blue", "yellow"))
  p <- p +  labs(x="Time", y=gas)
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
  p <- p +  scale_y_continuous()
  #p <- p + geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")
  #p <- p + geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")
  #p <- p + theme(plot.margin = unit(c(0.5,0.5,0,0.5), "lines"))
  p <- p + theme(legend.position="bottom")
  print(p)
}

#calculate some differences. Use first egg in AQE2aEggs as the base

base <- AQE2aEggs[1]
gas <- "NO2"
eggBase <- paste("dfMin",base,gas,sep="_")
egg2 <- paste("dfMin",AQE2aEggs[2],gas,sep="_") 
egg3 <- paste("dfMin",AQE2aEggs[3],gas,sep="_") 
egg4 <- paste("dfMin",AQE2aEggs[4],gas,sep="_") 

for (k in 2:4) {
  eggMerge <- merge(eval(parse(text = paste("dfMin",AQE2aEggs[1],gas,sep="_"))), 
                    eval(parse(text = paste("dfMin",AQE2aEggs[k],gas,sep="_") )), 
                    by ="timeLocal")
  eggDiff <- eggMerge$value.y - eggMerge$value.x
  #plot(eggMerge$timeLocal,eggDiff)
  p<- ggplot(data=eggMerge)
  p <- p + geom_line(aes(x = timeLocal, y = eggDiff))  
  p <- p  +  labs(
    (list(title = paste("Difference in", gas, "between egg", 
                        AQE2aEggs[k], "and", AQE2aEggs[1]),
          x = "Local time",
          y = bquote('Difference in'~.(gas)))
    ))
 # print(p)
  print(summary(lm(eggMerge$value.y ~ eggMerge$value.x)))
}



egg2comparison(eggList,"NO2")
egg2comparison(eggList,"CO")

lm(eggMerge$value.y ~ eggMerge$value.x)

#Creating the download url
#Xively limits the amount of data you can download at one time. Here are the values and limits for 'interval'
#Interval Value  Description	Maximum range in One Query
#0	Every datapoint stored	6 hours
#30	One datapoint every 30 seconds	12 hours
#60	One datapoint every minute	24 hours
#300	One datapoint every 5 minutes	5 days
#900	One datapoint every 15 minutes	14 days
#1800	One datapoint per 30 minutes	31 days
#3600	One datapoint per hour	31 days

#maximium of 1000 data points per call
#source: https://xively.com/dev/docs/api/quick_reference/historical_data/
