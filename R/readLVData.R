library(xts)
myFile <- c("~/Documents/workspace/data/AQEgg/LouisvilleDownload/Oct31Download.csv")
#first set up so new version of character can have format in colClasses; unfortunately
#this methodology gives some random NAs for reasons I can't figure out
setClass('myDate')
setAs("character","myDate", function(from) as.POSIXlt(from, format = '%m/%d/%y %H:%M') )
# 
LvData<-read.table(myFile, skip = 1, sep = ",", header = FALSE,
                    colClasses = c("myDate","numeric",
                                   "myDate","numeric",
                                   "myDate","numeric",
                                   "myDate","numeric",
                                   "myDate","numeric"))
Egg.O3 <- as.data.frame(LvData[1:2])
names(Egg.O3) = (c("DateTime","O3"))
Egg.NO2 <- as.data.frame(LvData[3:4])
names(Egg.NO2) = (c("DateTime","NO2"))
Egg.Hum <- as.data.frame(LvData[5:6])
names(Egg.Hum) = (c("DateTime","Hum"))
Egg.TEMP <- as.data.frame(LvData[7:8])
names(Egg.TEMP) = (c("DateTime","TEMP"))
Egg.Dust <- as.data.frame(LvData[9:10])
names(Egg.Dust) = (c("DateTime","Dust"))

egg.O3.hourly <- aggregate(list(ozone = Egg.O3$O3), 
                  list(DateTime = cut(Egg.O3$DateTime, "1 hour")), 
                  mean)
egg.O3.hourly$DateTime <- as.POSIXlt(egg.O3.hourly$DateTime)
plot(egg.O3.hourly$DateTime,egg.O3.hourly$ozone)
lines(final$O3,col="red")

temp <- subset(Egg.O3, 
               Egg.O3$DateTime >= as.POSIXlt("2014-10-01 00:00:00") & 
                 Egg.O3$DateTime <= as.POSIXlt("2014-10-15 00:00:00"))
temp2 <- subset(final, final$DateTime >= as.POSIXlt("2014-10-01 00:00:00") & 
                  final$DateTime <= as.POSIXlt("2014-10-15 00:00:00"))
temp2$DateTime<-as.POSIXlt(temp2$DateTime)




