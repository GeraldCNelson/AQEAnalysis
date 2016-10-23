#The code below assumes that the data are in the format derived from the Serial print statement. Make sure the names in this statement
# and the col.names variables are consistent
#Serial.println(F("time, humid(%), temp(degC), no2_rs(kohms), no2_est(ppb), co_rs(kohms), co_est(ppb), o3_rs(kohms), o3_est(ppb), particulates(pcs/283mL)"));
library(plyr)
library(ggplot2)
library(car)
setwd("~/Documents/workspace/airQualityCompare")
#first read in egg data and massage it
eggDataFileName <- "EggData2015_01_12_100220.csv"
tt <- read.csv(paste(getwd(),"/data/",eggDataFileName,sep=""), 
               skip=51, header = FALSE,
               stringsAsFactors=FALSE, 
               sep = ",",
               col.names = c("time","rh","temp","no2_rs","no2_est","co_rs","co_est","o3_rs","o3_est","dust"),
               colClasses = c("character",rep("numeric",9))
               )
tt$time <- as.POSIXct(tt$time, format="%m/%d/%Y %H:%M:%S", tz = "America/Denver")
tt$time <- tt$time + 12*60*60 #- in case tt$time is in 12 hour clock
tt<- subset(tt,select = -c(no2_est,co_est,o3_est))
# Note: Time choices for breaks are "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year"
# For Date objects, only interval specifications using "day", "week", "month", "quarter" and "year" are allowed.)
aggMin <- aggregate(tt[c("rh","temp","no2_rs","co_rs","o3_rs","dust")],
                 by=list(cut(tt$time,breaks="min")),
                 FUN=mean)
names(aggMin)[names(aggMin)=="Group.1"] <- "time"
aggMin$time <- as.POSIXct(aggMin$time)
aggHr <- aggregate(tt[c("rh","temp","no2_rs","co_rs","o3_rs","dust")],
                    by=list(cut(tt$time,breaks="hour")),
                    FUN=mean)
names(aggHr)[names(aggHr)=="Group.1"] <- "time"
aggHr$time <- as.POSIXct(aggHr$time)

#now import state data for ozone from Palisade site
#stateDataFileName <- "Palisade_O3_WS_WD_01122015.csv"
#The removed outliers are around midnight when they inject some ozone for calibration purposes
stateDataFileName <- "Palisade_O3_WS_WD_01122015OutliersRemoved.csv"
CDPHE.tt <- read.csv(paste(getwd(),"/data/",stateDataFileName,sep=""), 
               skip=1, header = FALSE,
               stringsAsFactors=FALSE, 
               sep = ",",
               col.names = c("time","Pal.O3","Pal.WD","Pal.WS"),
               colClasses = c("character",rep("numeric",3)))
# for GJshelter, use the following
#col.names = c("time","GJShelt.CO","GJShelt.temp","GJShelt.WD","GJShelt.WS"),
#colClasses = c("character",rep("numeric",4))

CDPHE.tt$time <- as.POSIXct(CDPHE.tt$time, format="%m/%d/%Y %I:%M %p", tz = "America/Denver")

# GJShelter has temp and humidity. Reports temp in farenheit. Next line converts to celsius
#CDPHE.tt$GJShelt.temp <- (CDPHE.tt$GJShelt.temp - 32) * 5/9
combined <- merge(aggMin,CDPHE.tt, by="time")
#delete first 15 minutes of data to deal with warm up issues
combined <- combined[-(1:15),]

#do regressions for CO
fit <- lm(combined$GJShelt.CO ~ combined$co_rs + I(combined$co_rs^2) + I(combined$co_rs^3) + 
            combined$temp * combined$rh, data=combined)
summary(fit)
par(mfrow=c(2,2),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(fit)
plot(combined$time,combined$GJShelt.CO,xlab = "time", ylab="CO, GJ Shelter (ppm)")
plot(combined$time,combined$temp,xlab = "time", ylab="egg temp (C)")
plot(combined$time,combined$rh,xlab = "time", ylab="egg relative humidity (%)")
plot(fit$residuals,xlab = "time", ylab="regression residuals")

#do regressions for O3
fit <- lm(combined$Pal.O3 ~ combined$o3_rs + I(combined$o3_rs^2) + I(combined$o3_rs^3) + 
            combined$temp + combined$rh, data=combined)
summary(fit)
par(mfrow=c(2,2),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(fit)
plot(combined$time,combined$GJShelt.CO,xlab = "time", ylab="CO, GJ Shelter (ppm)")
plot(combined$time,combined$temp,xlab = "time", ylab="egg temp (C)")
plot(combined$time,combined$rh,xlab = "time", ylab="egg relative humidity (%)")
plot(fit$residuals,xlab = "time", ylab="regression residuals")
# adjust for outliers using Cook's distance. Rule is to throw out observations where cooks.distance is greater
# than 4/nrow (see http://en.wikipedia.org/wiki/Cook%27s_distance#cite_note-3p)
# Note that the Cooks cutoff value is sometimes 4/(n-k) where k is number of independent variables
plot(cooks.distance(fit))
cutoff <- 4/fit$df.residual
abline(h=cutoff, lty=2, col=c("orange", "red"))
cooksDistance <- cooks.distance(fit)
cooksDistance <- as.data.frame(cooksDistance)
combined$cooksDistance <- cooksDistance$cooksDistance
combined <- subset(combined, cooksDistance < cutoff)
#do work on particulates
fit <- lm(combined$GJShelt.CO ~ combined$co_rs + I(combined$co_rs^2) + I(combined$co_rs^3) + 
            combined$temp * combined$rh, data=combined)
summary(fit)
par(mfrow=c(2,2),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(fit)
plot(combined$time,combined$Powell.pm10,xlab = "time", ylab="PM10, Powell Building (µg/m3)")
plot(combined$time,combined$Powell.pm25,xlab = "time", ylab="PM2.5, Powell Building (µg/m3)")
plot(combined$time,combined$temp,xlab = "time", ylab="egg temp (C)")
plot(combined$time,combined$dust,xlab = "time", ylab="egg particulates ()")

#---------testing stuff from http://connectmv.com/tutorials/r-tutorial/investigating-outliers-discrepancies-and-other-influential-points/

par(mfrow=c(1,1),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(cooks.distance(fit))
cutoff <- 4/fit$df.residual
abline(h=cutoff, lty=2, col=c("orange", "red"))
#use identify to click on the graph to identify the points
#identify(cooks.distance(fit.rebuild))
# Let the function auto-identify the outliers; tell it which labels to use
influencePlot(fit.rebuild, id.method="noteworthy", labels=row.names(combined))
#find the row numbers to delete
cooksDistance <- cooks.distance(fit)
cooksDistance <- as.data.frame(cooksDistance)
cooksMax <- subset(cooksDistance, cooks.distance(fit) > cutoff)
cooksMin <- subset(cooksDistance, cooks.distance(fit) < cutoff)
remove = as.numeric(row.names(cooksMin))
fit.rebuild <- lm(fit, subset=remove)
summary(fit)
summary(fit.rebuild)
