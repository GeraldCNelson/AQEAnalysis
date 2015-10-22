#The code below assumes that the data are in the format derived from the Serial print statement. Make sure the names in this statement
# and the col.names variables are consistent
#Serial.println(F("time, humid(%), temp(degC), no2_rs(kohms), no2_est(ppb), co_rs(kohms), co_est(ppb), o3_rs(kohms), o3_est(ppb), particulates(pcs/283mL)"));
library(plyr)
library(ggplot2)
library(car)
setwd("~/Documents/workspace/AQEAnalysis")
#first read in egg data and massage it
aqev2a <- "AQEV2A_Pal_151011_151012.txt"
aqev2b <- "AQEV2B_Pal_151011_151012.txt"
aqev2c <- "AQEV2C_Pal_151011_151012.txt"
cdphe_pal <- "CDPHE_Palisade_10112015_10122015.txt"

df.cdphe_pal <- read.csv(paste(getwd(),"/data/csv/",cdphe_pal,sep=""), 
                      skip = 1,header = FALSE,
                      stringsAsFactors=FALSE, 
                      sep = ",",
                      col.names = c("time","ws","wd","temp.cdphe","O3ppm"),
                      colClasses = c("character",rep("numeric",4))
)
#drop temp because it only has observations every 1/2 hour. That has been fixed for future data collection there.
df.cdphe_pal <- df.cdphe_pal[,c("time","ws","wd","O3ppm")]

df.aqev2a <- read.csv(paste(getwd(),"/data/csv/",aqev2a,sep=""), 
               skip = 1,header = FALSE,
               stringsAsFactors=FALSE, 
               sep = ",",
               col.names = c("time","temp.v2a","rh.v2a","no2","co","no2_v","co_v"),
               colClasses = c("character",rep("numeric",6))
               )
df.aqev2b <- read.csv(paste(getwd(),"/data/csv/",aqev2b,sep=""), 
                      skip = 1,header = FALSE,
                      stringsAsFactors=FALSE, 
                      sep = ",",
                      col.names = c("time","temp.v2b","rh.v2b","so2","o3","so2_v","o3_v"),
                      colClasses = c("character",rep("numeric",6))
)

df.aqev2c <- read.csv(paste(getwd(),"/data/csv/",aqev2c,sep=""), 
                      skip = 1,header = FALSE,
                      stringsAsFactors=FALSE, 
                      sep = ",",
                      col.names = c("time","temp.v2c","rh.v2c","pm","pm_v","lat","long","altitude"),
                      colClasses = c("character",rep("numeric",4),rep("character",2))
)
#drop long, lat, and altitude from df.aqev2c
df.aqev2c <- df.aqev2c[,c("time","temp.v2c","rh.v2c","pm","pm_v")]

#tt$time <- tt$time + 12*60*60 #- in case tt$time is in 12 hour clock
# Note: Time choices for breaks are "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year"
# For Date objects, only interval specifications using "day", "week", "month", "quarter" and "year" are allowed.)

df.aqev2a$time <- as.POSIXct(df.aqev2a$time, format="%m/%d/%Y %H:%M:%S", tz = "America/Denver")
df.aqev2b$time <- as.POSIXct(df.aqev2b$time, format="%m/%d/%Y %H:%M:%S", tz = "America/Denver")
df.aqev2c$time <- as.POSIXct(df.aqev2c$time, format="%m/%d/%Y %H:%M:%S", tz = "America/Denver")
df.cdphe_pal$time <- as.POSIXct(df.cdphe_pal$time, format="%d-%b-%Y %H:%M", tz = "America/Denver")

df.aqev2a.min <- aggregate(df.aqev2a[c("temp.v2a","rh.v2a","no2","co","no2_v","co_v")],
                           by=list(cut(df.aqev2a$time,breaks="min")),
                           FUN=mean)
names(df.aqev2a.min)[names(df.aqev2a.min)=="Group.1"] <- "time"
df.aqev2a.min$time <- as.POSIXct(df.aqev2a.min$time)

df.aqev2b.min <- aggregate(df.aqev2b[c("temp.v2b","rh.v2b","so2","o3","so2_v","o3_v")],
                           by=list(cut(df.aqev2b$time,breaks="min")),
                           FUN=mean)
names(df.aqev2b.min)[names(df.aqev2b.min)=="Group.1"] <- "time"
df.aqev2b.min$time <- as.POSIXct(df.aqev2b.min$time)

df.aqev2c.min <- aggregate(df.aqev2c[c("temp.v2c","rh.v2c","pm","pm_v")],
                           by=list(cut(df.aqev2c$time,breaks="min")),
                           FUN=mean)
names(df.aqev2c.min)[names(df.aqev2c.min)=="Group.1"] <- "time"
df.aqev2c.min$time <- as.POSIXct(df.aqev2c.min$time)

#join all the dfs
df.merged <- Reduce(function(...) merge(..., by = "time", all=TRUE), list(df.aqev2a.min, df.aqev2b.min, df.aqev2c.min, df.cdphe_pal))

#Drop rows where temp.v2a is na
df.joined <- df.merged[complete.cases(df.merged$temp.v2a),]

#drop first fifteen minutes to account for warmup
df.joined <- df.joined[16:dim(df.joined)[1],]
#Do regressions for O3
#convert state O3 in ppm to O3 in ppb to make it comparable with egg data
df.joined$O3ppb <- df.joined$O3ppm *1000

o3_v.baseline <- -0.823185825
no2_v.baseline <-  -0.828167629

#create a delta of the _v variables
df.joined$o3_v.delta <- (df.joined$o3_v - o3_v.baseline) * 1000
df.joined$no2_v.delta <- df.joined$no2_v - no2_v.baseline * 1000

df.joined <- df.joined[complete.cases(df.joined),]

df.joined$o3_temp <- df.joined$o3_v.delta/df.joined$temp.v2b

fit <- lm(df.joined$O3ppb ~ df.joined$o3_temp + df.joined$o3_v.delta + I(df.joined$o3_v.delta^2) + 
            df.joined$no2_v.delta + 
            df.joined$temp.v2b + df.joined$rh.v2b, data=df.joined)
summary(fit)
par(mfrow=c(2,2),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(fit)
plot(df.joined$time,df.joined$O3ppb,xlab = "time", ylab="O3, Palisade (ppb)")
plot(df.joined$time,df.joined$o3_v.delta,xlab = "time", ylab="O3_v, Palisade (v)")
plot(df.joined$time,df.joined$temp.v2b,xlab = "time", ylab="egg V2b temp (C)")
plot(df.joined$time,df.joined$rh.v2b,xlab = "time", ylab="egg V2b relative humidity (%)")
#plot(fit$residuals,xlab = "time", ylab="regression residuals")

# adjust for outliers using Cook's distance. Rule is to throw out observations where cooks.distance is greater
# than 4/nrow (see http://en.wikipedia.org/wiki/Cook%27s_distance#cite_note-3p)
# Note that the Cooks cutoff value is sometimes 4/(n-k) where k is number of independent variables
plot(cooks.distance(fit))
cutoff <- 4/fit$df.residual
abline(h=cutoff, lty=2, col=c("orange", "red"))
cooksDistance <- cooks.distance(fit)
cooksDistance <- as.data.frame(cooksDistance)
df.joined$cooksDistance <- cooksDistance$cooksDistance
df.joined.combined <- subset(df.joined, cooksDistance < cutoff)

fit <- lm(df.joined.combined$O3ppb ~ df.joined.combined$o3_v.delta + I(df.joined.combined$o3_v.delta^2) + df.joined.combined$no2_v + 
            df.joined.combined$temp.v2b + df.joined.combined$rh.v2b, data=df.joined.combined)
summary(fit)
par(mfrow=c(2,2),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(fit)
plot(df.joined$time,df.joined.combined$O3ppb,xlab = "time", ylab="O3, Palisade (ppb)")
plot(df.joined$time,df.joined.combined$o3_v.delta,xlab = "time", ylab="O3_v, Palisade (v)")
plot(df.joined$time,df.joined.combined$temp.v2b,xlab = "time", ylab="egg V2b temp (C)")
plot(df.joined$time,df.joined.combined$rh.v2b,xlab = "time", ylab="egg V2b relative humidity (%)")

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
