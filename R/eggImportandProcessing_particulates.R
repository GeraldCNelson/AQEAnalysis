#The code below assumes that the data are in the format derived from the Serial print statement. Make sure the names in this statement
# and the col.names variables are consistent
#Serial.println(F("time, humid(%), temp(degC), no2_rs(kohms), no2_est(ppb), co_rs(kohms), co_est(ppb), o3_rs(kohms), o3_est(ppb), particulates(pcs/283mL)"));
library(plyr)
library(ggplot2)
library(car)
library(openxlsx)
library(censReg)

#userName is reported in the basic info worksheet
userName <- "Gerald Nelson"
dataCollectionDate <- "2014_12_15"
dataCollectionLocation <- "Powell Building, CO"
setwd("~/Documents/workspace/airQualityCompare")
#first read in egg data and massage it
eggDataFileName <- "EggData2014_12_15_130939.csv"
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
aggMin <- aggregate(tt[c("rh","temp","co_rs","dust")],
                 by=list(cut(tt$time,breaks="min")),
                 FUN=mean)
names(aggMin)[names(aggMin)=="Group.1"] <- "time"
aggMin$time <- as.POSIXct(aggMin$time)
aggHr <- aggregate(tt[c("rh","temp","co_rs","dust")],
                    by=list(cut(tt$time,breaks="hour")),
                    FUN=mean)
names(aggHr)[names(aggHr)=="Group.1"] <- "time"
aggHr$time <- as.POSIXct(aggHr$time)
#drop last row in case it is incomplete
aggMin <- aggMin[-(nrow(aggMin)),]
timeStart <- min(aggMin$time)
timeEnd <- max(aggMin$time)

#now import state data  from GJ Shelter and Powell site
#note that the outliers removed were for the CO monitor
stateDataFileName <- "CDPHEdataDec15outliersRemoved.csv"
CDPHE.tt <- read.csv(paste(getwd(),"/data/",stateDataFileName,sep=""), 
               skip=2, header = FALSE,
               stringsAsFactors=FALSE, 
               sep = ",",
               col.names = c("time","GJShelt.CO","GJShelt.temp","GJShelt.WD","GJShelt.WS", 
                             "PAL.O3", "PAL.temp", "PAL.WD", "PAL.WS", "Powell.pm10", "Powell.pm25"),
               colClasses = c("character",rep("numeric",10)))
CDPHE.tt<- subset(CDPHE.tt,select = -c(PAL.O3,PAL.temp,PAL.WD,PAL.WS))

CDPHE.tt$time <- as.POSIXct(CDPHE.tt$time, format="%m/%d/%Y %I:%M %p", tz = "America/Denver")

# GJShelter has temp and humidity. Reports temp in farenheit. Next line converts to celsius
CDPHE.tt$GJShelt.temp <- (CDPHE.tt$GJShelt.temp - 32) * 5/9
combined <- merge(aggMin,CDPHE.tt, by="time")
#delete first 15 minutes of data to deal with warm up issues
combined <- combined[-(1:15),]
#remove last row, in case it has NAs
combined <- combined[-nrow(combined),]
combined <- na.omit(combined)

lmOut <- function(res, file="test.csv", ndigit=3, writecsv=T) {
  #function to write regression results in a csv style
  # If summary has not been run on the model then run summary
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  co <- res$coefficients
  nvar <- nrow(co)
  ncoll <- ncol(co)
  f <- res$fstatistic
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  # This sets the number of rows before we start recording the coefficients
  nstats <- 4
  # G matrix stores data for output
  G <- matrix("", nrow=nvar+nstats, ncol=ncoll+1)
  G[1,1] <- toString(res$call)
  # Save rownames and colnames
  G[(nstats+1):(nvar+nstats),1] <- rownames(co)
  G[nstats, 2:(ncoll+1)] <- colnames(co)
  # Save Coefficients
  G[(nstats+1):(nvar+nstats), 2:(ncoll+1)] <- formatter(co)
  # Save F-stat
  G[1,2] <- paste0("F(",f[2],",",f[3],")")
  G[2,2] <- formatter(f[1])
  # Save F-p value
  G[1,3] <- "Prob > P"
  G[2,3] <- formatter(1-pf(f[1],f[2],f[3]))
  # Save R2
  G[1,4] <- "R-Squared"
  G[2,4] <- formatter(res$r.squared)
  # Save Adj-R2
  G[1,5] <- "Adj-R2"
  G[2,5] <- formatter(res$adj.r.squared)
  print(G)
  if (writecsv) write.csv(G, file=file, row.names=F)
  return(G)
}

#do work on particulates
fit <- lm(combined$Powell.pm25 ~ combined$dust + I(combined$dust^2) + I(combined$dust^3) + 
            combined$temp + combined$rh, data=combined)
summary(fit)
par(mfrow=c(1,1),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(fit)
plot(combined$time,combined$Powell.pm10,xlab = "time", ylab="PM10, Powell Building (µg/m3)")
plot(combined$time,combined$Powell.pm25,xlab = "time", ylab="PM2.5, Powell Building (µg/m3)")
plot(combined$time,combined$temp,xlab = "time", ylab="egg temp (C)")
plot(combined$time,combined$dust,xlab = "time", ylab="egg particulates ()")
plot(combined$time,combined$temp,xlab = "time", ylab="egg temp (C)")
plot(combined$time,combined$rh,xlab = "time", ylab="egg relative humidity (%)")
plot(fit$residuals,xlab = "time", ylab="regression residuals")

# adjust for outliers using Cook's distance. Rule is to throw out observations where cooks.distance is greater
# than 4/nrow (see http://en.wikipedia.org/wiki/Cook%27s_distance#cite_note-3p)
par(mfrow=c(1,1),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(cooks.distance(fit))
cutoff <- 4/fit$df.residual
abline(h=cutoff, lty=2, col=c("orange", "red"))
cooksDistance <- cooks.distance(fit)
cooksDistance <- as.data.frame(cooksDistance)
combined$cooksDistance <- cooksDistance$cooksDistance
combined.reduced <- subset(combined, cooksDistance < cutoff)
fit.reduced <- lm(combined.reduced$Powell.pm25 ~ combined.reduced$dust + I(combined.reduced$dust^2) + 
              I(combined.reduced$dust^3) + combined.reduced$temp + combined.reduced$rh, data=combined.reduced)
summary(fit)
summary(fit.reduced)

wb <- createWorkbook()

#Set up the lists to be used document all the worksheets
sheetNameList <- ("Sheet names")
sheetNameDesc <- ("Description of sheet contents")

#create a worksheet with info on creator, date, model version, etc.
creationInfo <- ("Information on creator, date, model version, etc.")
creationInfo <- rbind(creationInfo, paste("Creator:", userName))
creationInfo <- rbind(creationInfo, paste("Date of file creation:", Sys.time()))
creationInfo <- rbind(creationInfo, paste("Date data collected:", dataCollectionDate))
creationInfo <- rbind(creationInfo, paste("Location data collected:", dataCollectionLocation))
creationInfo <- rbind(creationInfo, paste("Regression formula:", Reduce(paste,deparse(formula(fit)))))
addWorksheet(wb, sheetName="creationInfo")
writeData(wb, creationInfo, sheet="creationInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
sheetNameList <- rbind(sheetNameList,"creationInfo")
sheetNameDesc <- rbind(sheetNameDesc,"Information on creator, date, model version, etc.")

temp.fit <- lmOut(fit)

addWorksheet(wb, sheetName="uncorrected pm25")
sheetNameList <- rbind(sheetNameList,"uncorrected pm25")
sheetNameDesc <- rbind(sheetNameDesc,"Particulate results uncorrected for outliers")
writeData(wb, temp.fit, sheet="uncorrected pm25", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)

#create worksheet for outlier corrected results
temp.fit.reduced <- lmOut(fit.reduced)
addWorksheet(wb, sheetName="Corrected pm25")
sheetNameList <- rbind(sheetNameList,"Corrected pm25")
sheetNameDesc <- rbind(sheetNameDesc,"Pm25 results corrected for outliers using Cooks Distance")
writeData(wb, temp.fit.reduced, sheet="Corrected pm25", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
resultsType <- "Dust"
OutFileName <- paste("results/",resultsType,".Results.",dataCollectionDate,".xlsx",sep="")

saveWorkbook(wb, OutFileName, overwrite = TRUE)
