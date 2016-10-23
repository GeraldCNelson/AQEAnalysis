#scriptHolder calls all the needed libraries and loads functions
source(file = "R/scriptHolder.R")
#It contains
#agPlot - do a 6 figure plot of data from the egg. It currently displays temp, rh, O3, CO, NO2_rs, particulates
#lmOut - prepares regression results in a csv format and also writes a csv file if that option is chosen

#userName is reported in the basic info worksheet
userName <- "Gerald Nelson"
# dataCollectionLocation <- "Palisade, CO" #choices are "Grand Junction, CO"
dataCollectionLocation <- "Grand Junction, CO" #choices are "Grand Junction, CO"
dataCollectionLocationAbrev  <- "GJC"
# dataCollectionDate <- "2015_01_12 - 2015_01_13"
# dataCollectionDate <- "2015_03_23 - 2015_03_24"

#fileNames
#The removed outliers from the state data are around midnight when some ozone is 
#injected for calibration purposes
#stateDataFileName <- "Palisade_O3_WS_WD_01122015OutliersRemoved.csv"
stateDataFileName <- "Palisade_O3_WS_WD_03232015OutliersRemoved.csv"
#eggDataFileName <- "EggData2015_01_12_100220.csv"
#eggDataFileName <- "EggData2015_03_23_131853.csv"

lookupFileName <- "lookupTables.xlsx"

#Read in the original lookup table for gas
gas <- "O3" # choices are NO2, co, O3, dust (worksheet nums are 2,3,4,5)
R0 <- data.frame(
  gas = c("NO2","CO","O3","dust"),
  val = c(3892,868876,316000,360))
R0.gas <- R0$val[match(gas,R0$gas)]
lookup <- read.xlsx(paste("data/",lookupFileName,sep=""),sheet = gas, startRow = 1, colNames = FALSE)
colnames(lookup) <- c("table_row","row_value_1","row_value_2","RsRatio","conc")
lookup <- lookup[,-(1:3)]
eggParams <- extractEggParams(eggDataFileName)
                           
#Read in the egg data
tt <- read.csv(paste(getwd(),"/data/csv/",eggDataFileName,sep=""), 
               skip=64, header = FALSE,
               stringsAsFactors=FALSE, 
               sep = ",",
               col.names = c("time","rh","temp","NO2_rs","NO2_est","CO_rs","CO_est","O3_rs","O3_est","dust_est"),
               colClasses = c("character",rep("numeric",9))
               )
tt[sapply(tt,is.na)] = 0
tt$time <- as.POSIXct(tt$time, format="%m/%d/%Y %H:%M:%S", tz = "America/Denver")
#tt$time <- tt$time + 12*60*60 #- in case tt$time is in 12 hour clock
# Note: Time choices for breaks are "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year"
# For Date objects, only interval specifications using "day", "week", "month", "quarter" and "year" are allowed.)
#drop last row in case it is incomplete
tt <- tt[-(nrow(tt)),]

aggMin <- aggregate(tt[c("rh","temp","NO2_rs","NO2_est","CO_rs","CO_est","O3_rs","O3_est","dust_est")],
                 by=list(cut(tt$time,breaks="min")),
                 FUN=mean)
names(aggMin)[names(aggMin)=="Group.1"] <- "time"
aggMin$time <- as.POSIXct(aggMin$time)
#delete first 15 minutes of data to deal with warm up issues
aggMin <- aggMin[-(1:15),]
#aggregate to hourly basis
aggHr <- aggregate(aggMin[c("rh","temp","NO2_rs","NO2_est","CO_rs","CO_est","O3_rs","O3_est","dust_est")],
                    by=list(cut(aggMin$time,breaks="hour")),
                    FUN=mean)
names(aggHr)[names(aggHr)=="Group.1"] <- "time"
aggHr$time <- as.POSIXct(aggHr$time)

#calculate the original estimated concentration value. The process is to look up the Rs/Ro ratio (RsRatio)
#in the lookup table and then do a linear interpolation of the conc values.
tmpColName <- paste("aggMin$",gas,"_rs", sep="")
#multiply by 1000 to get the denominator into kohms
testRsRatio <- 1000*eval(parse(text = tmpColName))/R0.gas
tmp <- approx(lookup$Rs,lookup$conc,xout = testRsRatio, yleft = 0)
origEstName <- paste(gas,"_est_org", sep="")
aggMin$tmp <- tmp$y
names(aggMin)[names(aggMin) == 'tmp'] <- origEstName

#do a 6 graph plot of data from the egg
timeStart <- min(aggMin$time)
timeEnd <- max(aggMin$time)
#Graph title for 6 graph plot
mainText <- paste("Palisade ozone egg data,", as.POSIXct(timeStart), "to",as.POSIXct(timeEnd))
SixPlotOutputFileName <- "SixPlotOutputFileName.png"
aqPlot(aggMin,mainText, SixPlotOutputFileName)

#now import state data for ozone from Palisade site
#ozone units are in ppm, converted to ppb below
CDPHE.tt <- read.csv(paste(getwd(),"/data/csv/",stateDataFileName,sep=""), 
               skip=2, header = FALSE,
               stringsAsFactors=FALSE, 
               sep = ",",
               col.names = c("time","Pal.O3","Pal.WD","Pal.WS"),
               colClasses = c("character",rep("numeric",3)))
CDPHE.tt$Pal.O3 <- CDPHE.tt$Pal.O3 *1000
CDPHE.tt$time <- as.POSIXct(CDPHE.tt$time, format="%m/%d/%Y %I:%M %p", tz = "America/Denver")

combined <- merge(aggMin,CDPHE.tt, by="time")

# p1 <- ggplot(data=combined, aes(x=time)) + 
#   geom_line(aes(y = O3_est, color = "O3_est")) + 
#   geom_line(aes(y = O3_est_org, color = "O3_est_org")) + 
#   geom_line(aes(y = Pal.O3, color = "Pal.O3")) + 
#   labs(x="Time", y="Ozone (ppb)") +
#   scale_color_manual("",breaks = c("O3_est", "O3_est_org", "Pal.O3"),
#                       values = c("red", "green", "blue"))
# 
# p1


##do regressions for gas
rhs <- paste(dataCollectionLocationAbrev,".",gas,sep = "")
gas_rs <- paste(gas, "_rs", sep = "")

fit <- lm(eval(parse(text=rhs)) ~ 
            eval(parse(text=gas_rs)) + 
            I(eval(parse(text=gas_rs))^2) + 
            I(eval(parse(text=gas_rs))^3) + 
            temp + 
            rh, 
          data=combined)
#summary(fit)
par(mfrow=c(2,2),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(fit)
# plot(combined$time,combined$GJShelt.CO,xlab = "time", ylab="CO, GJ Shelter (ppm)")
# plot(combined$time,combined$temp,xlab = "time", ylab="egg temp (C)")
# plot(combined$time,combined$rh,xlab = "time", ylab="egg relative humidity (%)")
# plot(fit$residuals,xlab = "time", ylab="regression residuals")
# adjust for outliers using Cook's distance. Rule is to throw out observations where cooks.distance is greater
# than 4/nrow (see http://en.wikipedia.org/wiki/Cook%27s_distance#cite_note-3p)
# Note that the Cooks cutoff value is sometimes 4/(n-k) where k is number of independent variables
par(mfrow=c(1,1),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(cooks.distance(fit))
cutoff <- 4/fit$df.residual
abline(h=cutoff, lty=2, col=c("orange", "red"))
cooksDistance <- cooks.distance(fit)
cooksDistance <- as.data.frame(cooksDistance)
combined$cooksDistance <- cooksDistance$cooksDistance
combined.reduced <- subset(combined, cooksDistance < cutoff)

fit.reduced <- lm(eval(parse(text=rhs)) ~ 
            eval(parse(text=gas_rs)) + 
            I(eval(parse(text=gas_rs))^2) + 
            I(eval(parse(text=gas_rs))^3) + 
            temp + 
            rh, 
          data=combined.reduced)

summary(fit)
summary(fit.reduced)

combined.reduced$fitted <- fit.reduced$fitted.values

gasEst <- paste(gas,"_est",sep = "")
gasEstOrg <- paste(gas,"_est_org",sep = "")
gasStation <- paste(dataCollectionLocationAbrev,gas,sep = ".")
mainTitle <- paste(gas,"official and egg estimates for", dataCollectionLocation)
color1 <- paste(gas,"est w/ old params")
color2 <- paste(gas,"est using egg lookup table")
color3 <- paste(gas,"est w/ new params")
color4 <- paste(dataCollectionLocationAbrev,gas,"monitor")
gasEst <- paste(gas,"_est",sep = "")
gasEstOrg <- paste(gas,"_est_org",sep = "")
gasStation <- paste(dataCollectionLocationAbrev,gas,sep = ".")
mainTitle <- paste(gas,"official and egg estimates for", dataCollectionLocation)
p2 <- ggplot(data=combined.reduced, aes(x=time)) + 
  geom_line(aes(y = eval(parse(text=gasStation)), color = color4)) + 
  geom_line(aes(y = eval(parse(text=gasEst)),     color = color1)) + 
  geom_line(aes(y = eval(parse(text=gasEstOrg)),  color = color2)) + 
  geom_line(aes(y = fitted,     color = color3)) + 
  labs(x="Time", y=gas) +
  theme(legend.position="bottom") +
  scale_color_manual("",breaks = c(color4, color3, color2, color1), 
                     limits = c(color4, color3, color2, color1),
                     values = c("red", "blue", "gray","green"))

p2
outputPlot <- paste(gas,"Plot.png",sep = "")
ggsave(file = outputPlot)

#create styles to format the worksheets
numStyle <- createStyle(numFmt = "0.0")
numStyle3 <- createStyle(numFmt = "0.000")
shareStyle <- createStyle(numFmt = "0.0%")
textStyle <- createStyle(fontName = NULL, fontSize = NULL, fontColour = NULL,
                         numFmt = "GENERAL", border = NULL,
                         borderColour = getOption("openxlsx.borderColour", "black"),
                         borderStyle = getOption("openxlsx.borderStyle", "thin"), bgFill = NULL,
                         fgFill = NULL, halign = NULL, valign = NULL, textDecoration = NULL,
                         wrapText = FALSE, textRotation = NULL)

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

#copy in the parameter values used to generate estimates of concentrations used while the egg was collecting data
addWorksheet(wb, sheetName="parameter values")
writeData(wb, eggParams, sheet="parameter values", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
#addStyle(wb, sheet="parameter values", style=textStyle, rows = 1:nrow(sheetNameMeta), cols=1:(ncol(sheetNameMeta)), gridExpand = TRUE)
setColWidths(wb, sheet="parameter values", cols = 1:2, widths=30)

sheetNameList <- rbind(sheetNameList,"parameter values")
sheetNameDesc <- rbind(sheetNameDesc,"Values of the parameters used to create the original estimates of gas concentrations")

#copy in the summary data from the combined.reduced data 
sumDF <- as.data.frame(summary(combined.reduced), stringsAsFactors=FALSE)
sumDF <- sumDF[-1]
colnames(sumDF) <- c("varName","freq")
sumDF$varName <-as.character(sumDF$varName)
sumDF$freq <-as.character(sumDF$freq)
sumDF$freqName <- substring(sumDF$freq, 1,7)
sumDF$value <- substring(sumDF$freq, 9,)
sumDF <- sumDF[c("varName","freqName","value")]
temp4 <- dcast(sumDF,freqName ~ varName, value.var = "value")
addWorksheet(wb, sheetName="summaryData")
writeData(wb, temp4, sheet="summaryData", startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
sheetNameList <- rbind(sheetNameList,"summary Data")
sheetNameDesc <- rbind(sheetNameDesc,"Summary of the data available for analysis after outliers are removed")

temp.fit <- lmOut(fit)

addWorksheet(wb, sheetName="O3 params, uncorrected")
sheetNameList <- rbind(sheetNameList,"O3 params, uncorrected")
sheetNameDesc <- rbind(sheetNameDesc,"Ozone results uncorrected for outliers")
writeData(wb, temp.fit, sheet="O3 params, uncorrected", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)

#create worksheet and csv file for outlier corrected results

paramResults <- paste(gas,dataCollectionLocation,sep = ".")
paramFileName <- paste("results/param.",paramResults,dataCollectionDate,".csv",sep="")
temp.fit.reduced <- lmOut(fit.reduced,file=paramFileName, ndigit=4, writecsv=T)
addWorksheet(wb, sheetName="O3 params, corrected")
sheetNameList <- rbind(sheetNameList,"O3 params, corrected")
sheetNameDesc <- rbind(sheetNameDesc,"Ozone results corrected for outliers identified using Cooks Distance")
writeData(wb, temp.fit.reduced, sheet="O3 params, corrected", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)


#add graphics files

# Plot of six egg outputs; plot must be saved to a file previously
addWorksheet(wb, sheetName="EggOutput")
insertImage(wb, SixPlotOutputFileName, sheet = "EggOutput",  startRow = 4,
            startCol = 1, width = 8, height = 4, units = "in", dpi = 300)
# # Remove the plot from the disk
# res<-file.remove(SixPlotOutputFileName)
sheetNameList <- rbind(sheetNameList,"EggOutput")
sheetNameDesc <- rbind(sheetNameDesc,"Plots of 6 data sets from the egg")

# Plot of  estimates; plot must be saved to a file previously
gasEstimates <- paste(gas, "graphs")
addWorksheet(wb, sheetName=gasEstimates)
insertImage(wb, outputPlot, sheet = gasEstimates,  startRow = 4,
            startCol = 1, width = 8, height = 4, units = "in", dpi = 300)
sheetNameList <- rbind(sheetNameList,gasEstimates)
sheetNameDesc <- rbind(sheetNameDesc,"Plots of estimates of ozone concentrations")

#add sheet with info about each of the worksheets
sheetNameMeta <-as.data.frame(cbind(sheetNameList,sheetNameDesc))
addWorksheet(wb, sheetName="sheetInfo")
writeData(wb, sheetNameMeta, sheet="sheetInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
addStyle(wb, sheet="sheetInfo", style=textStyle, rows = 1:nrow(sheetNameMeta), cols=1:(ncol(sheetNameMeta)), gridExpand = TRUE)
setColWidths(wb, sheet="sheetInfo", cols = 1:2, widths=20)

#move sheetInfo worksheet from the last to the first
temp<- 2:length(names(wb))-1
temp <- c(length(names(wb)),temp)
worksheetOrder(wb) <- temp

#write the estimated parameters, the gas rs, and official values to a file for future use
gasLocation <- paste(gas,dataCollectionLocation,sep = ".")
paramFileName <- paste("results/param.",gasLocation,dataCollectionDate,".csv",sep="")
write.csv(fit.reduced$coefficients,paramFileName)

data.out <- paste("results/combined.reduced.",gasLocation,dataCollectionDate,".csv",sep="")
write.csv(combined.reduced,file = data.out)

resultsType <- paste(gas,dataCollectionLocation,sep = ".")
OutFileName <- paste("results/",resultsType,".Results.",dataCollectionDate,".xlsx",sep="")

saveWorkbook(wb, OutFileName, overwrite = TRUE)


resultsType <- paste(gas,dataCollectionLocation,sep = ".")

OutFileName <- paste("results/",resultsType,".Results.",dataCollectionDate,".xlsx",sep="")

saveWorkbook(wb, OutFileName, overwrite = TRUE)

# #---------- trying tobit regression
# estResult <- censReg( combined.reduced$Pal.O3 ~ combined.reduced$O3_rs + I(combined.reduced$O3_rs^2) + 
#                         combined.reduced$temp + combined.reduced$rh, data=combined.reduced, left = 0)
# summary(estResult)
# 
# plot(estResult$residuals,xlab = "time", ylab="regression residuals")
