setwd("~/Documents/workspace/AQEAnalysis")

#scriptHolder calls all the standard, needed libraries and loads functions
source(file = "scriptHolder.R")
library(systemfit)
#It contains
#agPlot - do a 6 figure plot of data from the egg. It currently displays temp, rh, O3, CO, NO2_rs, particulates
#lmOut - prepares regression results in a csv format and also writes a csv file if that option is chosen

#userName is reported in the basic info worksheet
userName <- "Gerald Nelson"
#choose a contaminant to work on.
gas <- "O3" # choices are NO2, CO, O3, dust; at Palisade only O3

# use gasSubscript in plots
if (gas == "O3") {gasSubscript <- "O[3]"}
if (gas == "NO2") {gasSubscript <- "NO[2]"}
if (gas == "CO") {gasSubscript <- "CO"}
#fileNames
#The removed outliers from the state data are around midnight when some ozone is 
#injected for calibration purposes
obs1 <- c("Pal","Palisade","CO","Palisade_O3_WS_WD_01122015OutliersRemoved.csv","EggData2015_01_12_100220.csv","2015_01_12 - 2015_01_13","2015_01_12")
obs2 <- c("Pal","Palisade","CO","Palisade_O3_WS_WD_03232015OutliersRemoved.csv","EggData2015_03_23_131853.csv","2015_03_23 - 2015_03_24","2015_03_23")
obs3 <- c("Gln","Glenwood Springs","CO","GACO-VP_MinuteData_150331-150401.csv","EggData2015_03_31_140153.csv","2015_03_31 - 2015_4_1", "2015_03_31")

aqObs <- as.data.frame(rbind(obs1,obs2,obs3))
colnames(aqObs) <- c("cityAbr","city","state","stateData","eggData","longDates","shortDates")
write.csv(aqObs, file = "results/aqObs.csv") 
lookupFileName <- "lookupTables.xlsx"
R0 <- data.frame(
  gas = c("NO2","CO","O3","dust"),
  val = c(3892,868876,380952,360))
R0.gas <- R0$val[match(gas,R0$gas)]
lookup <- read.xlsx(paste("data/",lookupFileName,sep=""),sheet = gas, startRow = 1, colNames = FALSE)
colnames(lookup) <- c("table_row","row_value_1","row_value_2","RsRatio","conc")
lookup <- lookup[,-(1:3)]

#create empty list to hold regression results
resultsList <- list()
for (i in 1:nrow(aqObs)) {
  #Read in the original lookup table for gas
  
  eggParams <- extractEggParams(aqObs$eggData[i])
  
  #Read in the egg data
  tt <- read.csv(paste(getwd(),"/data/csv/",aqObs$eggData[i],sep=""), 
                 skip=64, header = FALSE,
                 stringsAsFactors=FALSE, 
                 sep = ",",
                col.names = c("time","rh","temp","NO2_rs","NO2_est","CO_rs","CO_est",
                              "O3_rs","O3_est","dust_est", "dust_rs"),
                 colClasses = c("character",rep("numeric",9))
  )
   tt[sapply(tt,is.na)] = 0
  tt$time <- as.POSIXct(tt$time, format="%m/%d/%Y %H:%M:%S", tz = "America/Denver")
  #---- absolute humidity equation, from https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/ 
  tt$ah=(6.112*exp(17.67*tt$temp/(tt$temp+243.5)) *tt$rh*2.1674)/(273.15+tt$temp)
  #tt$time <- tt$time + 12*60*60 #- in case tt$time is in 12 hour clock
  # Note: Time choices for breaks are "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year"
  # For Date objects, only interval specifications using "day", "week", "month", "quarter" and "year" are allowed.)
  #drop last row in case it is incomplete
  tt <- tt[-(nrow(tt)),]
  stargazer(tt, type = "text")
  #remove obs where NO2_rs is Inf
  temp <- is.infinite(tt$NO2_rs)
  tt = tt[!temp,]
  #convert NO2_rs to 1000 Kohms to deal with precision issues. Note that as the Rs values come from the egg
  #they are already in Kohms
  tt$NO2_rs <- tt$NO2_rs/1000
  aggMin <- aggregate(tt[c("rh","temp","NO2_rs","NO2_est","CO_rs","CO_est","O3_rs","O3_est","dust_rs","dust_est","ah" )],
                      by=list(cut(tt$time,breaks="min")),
                      FUN=mean)
  names(aggMin)[names(aggMin)=="Group.1"] <- "time"
  aggMin$time <- as.POSIXct(aggMin$time)
  #delete first 15 minutes of data to deal with warm up issues
  aggMin <- aggMin[-(1:15),]
  #aggregate to hourly basis
  aggHr <- aggregate(aggMin[c("rh","temp","NO2_rs","NO2_est","CO_rs","CO_est","O3_rs","O3_est","dust_rs","dust_est","ah" )],
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
  
  if (aqObs$city[i] == "Palisade") {
    #now import state data for ozone from Palisade site
    #ozone units are in ppm, converted to ppb below
    CDPHE.tt <- read.csv(paste(getwd(),"/data/csv/",aqObs$stateData[i],sep=""), 
                         skip=2, header = FALSE,
                         stringsAsFactors=FALSE, 
                         sep = ",",
                         col.names = c("time","Pal.O3","Pal.WD","Pal.WS"),
                         colClasses = c("character",rep("numeric",3)))
    CDPHE.tt$Pal.O3 <- CDPHE.tt$Pal.O3 *1000
    CDPHE.tt$time <- as.POSIXct(CDPHE.tt$time, format="%m/%d/%Y %I:%M %p", tz = "America/Denver")
    
combined <- merge(aggMin,CDPHE.tt, by="time")

  }
  else {
    # import  data from Glenwood site
    Gln.tt <- read.csv(paste(getwd(),"/data/csv/",aqObs$stateData[i],sep=""), 
                      skip=3, header = FALSE,
                      stringsAsFactors=FALSE, 
                      sep = ",",
                      col.names = c("date","timeInd", "time", "Gln.O3","Gln.NO","Gln.NO2",
                                    "Gln.NOX","Gln.CH4","Gln.NMHC","Gln.THC","xx"),
                      #                  colClasses = c(rep("character",3),rep("numeric",7))
    )
    
    # set -999 to NA
    Gln.tt[Gln.tt == -999] <- NA
    
    Gln.tt$time <- as.POSIXct(strptime(Gln.tt$time, format = "%m/%d/%y %H:%M", tz = "America/Denver"))
    Gln.tt <- Gln.tt[c("time", "Gln.O3","Gln.NO","Gln.NO2","Gln.NOX","Gln.CH4","Gln.NMHC","Gln.THC")]

combined <- merge(aggMin,Gln.tt, by="time")
  }

#remove rows where the gas value is NA
completeVec <- complete.cases(combined[, paste(aqObs$cityAbr[i],gas, sep=".")])
combined <- combined[completeVec, ]

##do regressions for gas
rhs <- paste(aqObs$cityAbr[i],".",gas,sep = "")
gas_rs <- paste(gas, "_rs", sep = "")


fit <- lm(eval(parse(text=rhs)) ~ 
            eval(parse(text=gas_rs)) + 
            I(eval(parse(text=gas_rs))^2) + 
            I(eval(parse(text=gas_rs))^3) + 
            temp + I(temp^2) + 
            ah + I(ah^2) +
            NO2_rs + I(NO2_rs^2),
            data=combined)
combined$fitted <- fit$fitted.values

#write the estimated parameters, the gas rs, and official values to a file for future use
paramResults <- paste(gas,aqObs$cityAbr[i],sep = ".")
paramFileName <- paste("results/param",paramResults,aqObs$shortDates[i],"csv",sep=".")
write.csv(fit$coefficients,paramFileName)

#rename fit
assign(paste("fit",gas,aqObs$cityAbr[i],aqObs$shortDates[i],sep = "."),fit)

# add the new name of the fit file to the list of results
c(resultsList, paste("fit",gas,aqObs$cityAbr[i],aqObs$shortDates[i],sep = "."))

#write out the combined df to a file used to create the results
gasLocation <- paste(gas,aqObs$cityAbr[i],sep = ".")
data.out <- paste("results/combined",gasLocation,aqObs$shortDates[i],"csv",sep=".")
write.csv(combined,file = data.out)

#---- Graph 4 plots of gas results
gasEst <- paste(gas,"_est",sep = "")
gasEstOrg <- paste(gas,"_est_org",sep = "")
gasStation <- paste(aqObs$cityAbr[i],gas,sep = ".")
mainTitle <- paste(gasSubscript,"*\"  official and AQE estimates for \"", "*\"",aqObs$city[i],"\"","*\"" , aqObs$shortDates[i],"\""); parse(text=mainTitle)

color1 <- paste(gasSubscript,"*\" est w/ old params\""); parse(text=color1)
color2 <- paste(gasSubscript,"*\" est using egg lookup table\""); parse(text=color2)
color3 <- paste(gasSubscript,"*\" est w/ new params\""); parse(text=color3)
color4 <- paste(aqObs$cityAbr[i]," *" ,  gasSubscript,"*\" monitor\"", sep=" "); parse(text=color4)


p2 <- ggplot(data=combined, aes(x=time)) + 
  geom_line(aes(y = eval(parse(text=gasStation)), color = color4)) + 
  geom_line(aes(y = eval(parse(text=gasEst)),     color = color1)) + 
  geom_line(aes(y = eval(parse(text=gasEstOrg)),  color = color2)) + 
  geom_line(aes(y = fitted,     color = color3)) + 
  labs(title = parse(text = mainTitle),x="Time", y=parse(text = gasSubscript)) +
  theme(legend.position="bottom") +
  scale_color_manual("",breaks = c(color1, color2, color3, color4), 
                     limits = c(color1, color2, color3, color4),
                     values = c("gray","red", "green", "blue"),
                     guide = guide_legend(title = waiver(),  nrow = 2),
                     labels = function(x) parse(text=x))

p2
outputPlot <- paste("results/",aqObs$cityAbr[i],".",gas,".",aqObs$shortDates[i],".","plot.png",sep = "")
ggsave(file = outputPlot)
#----

gasLocation <- paste(gas,aqObs$cityAbr[i],sep = ".")
data.out <- paste("results/combined.",gasLocation,aqObs$shortDates[i],".csv",sep="")
write.csv(combined,file = data.out)
}

#--- print the results in a pretty form
temp <-paste(gas,"concentration (ppb)")

stargazer(fit.O3.Pal.2015_01_12,fit.O3.Pal.2015_03_23,fit.O3.Gln.2015_03_31
          , type="html"
          , summary = TRUE
#          , dep.var.caption = "O<sub>3</sub> concentration (ppb)"
          , dep.var.labels   = "O<sub>3</sub> concentration (ppb)"
          , covariate.labels = c("Constant",
              "O<sub>3</sub>r<sub>s</sub> (Ω)", "(O<sub>3</sub>r<sub>s</sub>)<sup>2</sup>", 
              "(O<sub>3</sub>r<sub>s</sub>)<sup>3</sup>", 
              "Temp (°C)","Temp<sup>2</sup>", 
              "Abs. humidity (%)", "Abs. humidity<sup>2</sup>", 
              "NO<sub>2</sub>r<sub>s</sub> (kΩ)", "NO<sub>2</sub>r<sub>s</sub><sup>2</sup>")
          , column.labels = c("Palisade, Jan 12, 2015", "Palisade, Mar 23, 2015","Glenwood Springs, March 31, 2015")
          , digits = 4
          #, digits.extra = 10
          , style = "aer"
          , out = "results/stargazerout.html"
          , out.header = TRUE
          , object.names = TRUE
          , intercept.top = TRUE
          , intercept.bottom = FALSE
)
par(mfrow=c(2,2),cex = 0.7,cex.axis = 0.8, mar = c(4,4,1,1))
plot(fit)



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
creationInfo <- rbind(creationInfo, paste("Date data collected:", aqObs$shortDates[i]))
creationInfo <- rbind(creationInfo, paste("Location data collected:", paste(aqObs$city[i],aqObs$state[i],sep = ",")))
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

#copy in the summary data from the combined data 
sumDF <- as.data.frame(summary(combined), stringsAsFactors=FALSE)
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

data.out <- paste("results/combined.",gasLocation,dataCollectionDate,".csv",sep="")
write.csv(combined,file = data.out)

resultsType <- paste(gas,dataCollectionLocation,sep = ".")
OutFileName <- paste("results/",resultsType,".Results.",dataCollectionDate,".xlsx",sep="")

saveWorkbook(wb, OutFileName, overwrite = TRUE)


resultsType <- paste(gas,dataCollectionLocation,sep = ".")

OutFileName <- paste("results/",resultsType,".Results.",dataCollectionDate,".xlsx",sep="")

saveWorkbook(wb, OutFileName, overwrite = TRUE)

# #---------- trying tobit regression
# estResult <- censReg( combined$Pal.O3 ~ combined$O3_rs + I(combined$O3_rs^2) + 
#                         combined$temp + combined$rh, data=combined, left = 0)
# summary(estResult)
# 
# plot(estResult$residuals,xlab = "time", ylab="regression residuals")

#--- plots of NO2 relationships

pno2.no2rs <- ggplot(data=combined, aes(x=NO2_rs)) + 
  geom_point(aes(y = Gln.NO2))

rh.no2rs <- ggplot(data=combined, aes(x=NO2_rs)) + 
  geom_point(aes(y = rh))

temp.no2rs <- ggplot(data=combined, aes(x=NO2_rs)) + 
  geom_point(aes(y = temp))

temp.rh <- ggplot(data=combined, aes(x=rh)) + 
  geom_point(aes(y = temp))


pno2.temp <- ggplot(data=combined, aes(x=temp)) + 
  geom_point(aes(y = Gln.NO2))

pno2.rh <- ggplot(data=combined, aes(x=rh)) + 
  geom_point(aes(y = Gln.NO2))
            
grid.arrange(pno2.no2rs,rh.no2rs,temp.no2rs,temp.rh,pno2.temp,pno2.rh,ncol=2)

              
              
  geom_line(aes(y = eval(parse(text=gasEst)),     color = color1)) + 
  geom_line(aes(y = eval(parse(text=gasEstOrg)),  color = color2)) + 
  geom_line(aes(y = fitted,     color = color3)) + 
  labs(x="Time", y=gas) +
  theme(legend.position="bottom") +
  scale_color_manual("",breaks = c(color4, color3, color2, color1), 
                     limits = c(color4, color3, color2, color1),
                     values = c("red", "blue", "gray","green"))

p2


