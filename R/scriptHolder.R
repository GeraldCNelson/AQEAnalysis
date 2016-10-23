library(data.table)
#library(car) #Companion to Applied Regression
library(censReg) #to do tobit style regressions
# ggplot2, grid, gridExtra and scales (I think) are for plotting features
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(reshape)
library(reshape2)
library(openxlsx) #openxlsx - manipulate xlsx files
library(stargazer) # nicely formatted output

#This script is to hold functions and load libraries. It contains
#agPlot - do a 6 figure plot of data from the egg. It currently displays temp, rh, O3, CO, NO2_rs, particulates
#... It writes a png file to outputFileName
#lmOut - writes regression results in a csv format
#extractEggParams - needs to know the file name for the egg data collected at a monitoring site. Assumes it is stored in data/csv.
#aqComparePlot - does plots of the same variable (temp, humidity, gas concentration) from multiple eggs.
# The idea is to see how close the results are from eggs that collect data physically close to each other.


# function extractEggParams -----------------------------------------------

extractEggParams<- function(eggDataFileName) {
  #read in the egg parameters for writing out to the metadata
thepage = readLines(paste("data-raw/",eggDataFileName, sep=""), n = 100)
mypattern1 = 'Here are the updated coefficients:'
mypattern2 = 'Current Date/Time'
row1 <-  grep(mypattern1,thepage) + 1
row2 <-  grep(mypattern2,thepage) - 1
#split the single string at the colon
eggParams <- strsplit(thepage[row1:row2], split = "[:]")

eggParams <- as.data.frame(t(sapply(eggParams,c)),stringsAsFactors=FALSE) 
colnames(eggParams) <- c("param_name","param_value")
eggParams$param_value <- as.numeric(eggParams$param_value)
return(eggParams)
}


# function aqPlot ---------------------------------------------------------

aqPlot <- function(aggMin,mainText,outputFileName) {
  rect1 <- data.frame (xmin=timeStart, xmax=timeEnd, ymin =-Inf, ymax =Inf)
  rect2 <- data.frame (xmin=timeStart, xmax=timeEnd, ymin =-Inf, ymax =Inf)
  
  p1<- ggplot() + geom_line(data=aggMin, aes(time, temp), color="red") + 
    labs(x="Time", y="Temperature (°C)") + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p2<- ggplot() + geom_line(data=aggMin, aes(time, rh), color="green") + 
    labs(x="Time", y="Relative humidity (%)") + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p3<- ggplot() + geom_line(data=aggMin, aes(time, O3_est), color="blue") + 

    labs(x=NULL, y=expression(O[3] * " (ppb)")) + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    geom_hline(aes(yintercept=75), colour="#990000", linetype="dashed") +
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p4<- ggplot() + geom_line(data=aggMin, aes(time,CO_est), color="yellow") + 
    labs(x=NULL, y="CO (ppb)") + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    geom_hline(aes(yintercept=900), color="#990000", linetype="dashed") +
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p5<- ggplot() + geom_line(data=aggMin, aes(time, dust_est), color="black") + 
    labs(x=NULL, y=expression(paste(PM[2.5], " ", "(", mu, g/m^3,")"))) + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    geom_hline(aes(yintercept=15), colour="#990000", linetype="dashed") +
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p6<- ggplot() + geom_line(data=aggMin, aes(time, NO2_rs), color="black") + 
    labs(x=NULL, y= expression(paste(NO[2]," ", r[s]))) + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  gp1<- ggplot_gtable(ggplot_build(p1))
  gp2<- ggplot_gtable(ggplot_build(p2))
  gp3<- ggplot_gtable(ggplot_build(p3))
  gp4<- ggplot_gtable(ggplot_build(p4))
  gp5<- ggplot_gtable(ggplot_build(p5))
  gp6<- ggplot_gtable(ggplot_build(p6))
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp3$widths[2:3], gp4$widths[2:3], 
                       gp5$widths[2:3], gp6$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth
  gp3$widths[2:3] <- maxWidth
  gp4$widths[2:3] <- maxWidth
  gp5$widths[2:3] <- maxWidth
  gp6$widths[2:3] <- maxWidth
  png(outputFileName)
  grid.arrange(gp1, gp2, gp5,gp4, gp3, gp6, main=textGrob(mainText,gp=gpar(fontsize=12,font=3)))
  dev.off()
  
}


# function aqComparePlot --------------------------------------------------

aqComparePlot <- function(variable,AQE2aEggs) {
  for (i in 1:length(AQE2aEggs)) {
    fileNm <- parse(paste("dfMin_",AQE2aEggs[i], "_", variable, sep=""))
    timeStart <- parse(paste(fileNm, "$timeGMT[1]", sep=""))
    timeEnd <- eval(paste(fileNm,"$timeGMT[",  length(eval(fileNm)), "]", sep = ""))
    rect1 <- data.frame (xmin=timeStart, xmax = timeEnd, ymin = -Inf, ymax = Inf)
  rect2 <- data.frame (xmin=timeStart, xmax=timeEnd, ymin = -Inf, ymax = Inf)
  
  p<- ggplot() + geom_line(data=paste("dfMin",AQE2aEggs[i],"variable", sep="_"), 
                            aes(time, variable), color="red") + 
    labs(x="Time", y=variable) + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  p
  }
  
  p2<- ggplot() + geom_line(data=aggMin, aes(time, rh), color="green") + 
    labs(x="Time", y="Relative humidity (%)") + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p3<- ggplot() + geom_line(data=aggMin, aes(time, O3_est), color="blue") + 
    
    labs(x=NULL, y=expression(O[3] * " (ppb)")) + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    geom_hline(aes(yintercept=75), colour="#990000", linetype="dashed") +
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p4<- ggplot() + geom_line(data=aggMin, aes(time,CO_est), color="yellow") + 
    labs(x=NULL, y="CO (ppb)") + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    geom_hline(aes(yintercept=900), color="#990000", linetype="dashed") +
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p5<- ggplot() + geom_line(data=aggMin, aes(time, dust_est), color="black") + 
    labs(x=NULL, y=expression(paste(PM[2.5], " ", "(", mu, g/m^3,")"))) + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    geom_hline(aes(yintercept=15), colour="#990000", linetype="dashed") +
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  p6<- ggplot() + geom_line(data=aggMin, aes(time, NO2_rs), color="black") + 
    labs(x=NULL, y= expression(paste(NO[2]," ", r[s]))) + 
    #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
    scale_y_continuous() +
    geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin =ymin,ymax =ymax),alpha=0.1,fill="blue")+
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          plot.title=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))
  
  gp1<- ggplot_gtable(ggplot_build(p1))
  gp2<- ggplot_gtable(ggplot_build(p2))
  gp3<- ggplot_gtable(ggplot_build(p3))
  gp4<- ggplot_gtable(ggplot_build(p4))
  gp5<- ggplot_gtable(ggplot_build(p5))
  gp6<- ggplot_gtable(ggplot_build(p6))
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp3$widths[2:3], gp4$widths[2:3], 
                       gp5$widths[2:3], gp6$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth
  gp3$widths[2:3] <- maxWidth
  gp4$widths[2:3] <- maxWidth
  gp5$widths[2:3] <- maxWidth
  gp6$widths[2:3] <- maxWidth
  png(outputFileName)
  grid.arrange(gp1, gp2, gp5,gp4, gp3, gp6, main=textGrob(mainText,gp=gpar(fontsize=12,font=3)))
  dev.off()
  
}

lmOut <- function(res, file="test.csv", ndigit=4, writecsv=T) {
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