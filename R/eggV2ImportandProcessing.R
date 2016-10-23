# code to compare egg and CDPHE results
library(plyr)
library(ggplot2)
library(car)
library(grid)
library(gridExtra)
setwd("~/Documents/workspace/AQEAnalysis")
fileLocation <- "data_raw"
timezone <- "America/Denver"
eggFileList <- c("PowellPitkenEggV2a20160122.csv",
              "PowellPitkenEggV2b20160122.csv",
              "PowellPitkenEggV2c20160122.csv")
cdpheFilelist <- c("PowellPitkenCDPHE_012216_012316.csv")
#location info for graphing
mainText <- "Powell/Pitken AQE V2 data"

aqev2a <- eggFileList[1]
aqev2b <- eggFileList[2]
aqev2c <- eggFileList[3]
cdphe <-cdpheFilelist
# read in cdphe data
df.cdphe <- read.csv(paste(getwd(),fileLocation,cdphe,sep="/"), 
                      skip = 3,header = FALSE,
                      stringsAsFactors=FALSE, 
                      sep = ",",
                      col.names = c("time_local","co_ppm","ws_mph","resultant_speed_mph","wd_degree","resultant_dir_degree","humidity_percent","temp_farenheit","pm10","pm25"),
                      colClasses = c("character",rep("numeric",9))
)

#drop resultant speed and direction
df.cdphe <- df.cdphe[,c("time_local","co_ppm","ws_mph","wd_degree","humidity_percent","temp_farenheit","pm10","pm25")]

#read in the egg data
dropList <- c("latitude_deg","longitude_deg", "altitude_m")

df.aqev2a <- read.csv(paste(getwd(),fileLocation,aqev2a,sep="/"), 
                      skip = 1,header = FALSE,
                      stringsAsFactors = FALSE,
                      col.names = c(
                        "time_local",
                        "temp_C_v2a",
                        "humidity_percent_v2a",
                        "no2_ppb_v2a",
                        "co_ppm_v2a",
                        "no2_v_v2a",
                        "co_v_v2a"
                        #         "latitude_deg",
                        #         "longitude_deg",
                        #         "altitude_m"
                      ))

#v2a currently doesn't include the first three columns in the droplist
df.aqev2b <- read.csv(paste(getwd(),fileLocation,aqev2b,sep="/"), 
                      skip = 1,header = FALSE,
                      stringsAsFactors = FALSE,
                      col.names = c(
                        "time_local",
                        "temp_C_v2b",
                        "humidity_percent_v2b",
                        "so2_ppb_v2b",
                        "o3_ppb_v2b",
                        "so2_v_v2b",
                        "o3_v_v2b",
                        "latitude_deg",
                        "longitude_deg",
                        "altitude_m"
                      )
)
df.aqev2b[dropList[1]] <- df.aqev2b[dropList[2]]<- df.aqev2b[dropList[3]]  <- NULL

df.aqev2c <- read.csv(paste(getwd(),fileLocation,aqev2c,sep="/"), 
                      skip = 1,header = FALSE,
                      stringsAsFactors = FALSE,
                      col.names = 
                        c(
                          "time_local",
                          "temp_C_v2c",
                          "humidity_percent_v2c",
                          "pm_mgram_per_m3_v2c",
                          "pm_v_v2c",
                          "latitude_deg",
                          "longitude_deg",
                          "altitude_m"
                        )
)
df.aqev2c[dropList[1]] <- df.aqev2c[dropList[2]]<- df.aqev2c[dropList[3]]  <- NULL

# Note: Time choices for breaks are "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year"
# For Date objects, only interval specifications using "day", "week", "month", "quarter" and "year" are allowed.)

df.aqev2a$time_local <- as.POSIXct(df.aqev2a$time_local,format="%m/%d/%Y %H:%M:%S", tz = timezone)
df.aqev2b$time_local <- as.POSIXct(df.aqev2b$time_local,format="%m/%d/%Y %H:%M:%S", tz = timezone)
df.aqev2c$time_local <- as.POSIXct(df.aqev2c$time_local,format="%m/%d/%Y %H:%M:%S", tz = timezone)
df.cdphe$time_local  <- as.POSIXct(df.cdphe$time_local, format="%d-%b-%Y %H:%M", tz = timezone)
df.cdphe$time_local <- as.POSIXct(strftime(df.cdphe$time_local, format="%Y-%m-%d %H:%M", tz = timezone))
df.cdphe.min <- df.cdphe # to keep the naming convention the same

#aggregate to minutes
df.aqev2a.min <- aggregate(df.aqev2a[c(2:ncol(df.aqev2a))],
                        by = list(cut(df.aqev2a$time_local, breaks = "min")),
                        FUN = mean)
names(df.aqev2a.min)[names(df.aqev2a.min)=="Group.1"] <- "time_local"
df.aqev2a.min$time_local <- as.POSIXct(df.aqev2a.min$time_local)

df.aqev2b.min <- aggregate(df.aqev2b[c(2:ncol(df.aqev2b))],
                           by = list(cut(df.aqev2b$time_local, breaks = "min")),
                           FUN = mean)
names(df.aqev2b.min)[names(df.aqev2b.min)=="Group.1"] <- "time_local"
df.aqev2b.min$time_local <- as.POSIXct(df.aqev2b.min$time_local)

df.aqev2c.min <- aggregate(df.aqev2c[c(2:ncol(df.aqev2c))],
                           by = list(cut(df.aqev2c$time_local, breaks = "min")),
                           FUN = mean)
names(df.aqev2c.min)[names(df.aqev2c.min)=="Group.1"] <- "time_local"
df.aqev2c.min$time_local <- as.POSIXct(df.aqev2c.min$time_local)

#join all the dfs
df.merged.min <- Reduce(function(...) merge(..., by = "time_local", all=TRUE), 
                    list(df.aqev2a.min, df.aqev2b.min, df.aqev2c.min, df.cdphe))

# aggregate to hours -----

df.aqev2a.hour <- aggregate(df.aqev2a[c(2:ncol(df.aqev2a))],
                           by = list(cut(df.aqev2a$time_local, breaks = "hour")),
                           FUN = mean)
names(df.aqev2a.hour)[names(df.aqev2a.hour)=="Group.1"] <- "time_local"
df.aqev2a.hour$time_local <- as.POSIXct(df.aqev2a.hour$time_local)

df.aqev2b.hour <- aggregate(df.aqev2b[c(2:ncol(df.aqev2b))],
                           by = list(cut(df.aqev2b$time_local, breaks = "hour")),
                           FUN = mean)
names(df.aqev2b.hour)[names(df.aqev2b.hour)=="Group.1"] <- "time_local"
df.aqev2b.hour$time_local <- as.POSIXct(df.aqev2b.hour$time_local)

df.aqev2c.hour <- aggregate(df.aqev2c[c(2:ncol(df.aqev2c))],
                            by = list(cut(df.aqev2c$time_local, breaks = "hour")),
                            FUN = mean)
names(df.aqev2c.hour)[names(df.aqev2c.hour)=="Group.1"] <- "time_local"
df.aqev2c.hour$time_local <- as.POSIXct(df.aqev2c.hour$time_local)

df.cdphe.hour <- aggregate(df.cdphe[c(2:ncol(df.cdphe))],
                           by = list(cut(df.cdphe$time_local, breaks = "hour")),
                           FUN = mean)
names(df.cdphe.hour)[names(df.cdphe.hour)=="Group.1"] <- "time_local"
df.cdphe.hour$time_local <- as.POSIXct(df.cdphe.hour$time_local)

#join all the hour dfs
df.merged.hour <- Reduce(function(...) merge(..., by = "time_local", all=TRUE), 
                        list(df.aqev2a.hour, df.aqev2b.hour, df.aqev2c.hour, df.cdphe.hour))
# ----------------

#Drop rows with NA in co_ppb
# df.joined <- df.merged.min[complete.cases(df.merged.min["o3_v"],)]
#                        df.joined[!is.na(df.joined["co_ppb"]),]
                       
#drop first fifteen minutes to account for warmup
# df.joined <- df.joined[16:dim(df.joined)[1],]

# -------- plotting stuff
timeStart <- df.joined$time[1]
outputFileName <- "PitkenAQEVevoltagegraph.png"
timeEnd <- df.joined$time[length(df.joined)]
rect1 <- data.frame (xmin=timeStart, xmax=timeEnd, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=timeStart, xmax=timeEnd, ymin=-Inf, ymax=Inf)

#graph temperature ----
p_temp<- ggplot() + geom_line(data=df.joined, aes(time_local, temp_C), color="red") + 
  labs(x="Time", y="Temp (v2.a) (°C)") + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  theme(plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))

#graph relative humidity ----
p_rh<- ggplot() + geom_line(data=df.joined, aes(time_local, humidity_percent), color="green") + 
  labs(x="Time", y="Relative humidity (v2.a) (%)") + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  theme(plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))

#graph ozone ppb ----
p_o3_ppb<- ggplot() + geom_line(data=df.joined, aes(time_local, o3_ppb), color="blue") + 
  
  labs(x=NULL, y=expression("AQE2b " * O[3] * " (ppb)")) + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
  geom_hline(aes(yintercept=75), colour="#990000", linetype="dashed") +
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.title=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))

#graph ozone voltage ----
p_o3_v <- ggplot() + geom_line(data=df.joined, aes(time_local,o3_v), color="yellow") + 
  labs(x=NULL, y=expression(O[3] * " voltage")) + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
#  geom_hline(aes(yintercept=900), color="#990000", linetype="dashed") +
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.title=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))

#graph NO2 ppb ----
p_no2_ppb<- ggplot() + geom_line(data=df.joined, aes(time_local,no2_ppb), color="yellow") + 
  labs(x=NULL, y=expression("AQE2a " * NO[2] * " (ppb)")) + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
#  geom_hline(aes(yintercept=15), colour="#990000", linetype="dashed") +
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.title=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))

#graph NO2 voltage ----
p_no2_v<- ggplot() + geom_line(data=df.joined, aes(time_local,no2_v), color="yellow") + 
  labs(x=NULL, y=expression(NO[2] * "voltage")) + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
#  geom_hline(aes(yintercept=15), colour="#990000", linetype="dashed") +
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.title=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))

#graph particulates mgram_per_m3 ----
p_part_mgram<- ggplot() + geom_line(data=df.joined, aes(time_local,pm_mgram_per_m3), color="yellow") + 
  labs(x=NULL, expression(paste(Particulates, " ", "(", mu, g/m^3,")"))) + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
  #  geom_hline(aes(yintercept=15), colour="#990000", linetype="dashed") +
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.title=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))

p_part_mgram <- p_part_mgram + geom_hline(aes(yintercept = 35),
                colour = "black",
                linetype = "dashed")

#graph SO2 ppb ----
p_so2_ppb <- ggplot() + geom_line(data=df.joined, aes(time_local, so2_ppb), color="blue") + 
  
  labs(x=NULL, y=expression("AQE2b " * SO[2] * " (ppb)")) + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
  geom_hline(aes(yintercept=75), colour="#990000", linetype="dashed") +
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.title=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.5,0.5,-0,0.5), "lines"))

#graph SO2 voltage ----
p_so2_v <- ggplot() + geom_line(data=df.joined, aes(time_local,so2_v), color="yellow") + 
  labs(x=NULL, y=expression(O[3] * " voltage")) + 
  #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) + 
  #  geom_hline(aes(yintercept=900), color="#990000", linetype="dashed") +
  scale_y_continuous() +
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
  geom_rect(data=rect2,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.1,fill="blue")+
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
#grid.arrange(gp1, gp2, gp5,gp4, gp3, gp6, main=textGrob(mainText,gp=gpar(fontsize=12,font=3)))
grid.arrange(gp1, gp2, gp3,gp5,gp4,gp6,  ncol=2,  main=textGrob(mainText,gp=gpar(fontsize=12,font=3)))
dev.off()

#two line plot
#write df.merged.min to temp df dropping missing values in the pm10 column
temp.min <- df.merged.min[complete.cases(df.merged.min[,"pm10"]),]
temp.hour <- df.merged.hour[complete.cases(df.merged.hour[,"pm10"]),]
p10 <- ggplot(data = temp.hour, aes(x = time_local)) +                    # basic graphical object
  geom_line(aes(y = pm_mgram_per_m3_v2c, colour="pmV2c")) +  # first layer
  geom_line(aes(y= pm25, colour="pm25")) +   # second layer
  geom_line(aes(y= pm10, colour="pm10")) +
  ylab("mgrams per m3") + xlab("time") + title("Particulates comparison") +
  scale_colour_manual("", 
                      breaks = c("pmV2c", "pm25", "pm10"),
                      values = c("red", "green", "blue")) 

ggplot(df.merged.hour, aes(time_local, pm_mgram_per_m3_v2c, colour="red")) + 
  geom_line() + geom_point()