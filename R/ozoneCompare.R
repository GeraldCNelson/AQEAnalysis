setwd("~/Documents/workspace/airQualityCompare")

#scriptHolder calls all the needed libraries and loads functions
source(file = "scriptHolder.R")

aqObs<- read.csv(file = "results/aqObs.csv") 

gas <- "O3"

# use gasSubscript in plots
if (gas == "O3") {gasSubscript <- "O[3]"}
if (gas == "NO2") {gasSubscript <- "NO[2]"}
if (gas == "CO") {gasSubscript <- "CO"}

PalgasLocation <- paste(gas,aqObs$cityAbr[1],sep = ".")
PalparamFileName1 <- paste("results/param",gas,aqObs$cityAbr[1],aqObs$shortDates[1],"csv",sep=".")
PalparamFileName2 <- paste("results/param",gas,aqObs$cityAbr[2],aqObs$shortDates[2],"csv",sep=".")

PalParamResults1 <- read.csv(PalparamFileName1, 
               skip=1, header = FALSE,
               stringsAsFactors=FALSE, 
               sep = ",",
               col.names = c("paramName","paramValue"),
               colClasses = c("character","numeric"))
PalParamResults2 <- read.csv(PalparamFileName2, 
                             skip=1, header = FALSE,
                             stringsAsFactors=FALSE, 
                             sep = ",",
                             col.names = c("paramName","paramValue"),
                             colClasses = c("character","numeric"))

#get Glenwood data

GlnparamFileName1 <- paste("results/param",gas,aqObs$cityAbr[3],aqObs$shortDates[3],"csv",sep=".")

GlnParamResults1 <- read.csv(GlnparamFileName1, 
                             skip=1, header = FALSE,
                             stringsAsFactors=FALSE, 
                             sep = ",",
                             col.names = c("paramName","paramValue"),
                             colClasses = c("character","numeric"))

# read in Gln combined.reduced values

data.in <- paste("results/combined",gas,aqObs$cityAbr[3],aqObs$shortDates[3],"csv",sep=".")

Gln.combined1 <- read.csv(data.in, 
               skip=0, header = TRUE,
               stringsAsFactors=FALSE, 
               sep = ","
)
Gln.combined1$time <- as.POSIXct(Gln.combined1$time, format="%Y-%m-%d %H:%M:%S", tz = "America/Denver")


#--- read in Palisade official and rs data

Pal.data.in1 <- paste("results/combined",gas,aqObs$cityAbr[1],aqObs$shortDates[1],"csv",sep=".")

Pal.combined1 <- 
  read.csv(Pal.data.in1, 
           header = TRUE,
           stringsAsFactors=FALSE, 
           sep = ",",
  )
Pal.combined1$time <- as.POSIXct(Pal.combined1$time, format="%Y-%m-%d %H:%M:%S", tz = "America/Denver")

Pal.data.in2 <- paste("results/combined",gas,aqObs$cityAbr[2],aqObs$shortDates[2],"csv",sep=".")

Pal.combined2 <- 
  read.csv(Pal.data.in2, 
           header = TRUE,
           stringsAsFactors=FALSE, 
           sep = ","
  )
Pal.combined2$time <- as.POSIXct(Pal.combined2$time, format="%Y-%m-%d %H:%M:%S", tz = "America/Denver")

PalRs1 <- Pal.combined1$O3_rs
Paltemp1 <- Pal.combined1$temp
Palah1 <- Pal.combined1$ah
Paloff1 <- Pal.combined1$Pal.O3
PalNO2r1 <- Pal.combined1$NO2_rs

PalRs2 <- Pal.combined2$O3_rs
Paltemp2 <- Pal.combined2$temp
Palah2 <- Pal.combined2$ah
Paloff2 <- Pal.combined2$Pal.O3
PalNO2r2 <- Pal.combined2$NO2_rs

GlnRs1 <- Gln.combined1$O3_rs
Glntemp1 <- Gln.combined1$temp
Glnah1 <- Gln.combined1$ah
Glnoff1 <- Gln.combined1$Gln.O3
GlnNO2r1 <- Gln.combined1$NO2_rs

#Estimate Pal1 results with Pal1 params
Pal1results.Pal1params <- 
  PalParamResults1$paramValue[1] +
  PalParamResults1$paramValue[5]*Paltemp1 +
  PalParamResults1$paramValue[6]*Paltemp1^2 +
  PalParamResults1$paramValue[7]*Palah1 +
  PalParamResults1$paramValue[8]*Palah1^2 +
  PalParamResults1$paramValue[9]*PalNO2r1 +
  PalParamResults1$paramValue[10]*PalNO2r1^2 +
  PalParamResults1$paramValue[2]*PalRs1 +
  PalParamResults1$paramValue[3]*PalRs1^2 + 
  PalParamResults1$paramValue[4]*PalRs1^3

#Estimate Pal1 results with Pal2 params
Pal1results.Pal2params <- 
  PalParamResults2$paramValue[1] +
  PalParamResults2$paramValue[5]*Paltemp1 +
  PalParamResults2$paramValue[6]*Paltemp1^2 +
  PalParamResults2$paramValue[7]*Palah1 +
  PalParamResults2$paramValue[8]*Palah1^2 +
  PalParamResults2$paramValue[9]*PalNO2r1 +
  PalParamResults2$paramValue[10]*PalNO2r1^2 +
  PalParamResults2$paramValue[2]*PalRs1 +
  PalParamResults2$paramValue[3]*PalRs1^2 + 
  PalParamResults2$paramValue[4]*PalRs1^3

#Estimate Pal1 results with Gln params
Pal1results.Gln1params <- 
  GlnParamResults1$paramValue[1] +
  GlnParamResults1$paramValue[5]*Paltemp1 +
  GlnParamResults1$paramValue[6]*Paltemp1^2 +
  GlnParamResults1$paramValue[7]*Palah1 +
  GlnParamResults1$paramValue[8]*Palah1^2 +
  GlnParamResults1$paramValue[9]*PalNO2r1 +
  GlnParamResults1$paramValue[10]*PalNO2r1^2 +
  GlnParamResults1$paramValue[2]*PalRs1 +
  GlnParamResults1$paramValue[3]*PalRs1^2 + 
  GlnParamResults1$paramValue[4]*PalRs1^3

#Estimate Pal2 results with Pal2 params
Pal2results.Pal2params <- 
  PalParamResults2$paramValue[1] +
  PalParamResults2$paramValue[5]*Paltemp2 +
  PalParamResults2$paramValue[6]*Paltemp2^2 +
  PalParamResults2$paramValue[7]*Palah2 +
  PalParamResults2$paramValue[8]*Palah2^2 +
  PalParamResults2$paramValue[9]*PalNO2r2 +
  PalParamResults2$paramValue[10]*PalNO2r2^2 +
  PalParamResults2$paramValue[2]*PalRs2 +
  PalParamResults2$paramValue[3]*PalRs2^2 + 
  PalParamResults2$paramValue[4]*PalRs2^3

#Estimate Pal2 results with Pal1 params
Pal2results.Pal1params <- 
  PalParamResults1$paramValue[1] +
  PalParamResults1$paramValue[5]*Paltemp2 +
  PalParamResults1$paramValue[6]*Paltemp2^2 +
  PalParamResults1$paramValue[7]*Palah2 +
  PalParamResults1$paramValue[8]*Palah2^2 +
  PalParamResults1$paramValue[9]*PalNO2r2 +
  PalParamResults1$paramValue[10]*PalNO2r2^2 +
  PalParamResults1$paramValue[2]*PalRs2 +
  PalParamResults1$paramValue[3]*PalRs2^2 + 
  PalParamResults1$paramValue[4]*PalRs2^3

#Estimate Pal2 results with Gln1 params
Pal2results.Gln1params <- 
  GlnParamResults1$paramValue[1] +
  GlnParamResults1$paramValue[5]*Paltemp2 +
  GlnParamResults1$paramValue[6]*Paltemp2^2 +
  GlnParamResults1$paramValue[7]*Palah2 +
  GlnParamResults1$paramValue[8]*Palah2^2 +
  GlnParamResults1$paramValue[9]*PalNO2r2 +
  GlnParamResults1$paramValue[10]*PalNO2r2^2 +
  GlnParamResults1$paramValue[2]*PalRs2 +
  GlnParamResults1$paramValue[3]*PalRs2^2 + 
  GlnParamResults1$paramValue[4]*PalRs2^3

#Estimate Gln1 results with Pal1 params
Gln1results.Pal1params <- 
  PalParamResults1$paramValue[1] +
  PalParamResults1$paramValue[5]*Glntemp1 +
  PalParamResults1$paramValue[6]*Glntemp1^2 +
  PalParamResults1$paramValue[7]*Glnah1 +
  PalParamResults1$paramValue[8]*Glnah1^2 +
  PalParamResults1$paramValue[9]*GlnNO2r1 +
  PalParamResults1$paramValue[10]*GlnNO2r1^2 +
  PalParamResults1$paramValue[2]*GlnRs1 +
  PalParamResults1$paramValue[3]*GlnRs1^2 + 
  PalParamResults1$paramValue[4]*GlnRs1^3

#Estimate Gln1 results with Pal2 params
Gln1results.Pal2params <- 
  PalParamResults2$paramValue[1] +
  PalParamResults2$paramValue[5]*Glntemp1 +
  PalParamResults2$paramValue[6]*Glntemp1^2 +
  PalParamResults2$paramValue[7]*Glnah1 +
  PalParamResults2$paramValue[8]*Glnah1^2 +
  PalParamResults2$paramValue[9]*GlnNO2r1 +
  PalParamResults2$paramValue[10]*GlnNO2r1^2 +
  PalParamResults2$paramValue[2]*GlnRs1 +
  PalParamResults2$paramValue[3]*GlnRs1^2 + 
  PalParamResults2$paramValue[4]*GlnRs1^3

#Estimate Gln1 results with Gln params
Gln1results.Gln1params <- 
  GlnParamResults1$paramValue[1] +
  GlnParamResults1$paramValue[5]*Glntemp1 +
  GlnParamResults1$paramValue[6]*Glntemp1^2 +
  GlnParamResults1$paramValue[7]*Glnah1 +
  GlnParamResults1$paramValue[8]*Glnah1^2 +
  GlnParamResults1$paramValue[9]*GlnNO2r1 +
  GlnParamResults1$paramValue[10]*GlnNO2r1^2 +
  GlnParamResults1$paramValue[2]*GlnRs1 +
  GlnParamResults1$paramValue[3]*GlnRs1^2 + 
  GlnParamResults1$paramValue[4]*GlnRs1^3


#plot results
i = 2 # Palisade March
gasEst <- paste(gasSubscript,"_est",sep = "")
gasEstOrg <- paste(gasSubscript,"_est_org",sep = "")
gasStation <- paste(aqObs$cityAbr[i],gas,sep = ".")
temp <- as.character(aqObs$city[i])
mainTitle <- paste(gasSubscript,"*\"  official and AQE estimates for \"", "*\"",aqObs$city[i],"\"","*\"" , aqObs$shortDates[i],"\""); parse(text=mainTitle)
color1 <- paste(gasSubscript,"*\" Palisade Mar, official\""); parse(text=color1)
color2 <- paste(gasSubscript,"*\" Palisade Mar, w/ Pal. Mar params\""); parse(text=color2)
color3 <- paste(gasSubscript,"*\" Palisade Mar, w/ Pal. Jan params\""); parse(text=color3)
color4 <- paste(gasSubscript,"*\" Palisade Mar, w/ Gln. Mar params\""); parse(text=color4)

pl.Pal2 <- ggplot(data = Pal.combined2, aes(x=time)) +
  geom_line(aes(y = Pal.O3, color = color1)) +
  geom_line(aes(y = Pal2results.Pal2params, color = color2)) +
  geom_line(aes(y = Pal2results.Pal1params, color = color3)) +
  geom_line(aes(y = Pal2results.Gln1params, color = color4)) +
  labs(title = parse(text = mainTitle),x="Time", y=parse(text = gasSubscript)) +
  theme(legend.position="bottom") +
  scale_color_manual("",breaks = c(color1, color2, color3, color4), 
                     limits = c(color1, color2, color3, color4),
                     values = c("red", "blue", "gray","green"),
                     guide = guide_legend(title = waiver(),  nrow = 2),
                     labels = function(x) parse(text=x))

pl.Pal2
i <- 1 # Palisade Jan
gasEst <- paste(gasSubscript,"_est",sep = "")
gasEstOrg <- paste(gasSubscript,"_est_org",sep = "")
gasStation <- paste(aqObs$cityAbr[i],gas,sep = ".")
temp <- as.character(aqObs$city[i])
mainTitle <- paste(gasSubscript,"*\"  official and AQE estimates for \"", "*\"",aqObs$city[i],"\"","*\"" , aqObs$shortDates[i],"\""); parse(text=mainTitle)
color1 <- paste(gasSubscript,"*\" Palisade Jan, official\""); parse(text=color1)
color2 <- paste(gasSubscript,"*\" Palisade Jan, w/ Pal. Mar params\""); parse(text=color2)
color3 <- paste(gasSubscript,"*\" Palisade Jan, w/ Pal. Jan params\""); parse(text=color3)
color4 <- paste(gasSubscript,"*\" Palisade Jan, w/ Gln. Mar params\""); parse(text=color4)


pl.Pal1 <- ggplot(data = Pal.combined1, aes(x=time)) +
  geom_line(aes(y = Pal.O3, color = color1)) +
  geom_line(aes(y = Pal1results.Pal1params, color = color2)) +
  geom_line(aes(y = Pal1results.Pal2params, color = color3)) +
  geom_line(aes(y = Pal1results.Gln1params,  color = color4)) +
  labs(title = parse(text = mainTitle),x="Time", y=parse(text = gasSubscript)) +
  theme(legend.position="bottom") +
  scale_color_manual("",breaks = c(color1, color2, color3, color4), 
                     limits = c(color1, color2, color3, color4),
                     values = c("red", "blue", "gray","green"),
                     guide = guide_legend(title = waiver(),  nrow = 2),
                     labels = function(x) parse(text=x))

pl.Pal1

#---- Glenwood plot
i <- 3 # Glenwood Mar
gasEst <- paste(gasSubscript,"_est",sep = "")
gasEstOrg <- paste(gasSubscript,"_est_org",sep = "")
gasStation <- paste(aqObs$cityAbr[i],gas,sep = ".")
temp <- as.character(aqObs$city[i])
mainTitle <- paste(gasSubscript,"*\"  official and AQE estimates for \"", "*\"",aqObs$city[i],"\"","*\"" , aqObs$shortDates[i],"\""); parse(text=mainTitle)
color1 <- paste(gasSubscript,"*\" Glenwood Mar, official\""); parse(text=color1)
color2 <- paste(gasSubscript,"*\" Glenwood Mar, w/ Pal. Mar params\""); parse(text=color2)
color3 <- paste(gasSubscript,"*\" Glenwood Mar, w/ Pal. Jan params\""); parse(text=color3)
color4 <- paste(gasSubscript,"*\" Glenwood Mar, w/ Gln. Mar params\""); parse(text=color4)

pl.Gln1 <- ggplot(data = Gln.combined1, aes(x=time)) +
  geom_line(aes(y = Gln.O3, color = color1)) +
  geom_line(aes(y = Gln1results.Pal2params, color = color2)) +
  geom_line(aes(y = Gln1results.Pal1params, color = color3)) +
  geom_line(aes(y = Gln1results.Gln1params,  color = color4)) +
  labs(title = parse(text = mainTitle),x="Time", y=parse(text = gasSubscript)) +
  theme(legend.position="bottom") +
  scale_color_manual("",breaks = c(color1, color2, color3, color4), 
                     limits = c(color1, color2, color3, color4),
                     values = c("red", "blue", "gray","green"),
                     guide = guide_legend(title = waiver(),  nrow = 2),
                     labels = function(x) parse(text=x))
pl.Gln1


