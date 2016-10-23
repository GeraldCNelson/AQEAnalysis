SensorTechOzoneDataTest <- read.csv("~/Documents/EggSoftware/data/SensorTechOzoneDataTest.csv")
View(SensorTechOzoneDataTest)
STdata <- SensorTechOzoneDataTest[1:6]
STdata[STdata == 0] <- NA
STdata$Rs_MiCS <- STdata$Rs_MiCS *1000
fit <- lm(STdata$O3_UV ~ Rs_MiCS + I(Rs_MiCS^2) + I(Rs_MiCS^3) + Temp + RH, data=STdata)
summary(fit)
plot(fit)

