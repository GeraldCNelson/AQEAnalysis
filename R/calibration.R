library(data.table)
newNames.o3no2 <- c("time", "temp", "humidity", "no2", "o3", "no2_we_v", "no2_aux_v", "o3_v",  
                    "lat", "long", "altitude")
pitkinData <- data.table::fread("~/Documents/workspace/AQEAnalysis/data-raw/PowellPikten20160908/GeraldNelson_GJShelter_0908_09102016_preliminary.csv",
                       colClasses = c("POSIXct","numeric"), col.names = c("DateTime","o3.pitken"))
pitkinData[, DateTime := as.POSIXct(DateTime, format = "%d-%b-%Y %H:%M", tz = "America/Denver", usetz = TRUE)]
pitkinData[, o3.pitken := o3.pitken * 1000]

eggData <- data.table::fread("~/Documents/workspace/AQEAnalysis/data-raw/PowellPikten20160908/egg00802aaa019b0111.csv",
                             na.strings = "---")
setnames(eggData, old = names(eggData), new = newNames.o3no2)
# convert from 1970 to current time of data collection by adding seconds
eggData[, time := as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S", tz = "GMT")][,time := time + 17052.92 * 24 * 60 * 60]
varsToAgg <-  c("temp", "humidity", "no2", "o3", "no2_we_v", "no2_aux_v", "o3_v")
fileOut.min <- eggData[, lapply(.SD, mean, na.rm = TRUE ), by = list(cut.POSIXt(eggData$time, breaks = "min")), .SDcols = varsToAgg]
fileOut.min[, "time" := as.POSIXct(cut.POSIXt,format = "%Y-%m-%d %H:%M", tz = "GMT")]
fileOut.min[,cut.POSIXt := NULL]
fileOut.min[, time := as.POSIXct(time,format = "%Y-%m-%d %H:%M")]
fileOut.min[, time := as.POSIXct(format(fileOut.min$time, tz = "America/Denver", usetz = TRUE))]
#drop first 15 minutes
fileOut.min <- fileOut.min[-(1:15),]
merged <- merge(pitkinData, fileOut.min, by.x = "DateTime", by.y = "time")
merged <- na.omit(merged)
#---- absolute humidity equation, from https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/ 
merged[,humidity := 6.112 * exp((17.67 * temp/(temp + 243.5)) * humidity * 2.1674)/(273.15 + temp)]

par(mfrow = c(6,1), mar = c(0.6, 5.1, 1, 0.6), oma = c(3.1, 0, 1, 0))
plot(o3.pitken ~ DateTime, data = merged, type = "l", xaxt = "n", ylab = "ppb", xlab = "", main = "Ozone, Pitken Shelter")
plot(no2 ~ DateTime, data = merged, type = "l", xaxt = "n", ylab = "ppb", xlab = "", main = "NO2 from egg")
plot(no2_we_v ~ DateTime, data = merged, type = "l", xaxt = "n", ylab = "v", xlab = "", main = "no2_we_v from egg")
plot(no2_aux_v ~ DateTime, data = merged, type = "l", xaxt = "n", ylab = "v", xlab = "", main = "no2_aux_v from egg")
plot(o3_v ~ DateTime, data = merged, type = "l", xaxt = "n", ylab = "v", xlab = "", main = "Ozone volts from egg")

ticks <- seq(from=min(merged$DateTime), by = '6 hours', length=15)
lbl <- strftime(ticks, format = "%b %d %H:00")
axis(side=1, outer=TRUE, at = ticks, labels = lbl, cex.axis = 0.7)

fit <- lm(o3.pitken ~ 
            o3_v + no2_aux_v + no2_we_v +
            temp + humidity, 
          data = merged)
summary(fit)

resid(fit) #List of residuals
plot(density(resid(fit)))

merged[, no2Combined := no2_we_v - no2_aux_v]
fit2 <- lm(o3.pitken ~ 
            o3_v + no2Combined +
            temp + humidity, 
          data = merged)
summary(fit2)
plot(resid(fit2), type = "l")

#resid(fit) #List of residuals
plot(density(resid(fit2)))

fit3 <- lm(formula = o3_v ~ no2Combined + temp, 
   data = merged)
summary(fit3)

#temp effects on aux
fit4 <- lm(formula = no2_aux_v ~  temp + humidity, 
           data = merged)
summary(fit4)

fit5 <- lm(formula = no2_we_v ~  temp + humidity, 
           data = merged)
summary(fit5)

# try out some of these coefficients on a different data set.
intercept = summary(fit2)$coefficients[1,1]
coef.o3_v = summary(fit2)$coefficients[2,1]
coef.no2combined = summary(fit2)$coefficients[3,1]
coef.temp = summary(fit2)$coefficients[4,1]
coef.humidity = summary(fit2)$coefficients[5,1]

test.data <- as.data.table(read.csv("~/Documents/workspace/AQEAnalysis/data-raw/DeerPark20160918/egg00802aaa019b0111.csv",
                   na.strings = "---"))
setnames(test.data, old = names(test.data), new = newNames.o3no2)
test.data[, no2Combined := no2_we_v - no2_aux_v]
test.data[, o3.est := intercept + coef.o3_v * o3_v + coef.no2combined * no2Combined + coef.temp * temp + coef.humidity * humidity]
plot(test.data$o3.est, type = "l")
