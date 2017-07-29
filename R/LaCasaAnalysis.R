library(readr)
library(data.table)
library(ggplot2)
library(stargazer)
library(car)
library(officer)

#' notes about CDPHE data
#' CO Trace indicates it's CO as measured by a 48i TLE. It's a different method code from the 48c. It's just higher resolution by a significant figure.
#' The units are CO Trace PPM, O3 PPM, NO, NO2, NOy PPB, RH in percent, TEMPL in degrees F. TEMPL is temperature at 2 meters. 
#' If we had TEMPU that would indicate temperature at 10 meters.
#' All of our data is MST.
#' 
#' NOy is NOx + NOz. At La Casa we measure NOy and NO directly and derive the NO2 number. That's done with a Teledyne 200E NOy analyzer. 
#' We also measure NO2 directly with a Teledyne 500 CAPS NO2 analyzer. It does get confusing.
#' The difference between a NOx analyzer and a NOy analyzer is the location of the molybdenum converter. With a standard NOx analyzer, it's in the box. With a NOy, it's at the sample inlet. That's what the NOy tower you may have noticed is for.
#' https://www3.epa.gov/ttnamti1/files/2006conference/neuschuler.pdf give some explanation of what NOy is. 

# import CDPHE data -----
#' Note that all CDPHE data are in MST. And the egg data below are also all in MST because they were not connected to the internet 
#'  during the data collection period.

library(readr)
#'LaCasa O3 and NO2
LaCasa_O3NO2 <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/NavajoDenverMar2017/LaCasa_JerryNelson_AQEgg.csv",
                              col_types = cols(
                                Date = col_character(),
                                `CO TRACE` = col_double(),
                                O3 = col_double(),
                                NO = col_double(),
                                NO2 = col_double(),
                                NOY = col_double(),
                                RH = col_double(),
                                TEMPL = col_double()
                              )))
names.new <-  c("date", "CO_LaCasa", "O3_LaCasa", "NO_LaCasa", "NO2_LaCasa", "NOY_LaCasa", "rh_LaCasa", "temp_LaCasa")
data.table::setnames(LaCasa_O3NO2, old = names(LaCasa_O3NO2), new = names.new)

#' convert units - O3 to ppb
LaCasa_O3NO2[, O3_LaCasa := O3_LaCasa * 1000]

# the next few lines convert the MST data to MT data, with daylight savings accounted for.
LaCasa_O3NO2[, date := as.POSIXct(date, format = "%d-%b-%Y %H:%M", tz = "UTC")] # report the CDPHE data as if it were in UTC
LaCasa_O3NO2[, date := date + 7 * 60 * 60] # add the number of seconds between UTC and MST; 7 hour difference
LaCasa_O3NO2[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))] # update the CDPHE data to be in MT
# convert temp from farenheit to celsius
LaCasa_O3NO2[, temp_LaCasa := (temp_LaCasa - 32) * 5/9]

#'LaCasa particulates
LaCasa_PM <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/NavajoDenverMar2017/particulates/LaCasa_JerryNelson_AQEgg_PM.csv", 
                           col_types = cols(
                                      Date = col_character(),
                                      PM10LTP = col_double(),
                                      PMFINE = col_double()
                           )))
names.new <-  c("date", "PM10_LaCasa", "PMfine_LaCasa")
data.table::setnames(LaCasa_PM, old = names(LaCasa_PM), new = names.new)
LaCasa_PM[, date := as.POSIXct(date, format = "%d-%b-%Y %H:%M", tz = "UTC")]
LaCasa_PM[, date := date + 7 * 60 * 60]
LaCasa_PM[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]

# Import egg data -----
#'ozone and NO2 eggs
# data for egg NO2O3b0153 starts one hour later than data for NO2O3b0111. I must have downloaded it incorrectly.
NO2O3b0153 <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/NavajoDenverMar2017/NO2O3/NO2O3b0153.csv",
                                    col_names = FALSE,
                                    col_types = cols(
                                      X1 = col_character(), X2 = col_number(),
                                      X3 = col_number(), X4 = col_number(),
                                      X5 = col_number(), X6 = col_number(),
                                      X7 = col_number(), X8 = col_number(),
                                      X9 = col_character(), X10 = col_character(),
                                      X11 = col_character()),
                            na = "---", skip = 1))
names.new <- c("date", "temp_b0153", "rh_b0153", "NO2_b0153", "O3_b0153", "NO2_WE_v_b0153", "NO2_Aux_v_b0153", "O3_v_b0153", "lat", "long", "alt")
data.table::setnames(NO2O3b0153, old = names(NO2O3b0153), new = names.new)
NO2O3b0153[, date := as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")]
NO2O3b0153[, date := date + 7 * 60 * 60]
NO2O3b0153[, temp_b0153 := (temp_b0153 - 32) * 5/9] #convert to celsius
NO2O3b0153[, O3_v_b0153 := O3_v_b0153 * 10]
NO2O3b0153[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]
NO2O3b0153[, c("lat", "long", "alt") := NULL]

NO2O3b0111 <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/NavajoDenverMar2017/NO2O3/NO2O3b0111.csv", 
                                     col_names = FALSE,
                                     col_types = cols(
                                       X1 = col_character(), X2 = col_number(),
                                       X3 = col_number(), X4 = col_number(),
                                       X5 = col_number(), X6 = col_number(),
                                       X7 = col_number(), X8 = col_number(),
                                       X9 = col_character(), X10 = col_character(),
                                       X11 = col_character()),
                                     na = "---", skip = 1))
names.new <- c("date", "temp_b0111", "rh_b0111", "NO2_b0111", "O3_b0111", "NO2_WE_v_b0111", "NO2_Aux_v_b0111", "O3_v_b0111", "lat", "long", "alt")
data.table::setnames(NO2O3b0111, old = names(NO2O3b0111), new = names.new)
NO2O3b0111[, date := as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")]
NO2O3b0111[, date := date + 7 * 60 * 60]
NO2O3b0111[, temp_b0111 := (temp_b0111 - 32) * 5/9]
NO2O3b0111[, O3_v_b0111 := O3_v_b0111 * 10]
NO2O3b0111[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]
NO2O3b0111[, c("lat", "long", "alt") := NULL]

# particulates egg. Note that these data are already in UTC
particulates0112 <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/NavajoDenverMar2017/particulates/particulates0112.csv",
                                           col_names = FALSE,
                                           col_types = cols(X1 = col_character(),
                                                            X2 = col_number(), X3 = col_number(), 
                                                            X4 = col_number(), X5 = col_number(), 
                                                            X6 = col_number(), X7 = col_number(), 
                                                            X8 = col_number()),
                                           na = "---", skip = 1))
names.new <- c("date", "temp_0112", "rh_0112", "PM_0112", "PM_v_0112", "lat", "long", "alt")
data.table::setnames(particulates0112, old = names(particulates0112), new = names.new)
particulates0112[, date := as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")]
# particulates0112[, date := date + 7 * 60 * 60]
particulates0112[, temp_0112 := (temp_0112 - 32) * 5/9]
particulates0112[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]
particulates0112[, c("lat", "long", "alt") := NULL]

#' assume all eggs started at the same time
# difference between particulates egg which had an internal clock and no2o3 eggs that didn't
#update date time in eggs without internal clocks. Use first observation in each data set.
timeDiff.b0111 <- as.numeric(difftime(particulates0112[1,date], NO2O3b0111[1,date], units = "secs"))
NO2O3b0111[,date := date + timeDiff.b0111]
# need to deal with missing first hour
timeDiff.b0153 <- as.numeric(difftime(particulates0112[1,date], NO2O3b0153[1,date], units = "secs"))
NO2O3b0153[,date := date + timeDiff.b0153 + 60 * 60]

#' round to nearest minute. Round converts to POSIXlt which data table doesn't like so need 
#' to convert back to as.POSIXct
particulates0112[, date := as.POSIXct(round.POSIXt(date, units = "mins"))]
NO2O3b0153[, date := as.POSIXct(round.POSIXt(date, units = "mins"))]
NO2O3b0111[, date := as.POSIXct(round.POSIXt(date, units = "mins"))]

# delete first fifteen minutes of egg data for warmup
particulates0112 <- particulates0112[!1:15]
# NO2O3b0153 <- NO2O3b0153[!1:15] commented out because this egg is already missing 1 hour of data
NO2O3b0111 <- NO2O3b0111[!1:15]

# # try adjusting the egg data by 6 hours back. Don't need to do this now that I remembered that the particulates egg was already in utc.
# NO2O3b0153[,date := date - 6 * 60 * 60]
# NO2O3b0111[,date := date - 6 * 60 * 60]
# particulates0112[,date := date - 6 * 60 * 60]

# combine all data into one data table -----
combined.eggs <- Reduce(merge,list(NO2O3b0111,particulates0112,NO2O3b0153))
combined.LaCasa <- Reduce(merge,list(LaCasa_O3NO2,LaCasa_PM))
combined <- Reduce(merge,list(combined.eggs,combined.LaCasa))

# average to every 15 minutes
varsToAve <-  names(combined)[!names(combined) %in% "date"]
combined <- combined[, lapply(.SD, mean, na.rm = TRUE ), by = list(cut.POSIXt(combined$date, breaks = "15 min")), .SDcols = varsToAve]
setnames(combined, old = "cut.POSIXt", new = "date")
combined[, date := as.POSIXct(date)]
# combined[, O3_NO2_b0153 := O3_b0153 - NO2_b0153]
# convert to long
combined.long <- data.table::melt(
  combined,
  id.vars = c("date"),
  variable.name = "variable",
  measure.vars = names(combined)[!names(combined) %in% "date"],
  value.name = "value",
  variable.factor = FALSE)

#convert rh to absolute humidity from the LaCasa data and add to combined -----
#---- absolute humidity equation, from https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/ 

combined[,ah_LaCasa := (6.112 * exp(17.67 * temp_LaCasa/(temp_LaCasa + 243.5)) * rh_LaCasa * 18.02)/(273.15 + temp_LaCasa) * 100 * 0.08314]
combined[,ah_b0111 := (6.112 * exp(17.67 * temp_b0111/(temp_b0111 + 243.5)) * rh_b0111 * 18.02)/(273.15 + temp_b0111) * 100 * 0.08314]
combined[,ah_b0153 := (6.112 * exp(17.67 * temp_b0153/(temp_b0153 + 243.5)) * rh_b0153 * 18.02)/(273.15 + temp_b0153) * 100 * 0.08314]
combined[,ah_0112 := (6.112 * exp(17.67 * temp_b0153/(temp_b0153 + 243.5)) * rh_b0153 * 18.02)/(273.15 + temp_b0153) * 100 * 0.08314]
newOrder <- c("date", sort(names(combined)[!names(combined) %in% "date"]))
data.table::setcolorder(combined, newOrder)
p <- ggplot(data = combined, aes(x = date)) +  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d")
temp <- p + geom_line(aes(y = temp_LaCasa, color = "LaCasa")) + labs(y = "temperature (C)")
temp <- temp + geom_line(aes(y = temp_0112, color = "egg, particulate"))
temp <- temp + geom_line(aes(y = temp_b0153, color = "egg, NO2O3, b0153"))
temp <- temp + geom_line(aes(y = temp_b0111, color = "egg, NO2O3, b01111"))
temp <- temp + theme(legend.position = "bottom") + guides(color = guide_legend(title = "Temperature", nrow = 2, byrow = TRUE))

rh <- p + geom_line(aes(y = rh_LaCasa, color = "LaCasa")) + labs(y = "relative humidity (%)")
rh <- rh + geom_line(aes(y = rh_0112, color = "egg, particulate"))
rh <- rh + geom_line(aes(y = rh_b0153, color = "egg, NO2O3, b0153"))
rh <- rh + geom_line(aes(y = rh_b0111, color = "egg, NO2O3, b01111"))
rh <- rh + theme(legend.position = "bottom") + guides(title = "Relative humidity", color = guide_legend(nrow = 2, byrow = TRUE))

ah <- p + geom_line(aes(y = ah_LaCasa, color = "LaCasa")) + labs(y = "Absolute humidity (grams per cubic meter)")
ah <- ah + geom_line(aes(y = ah_0112, color = "egg, particulate"))
ah <- ah + geom_line(aes(y = ah_b0153, color = "egg, NO2O3, b0153"))
ah <- ah + geom_line(aes(y = ah_b0111, color = "egg, NO2O3, b01111"))
ah <- ah + theme(legend.position = "bottom") + guides(color = guide_legend(title = "Absolute humidity", nrow = 2, byrow = TRUE))

O3 <- p + geom_line(aes(y = O3_LaCasa, color = "LaCasa")) + labs(y = "ozone (ppb)")
O3 <- O3 + geom_line(aes(y = O3_b0153, color = "egg b0153")) 
O3 <- O3 + geom_line(aes(y = O3_b0111, color = "egg b0111"))
#O3 <- O3 + geom_line(aes(y = O3_NO2_b0153, color = "O3-NO2, egg b0111"))
O3 <- O3 + theme(legend.position = "bottom") + guides(color = guide_legend(title = "O3", nrow = 2, byrow = TRUE))

O3v <- p + geom_line(aes(y = O3_v_b0153, color = "egg b0153")) + labs(y = "ozone (v times 10)")
O3v <- O3v + geom_line(aes(y = O3_v_b0111, color = "egg b0111"))
O3v <- O3v + theme(legend.position = "bottom") + guides(color = guide_legend(title = "O3 v", nrow = 2, byrow = TRUE))

O3.eggsonly <- p + geom_line(aes(y = O3_b0153, color = "egg b0153")) + labs(y = "ozone (ppb)")
O3.eggsonly <- O3.eggsonly + geom_line(aes(y = O3_b0111, color = "egg b0111"))
O3.eggsonly <- O3.eggsonly + theme(legend.position = "bottom") + guides(title = "O3", color = guide_legend(nrow = 2, byrow = TRUE))

NO2 <- p + geom_line(aes(y = NO2_LaCasa, color = "LaCasa")) + labs(y = "nitrogen dioxide (ppb)")
NO2 <- NO2 + geom_line(aes(y = NO2_b0153, color = "egg b0153"))
NO2 <- NO2 + geom_line(aes(y = NO2_b0111, color = "egg b0111"))
NO2 <- NO2 + theme(legend.position = "bottom") + guides(color = guide_legend(title = "NO2", nrow = 1, byrow = TRUE))

NO2_aux_v <- p + geom_line(aes(y = NO2_Aux_v_b0153, color = "egg b0153")) + labs(y = "nitrogen dioxide (aux voltage)")
NO2_aux_v <- NO2_aux_v + geom_line(aes(y = NO2_Aux_v_b0111, color = "egg b0111"))
NO2_aux_v <- NO2_aux_v + theme(legend.position = "bottom") + guides(color = guide_legend(title = "NO2", nrow = 1, byrow = TRUE))

NO2_WE_v <- p + geom_line(aes(y = NO2_WE_v_b0153, color = "egg b0153")) + labs(y = "nitrogen dioxide (WE voltage)")
NO2_WE_v <- NO2_WE_v + geom_line(aes(y = NO2_WE_v_b0111, color = "egg b0111"))
NO2_WE_v <- NO2_WE_v + theme(legend.position = "bottom") + guides(color = guide_legend(title = "NO2", nrow = 1, byrow = TRUE))

listOfPlots <- c("NO2_WE_v", "NO2_aux_v", "NO2", "O3.eggsonly", "O3v", "O3", "ah", "rh", "temp")
for (i in listOfPlots) {
  plotFileName <- paste(i, "png", sep = ".")
ggsave(filename = plotFileName, plot = get(i), path = "results/", device = "png")
}

particulates <- p + geom_line(aes(y = PM10_LaCasa, color = "PM10, LaCasa")) + labs(y = "particulates")
particulates <- particulates + geom_line(aes(y = PMfine_LaCasa, color = "PMfine, LaCasa"))
particulates <- particulates + geom_line(aes(y = PM_0112, color = "particulate, egg 0112"))
particulates <- particulates + theme(legend.position = "bottom") + guides(color = guide_legend(title = "Particulates", nrow = 2, byrow = TRUE))

cor(combined$temp_0112,combined$temp_LaCasa)
cor(combined$temp_b0153,combined$temp_LaCasa)
cor(combined$temp_b0111,combined$temp_LaCasa)

cor(combined$rh_0112,combined$rh_LaCasa)
cor(combined$rh_b0153,combined$rh_LaCasa)
cor(combined$rh_b0111,combined$rh_LaCasa)

cor(combined$O3_b0153,combined$O3_LaCasa)
cor(combined$O3_b0111,combined$O3_LaCasa)
cor(combined$O3_b0153,combined$O3_b0111)

cor(combined$NO2_b0153,combined$NO2_LaCasa)
cor(combined$NO2_b0111,combined$NO2_LaCasa)
cor(combined$NO2_b0153,combined$NO2_b0111)

cor(combined$PM_0112,combined$PM10_LaCasa)
cor(combined$NO2_b0111,combined$PMfine_LaCasa)
cor(combined$PM10_LaCasa,combined$PMfine_LaCasa)

# ozone with egg b0111 -----
O3only <- lm(O3_LaCasa ~ 
            O3_b0111, 
          data = combined)
combined[, O3only_b0111.fitted := fitted(O3only)]

O3vonly <- lm(O3_LaCasa ~ 
             O3_v_b0111, 
           data = combined)
combined[, O3vonly_b0111.fitted := fitted(O3vonly)]

O3vPlusLaCasaTAh <- lm(O3_LaCasa ~ 
             O3_v_b0111 + temp_LaCasa + ah_LaCasa, 
           data = combined)
combined[, O3vPlusLaCasaTAh_b0111.fitted := fitted(O3vPlusLaCasaTAh)]

O3vPlusLaCasaTAhPlusNO2 <- lm(O3_LaCasa ~ 
             O3_v_b0111 + temp_LaCasa + ah_LaCasa + NO2_b0111, 
           data = combined)
combined[, O3vPlusLaCasaTAhPlusNO2_b0111.fitted := fitted(O3vPlusLaCasaTAhPlusNO2)]

O3vPlusEggTAhPlusNO2 <- lm(O3_LaCasa ~ 
             O3_v_b0111 + temp_b0111 + ah_b0111 + NO2_b0111, 
           data = combined)
combined[, O3vPlusEggTAhPlusNO2_b0111.fitted := fitted(O3vPlusEggTAhPlusNO2)]

O3vPlusLaCasaTAhPlusNO2v <- lm(O3_LaCasa ~ 
             O3_v_b0111 + temp_LaCasa + ah_LaCasa + NO2_Aux_v_b0111, 
           data = combined)
combined[, O3vPlusLaCasaTAhPlusNO2v_b0111.fitted := fitted(O3vPlusLaCasaTAhPlusNO2v)]

O3vPlusLaCasaTAhPlusNO2V2 <- lm(O3_LaCasa ~ 
             O3_v_b0111 + NO2_Aux_v_b0111 + NO2_WE_v_b0111 +
             temp_LaCasa + ah_LaCasa, 
           data = combined)
combined[, O3vPlusLaCasaTAhPlusNO2V2_b0111.fitted := fitted(O3vPlusLaCasaTAhPlusNO2V2)]

temp <- data.table::copy(combined)
keepListCol <- c("O3_LaCasa", "O3_v_b0111", "NO2_Aux_v_b0111", "NO2_WE_v_b0111", 
  "temp_LaCasa", "ah_LaCasa", "O3vPlusLaCasaTAhPlusNO2V2_b0111.fitted")
temp <- temp[, (keepListCol), with = FALSE]

# plot residuals
plot(resid(O3only), type = "l")
plot(resid(O3vonly), type = "l")
plot(resid(O3vPlusLaCasaTAh), type = "l")
plot(resid(O3vPlusLaCasaTAhPlusNO2), type = "l")
plot(resid(O3vPlusEggTAhPlusNO2), type = "l")
plot(resid(O3vPlusLaCasaTAhPlusNO2v), type = "l")
plot(resid(O3vPlusLaCasaTAhPlusNO2V2), type = "l")

path <- getwd()
stargazer(O3only, O3vonly, O3vPlusLaCasaTAh, O3vPlusLaCasaTAhPlusNO2, O3vPlusEggTAhPlusNO2, O3vPlusLaCasaTAhPlusNO2v,O3vPlusLaCasaTAhPlusNO2V2, 
          type = "text",  align = TRUE, out = "results/O3regResults_b0111.html", out.header = TRUE, digits = 2,
          title = "Ozone results with egg b0111", omit.stat = c("LL","ser","f"))

# ozone with egg b0153 -----
O3only_b0153 <- lm(O3_LaCasa ~ 
               O3_b0153, 
             data = combined)

O3vonly_b0153 <- lm(O3_LaCasa ~ 
                O3_v_b0153, 
              data = combined)

O3vPlusLaCasaTAh_b0153 <- lm(O3_LaCasa ~ 
                         O3_v_b0153 + temp_LaCasa + ah_LaCasa, 
                       data = combined)

O3vPlusLaCasaTAhPlusNO2_b0153 <- lm(O3_LaCasa ~ 
                                O3_v_b0153 + temp_LaCasa + ah_LaCasa + NO2_b0153, 
                              data = combined)

O3vPlusEggTAhPlusNO2_b0153 <- lm(O3_LaCasa ~ 
                             O3_v_b0153 + temp_b0153 + ah_b0153 + NO2_b0153, 
                           data = combined)

O3vPlusLaCasaTAhPlusNO2v_b0153 <- lm(O3_LaCasa ~ 
                                 O3_v_b0153 + temp_LaCasa + ah_LaCasa + NO2_Aux_v_b0153, 
                               data = combined)

O3vPlusLaCasaTAhPlusNO2V2_b0153 <- lm(O3_LaCasa ~ 
                                  O3_v_b0153 + NO2_Aux_v_b0153 + NO2_WE_v_b0153 +
                                  temp_LaCasa + ah_LaCasa, 
                                data = combined)
combined[, O3vPlusLaCasaTAhPlusNO2V2_b0153.fitted := fitted(O3vPlusLaCasaTAhPlusNO2V2_b0153)]

p <- ggplot(data = combined, aes(x = date)) +  scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d")
p <- p + geom_line(aes(y = O3_LaCasa, color = "LaCasa"), size = 1) + labs(y = "ozone (ppb)")
# O3.predicted <- O3.predicted + geom_line(aes(y = O3only_b0111.fitted, color = "O3only")) 
# O3.predicted <- O3.predicted + geom_line(aes(y = O3vonly_b0111.fitted, color = "O3vonly"))
# O3.predicted <- O3.predicted + geom_line(aes(y = O3vPlusLaCasaTAh_b0111.fitted, color = "O3vPlusLaCasaTAh"))
# O3.predicted <- O3.predicted + geom_line(aes(y = O3vPlusLaCasaTAhPlusNO2_b0111.fitted, color = "O3vPlusLaCasaTAhPlusNO2"))
# O3.predicted <- O3.predicted + geom_line(aes(y = O3vPlusLaCasaTAhPlusNO2v_b0111.fitted, color = "O3vPlusLaCasaTAhPlusNO2v"))
O3.predicted <- p +            geom_line(aes(y = O3vPlusLaCasaTAhPlusNO2V2_b0111.fitted, color = "O3vPlusLaCasaTAhPlusNO2V2b0111.fitted"))
O3.predicted <- O3.predicted + geom_line(aes(y = O3vPlusLaCasaTAhPlusNO2V2_b0153.fitted, color = "O3vPlusLaCasaTAhPlusNO2V2b0153.fitted"))
O3.predicted <- O3.predicted + geom_line(linetype = "dashed",aes(y = O3vPlusLaCasaTAhPlusNO2V2_b0111.fitted - O3_LaCasa, color = "delta_b0111"))
O3.predicted <- O3.predicted + geom_line(linetype = "dashed",aes(y = O3vPlusLaCasaTAhPlusNO2V2_b0153.fitted - O3_LaCasa, color = "delta_b0153"))
O3.predicted <- O3.predicted + theme(legend.position = "bottom") + guides(color = guide_legend(title = "O3", nrow = 2, byrow = TRUE))


# plot residuals
plot(resid(O3only), type = "l")
plot(resid(O3vonly), type = "l")
plot(resid(O3vPlusLaCasaTAh), type = "l")
plot(resid(O3vPlusLaCasaTAhPlusNO2), type = "l")
plot(resid(O3vPlusEggTAhPlusNO2), type = "l")
plot(resid(O3vPlusLaCasaTAhPlusNO2v), type = "l")
plot(resid(O3vPlusLaCasaTAhPlusNO2V2), type = "l")

stargazer(O3only, O3vonly, O3vPlusLaCasaTAh, O3vPlusLaCasaTAhPlusNO2, O3vPlusEggTAhPlusNO2, O3vPlusLaCasaTAhPlusNO2v,O3vPlusLaCasaTAhPlusNO2V2, 
          type = "text",  align = TRUE, out = "results/O3regResults_b0153.html", out.header = TRUE, digits = 2,
          title = "Ozone results with egg b0153", omit.stat = c("LL","ser","f"))

# NO2 with egg b0111 -----
NO2only <- lm(NO2_LaCasa ~ 
            NO2_b0111,
          data = combined)

NO2Vsonly <- lm(NO2_LaCasa ~ 
            NO2_WE_v_b0111 + NO2_Aux_v_b0111 , 
          data = combined)

NO2VsPlusLaCasaTAh <- lm(NO2_LaCasa ~ 
                  NO2_WE_v_b0111 + NO2_Aux_v_b0111 +
                  temp_LaCasa + ah_LaCasa, 
                data = combined)

# with temp and absolute humidity from egg data
NO2VsPlusEggTAh <- lm(NO2_LaCasa ~ 
             NO2_Aux_v_b0111 + NO2_WE_v_b0111 +
            temp_b0111 + ah_b0111, 
          data = combined)

# with O3 voltage and LaCasa temp and ah
NO2VsPlusLaCasaTAhPlusO3v <- lm(NO2_LaCasa ~ 
            NO2_Aux_v_b0111 + NO2_WE_v_b0111 +
            temp_LaCasa + ah_LaCasa + O3_v_b0111, 
          data = combined)

# with O3 voltage and egg temp and ah
NO2VsPluseggTAhPlusO3v <- lm(NO2_LaCasa ~ 
                                  NO2_Aux_v_b0111 + NO2_WE_v_b0111 +
                                  temp_b0111 + ah_b0111 +O3_v_b0111, 
                                data = combined)

stargazer(NO2only, NO2Vsonly, NO2VsPlusLaCasaTAh, NO2VsPlusEggTAh, NO2VsPlusLaCasaTAhPlusO3v,NO2VsPluseggTAhPlusO3v,
          type = "text",  align = TRUE, out = "results/NO2regResults_b0111.html", out.header = TRUE, digits = 2,
          title = "NO2 results with egg b0111", omit.stat = c("LL","ser","f"))

# NO2 with egg b0153 -----
NO2only <- lm(NO2_LaCasa ~ 
                NO2_b0153,
              data = combined)

NO2Vsonly <- lm(NO2_LaCasa ~ 
                  NO2_WE_v_b0153 + NO2_Aux_v_b0153 , 
                data = combined)

NO2VsPlusLaCasaTAh <- lm(NO2_LaCasa ~ 
                           NO2_WE_v_b0153 + NO2_Aux_v_b0153 +
                           temp_LaCasa + ah_LaCasa, 
                         data = combined)

# with temp and absolute humidity from egg data
NO2VsPlusEggTAh <- lm(NO2_LaCasa ~ 
                        NO2_Aux_v_b0153 + NO2_WE_v_b0153 +
                        temp_b0153 + ah_b0153, 
                      data = combined)

# with O3 voltage and LaCasa temp and ah
NO2VsPlusLaCasaTAhPlusO3v <- lm(NO2_LaCasa ~ 
                                  NO2_Aux_v_b0153 + NO2_WE_v_b0153 +
                                  temp_LaCasa + ah_LaCasa + O3_v_b0153, 
                                data = combined)

# with O3 voltage and egg temp and ah
NO2VsPluseggTAhPlusO3v <- lm(NO2_LaCasa ~ 
                               NO2_Aux_v_b0153 + NO2_WE_v_b0153 +
                               temp_b0153 + ah_b0153 +O3_v_b0153, 
                             data = combined)

stargazer(NO2only, NO2Vsonly, NO2VsPlusLaCasaTAh, NO2VsPlusEggTAh, NO2VsPlusLaCasaTAhPlusO3v,NO2VsPluseggTAhPlusO3v,
          type = "text",  align = TRUE, out = "results/NO2regResults_b0153.html", out.header = TRUE, digits = 2,
          title = "NO2 results with egg b0153", omit.stat = c("LL","ser","f"))



