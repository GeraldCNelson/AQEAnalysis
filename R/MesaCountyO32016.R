library(readr)
library(data.table)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(openair)
library(rgdal)
library(maps)
# fips.code for Mesa County is 8077
#ozoneSites.lines <- readOGR("data-raw/MesaCty2016/Grand_Junction.kml", layer = "Grand Junction", require_geomType="wkbLineString")

BangsCanyon <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/BangsCanyon.csv"))
BangsCanyon <- BangsCanyon[1:(nrow(BangsCanyon) - 11), ] # get rid of ending rows with all NAs for data

BookCliffs <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/BookCliffs.csv"))
BookCliffs <- BookCliffs[1:(nrow(BookCliffs) - 11), ] # get rid of ending rows with all NAs for data

DougPass <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/DougPass.csv"))
Escalante <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/Escalante.csv"))
Escalante <- Escalante[1:(nrow(Escalante) - 11), ] # get rid of ending rows with all NAs for data

GrandMesa <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/GrandMesa.csv"))
Highline <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/Highline.csv"))
Highline <- Highline[1:(nrow(Highline) - 36), ] # get rid of ending rows with all NAs for data

Palisade <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/Palisade.csv",
                                   col_names = TRUE, cols(
                                     date = col_character(),
                                     wd = col_double(),
                                     ws = col_double(),
                                     O3_Avg = col_double()
                                   )))
Pitkin <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/Pitkin.csv",
                                 col_names = TRUE, cols(
                                   date = col_character(),
                                   wd = col_double(),
                                   ws = col_double(),
                                   O3_Avg = col_double()
                                 )))
Ute <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/Ute.csv",
                              col_names = TRUE, cols(
                                date = col_character(),
                                wd = col_double(),
                                ws = col_double(),
                                O3_Avg = col_double(),
                                T_Int = col_double(),
                                Flow_Avg = col_double(),
                                Cell_Pres_Avg = col_double(),
                                T_Air = col_double(),
                                T_inst = col_double(),
                                V_Batt = col_double()
                              )))

Ute <- Ute[1:(nrow(Ute) - 11), ] # get ride of ending rows with all NAs for data

Whitewater <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/GrandJunction_All/Whitewater.csv", 
                                     col_names = TRUE, cols(
                                       date = col_character(),
                                       wd = col_double(),
                                       ws = col_double(),
                                       O3_Avg = col_double()
                                     )))
Whitewater <- Whitewater[1:(nrow(Ute) - 37), ] # get rid of ending rows with all NAs for data
# Whitewater doesn't actually have any wind speed or direction information

# Powell PM 2.5 and PM 10
Powell_PM <- as.data.table(read_csv("~/Documents/workspace/AQEAnalysis/data-raw/MesaCty2016/Powell_PM_0401_09302016.csv",
                                    col_names = TRUE, cols(
                                      Date = col_character(),
                                      pm10ltp = col_double(),
                                      pmfine = col_double()
                                    )))

setnames(Powell_PM, old = c("Date", "pm10ltp", "pmfine"), new = c("date", "pm10_Powell", "pm2.5_Powell"))
Powell_PM[, date := as.POSIXct(date, format = "%d-%b-%Y %H:%M", tz = "UTC")]
Powell_PM[, date := date + 7 * 60 * 60]
Powell_PM[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]

locationNames.complete <- c("BangsCanyon", "BookCliffs", "Escalante", "Highline", "Ute")
locationNames.noWind <- c("DougPass", "GrandMesa")
locationNames.noT <- c("Palisade", "Pitkin")
locationNames.O3only <- c("Whitewater")
locationNames.particulates <- c("Powell_PM")
for (i in c(locationNames.complete)) {
  oldNames <- c("wd", "ws", "O3_Avg", "T_Air")
  temp <- get(i)
  keepListCol <- c("date", oldNames)
  temp <- temp[, (keepListCol), with = FALSE]
  data.table::setnames(temp, old = oldNames, new = paste(oldNames, i, sep = "_"))
  temp[, date := as.POSIXct(date, format = "%m/%d/%y %H:%M", tz = "UTC")]
  temp[, date := date + 7 * 60 * 60]
  temp[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]
  assign(i, temp)
}

for (i in locationNames.noT) {
  oldNames <- c("wd", "ws", "O3_Avg")
  temp <- get(i)
  keepListCol <- c("date", oldNames)
  temp <- temp[, (keepListCol), with = FALSE]
  data.table::setnames(temp, old = oldNames, new = paste(oldNames, i, sep = "_"))
  temp[, date := as.POSIXct(date, format = "%m/%d/%y %H:%M", tz = "UTC")]
  temp[, date := date + 7 * 60 * 60]
  temp[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]
  assign(i, temp)
}

for (i in locationNames.noWind) {
  oldNames <- c("O3_Avg", "T_Air")
  temp <- get(i)
  keepListCol <- c("date", oldNames)
  temp <- temp[, (keepListCol), with = FALSE]
  data.table::setnames(temp, old = oldNames, new = paste(oldNames, i, sep = "_"))
  temp[, date := as.POSIXct(date, format = "%m/%d/%y %H:%M", tz = "UTC")]
  temp[, date := date + 7 * 60 * 60]
  temp[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]
  assign(i, temp)
}

for (i in locationNames.O3only) {
  oldNames <- c("O3_Avg")
  temp <- get(i)
  keepListCol <- c("date", oldNames)
  temp <- temp[, (keepListCol), with = FALSE]
  data.table::setnames(temp, old = oldNames, new = paste(oldNames, i, sep = "_"))
  temp[, date := as.POSIXct(date, format = "%m/%d/%y %H:%M", tz = "UTC")]
  temp[, date := date + 7 * 60 * 60]
  temp[, date := as.POSIXct(format(date, usetz = TRUE, tz = "America/Denver"))]
  assign(i, temp)
}

allSites <- c(locationNames.complete, locationNames.noWind, locationNames.noT, locationNames.O3only, locationNames.particulates)
combined.sites = Reduce(function(...) merge(..., all = T), list(BangsCanyon, BookCliffs, Escalante, Highline, Ute, DougPass, GrandMesa, Palisade, Pitkin, Whitewater, Powell_PM))
setcolorder(combined.sites, c("date", "O3_Avg_BangsCanyon", "O3_Avg_BookCliffs", "O3_Avg_DougPass", "O3_Avg_Escalante", "O3_Avg_Highline",
                              "O3_Avg_GrandMesa", "O3_Avg_Palisade", "O3_Avg_Pitkin", "O3_Avg_Whitewater",  "O3_Avg_Ute", 
                              "pm10_Powell", "pm2.5_Powell",
                              "T_Air_BangsCanyon", "T_Air_BookCliffs", "T_Air_Escalante", "T_Air_Highline", "T_Air_GrandMesa", "T_Air_DougPass", "T_Air_Ute",
                              "wd_BangsCanyon", "ws_BangsCanyon", "wd_BookCliffs", "ws_BookCliffs",  "wd_Escalante", "ws_Escalante",  
                              "wd_Highline", "ws_Highline", "wd_Ute", "ws_Ute","wd_Palisade", "ws_Palisade",  "wd_Pitkin", "ws_Pitkin"))

# p <- ggplot(data = combined.sites, aes(x = date)) +  scale_x_datetime(date_breaks = "1 day", date_labels = "%m %d")
# O3 <- p + geom_line(aes(y = O3_Avg_DougPass, color = "DougPass")) + labs(y = "ozone (ppb)")
# O3 <- O3 + geom_line(aes(y = O3_Avg_Palisade, color = "Palisade")) 
# O3 <- O3 + geom_line(aes(y = O3_Avg_BangsCanyon, color = "BangsCanyon"))
# O3 <- O3 + geom_line(aes(y = O3_Avg_BookCliffs, color = "BookCliffs"))
# O3 <- O3 + geom_line(aes(y = O3_Avg_Escalante, color = "Escalante"))
# O3 <- O3 + geom_line(aes(y = O3_Avg_Highline, color = "Highline"))
# O3 <- O3 + geom_line(aes(y = O3_Avg_Ute, color = "Ute"))
# O3 <- O3 + geom_line(aes(y = O3_Avg_GrandMesa, color = "GrandMesa"))
# O3 <- O3 + geom_line(aes(y = O3_Avg_Pitkin, color = "Pitkin"))
# O3 <- O3 + geom_line(aes(y = O3_Avg_Whitewater, color = "Whitewater"))

# windrose by season
for (i in c(locationNames.complete, locationNames.noT)) {
  print(paste("working on ", i))
  title = paste(i,  'wind roses, by season')
  ws = paste("ws_", i, sep = "")
  wd = paste("wd_", i, sep = "")
  temp <- windRose(combined.sites, ws, wd, type =  "season", main = title, key.footer = "miles/hour")
  assign(paste("rose", i, sep = "."), temp)
}

# windrose by hour
for (i in c(locationNames.complete, locationNames.noT)) {
  print(paste("working on ", i))
  title = paste(i,  'wind roses, by hour')
  ws = paste("ws_", i, sep = "")
  wd = paste("wd_", i, sep = "")
  temp <- windRose(combined.sites, ws, wd, type =  "hour", main = title, key.footer = "miles/hour")
  assign(paste("rose", i, sep = "."), temp)
}

# pollutionRose by season
for (i in c(locationNames.complete, locationNames.noT)) {
  print(paste("working on ", i))
  title = paste(i,  'pollution roses, by season')
  ws = paste("ws_", i, sep = "")
  wd = paste("wd_", i, sep = "")
  pollutant = paste("O3_Avg_", i, sep = "")
  temp <- pollutionRose(combined.sites, ws = ws, wd = wd, pollutant = pollutant, type =  "season", main = title)
  #  assign(paste("rose", i, sep = "."), temp)
}

# ozone cross with particulates
rose.O3.particulates <- pollutionRose(combined.sites[!is.na(combined.sites$pm10_Powell), ], ws = "ws_Pitkin", wd = "wd_Pitkin", 
                                      pollutant = "O3_Avg_Pitkin",  type = "pm10_Powell")



# code to make animations, from http://www.animatedgraphs.co.uk

# remove extraneous columns
combined.sites[, c( "T_Air_BangsCanyon", "T_Air_BookCliffs", "T_Air_Escalante", "T_Air_Highline", "T_Air_GrandMesa", 
                    "T_Air_DougPass", "T_Air_Ute", "wd_BangsCanyon", "ws_BangsCanyon", "wd_BookCliffs", "ws_BookCliffs", "wd_Escalante", 
                    "ws_Escalante", "wd_Highline", "ws_Highline", "wd_Ute", "ws_Ute", "wd_Palisade", "ws_Palisade", "wd_Pitkin", "ws_Pitkin") := NULL]

# prepare summary statistics table
dt.sumStats <- as.data.table(do.call(cbind, lapply(combined.sites, summary)))
setnames(dt.sumStats, old = names(dt.sumStats), new = gsub("O3_Avg_", "", names(dt.sumStats)))
dt.sumStats[, date := as.POSIXct(date, origin = "1970-01-01")]
dt.sumStats[, date := as.POSIXct(date, format = "%m/%d/%y %H:%M")]
colsToRound <- names(dt.sumStats)[!names(dt.sumStats) %in% "date"]
dt.sumStats[, (colsToRound) := round(.SD,1), .SDcols = colsToRound]

timemax <- 4645 # 4645 # number of frames (and observations - so no interpolation needed)
vis <- 100 # how many time points are on the screen at one time

right.num <- (((1:timemax) <= vis) * 100) + (((1:timemax) > vis) * 1:timemax) # rightmost time on screen
left.num <- right.num - vis + 1 # leftmost time on screen
leftlab <- 20 * ceiling((left.num - 1)/20) # leftmost x label
rightlab <- 20 * floor(right.num/20) # rightmost x label
right <- as.POSIXct(combined.sites[(((1:timemax) <= vis) * 100) + (((1:timemax) > vis) * 1:timemax), date]) # rightmost time on screen
left <- as.POSIXct(combined.sites[(right.num - vis + 1), date]) # leftmost time on screen

leftlab <-  as.POSIXct(combined.sites[(20 * ceiling((left.num )/20)), date]) # leftmost x label
rightlab <- as.POSIXct(combined.sites[(20 * floor(right.num/20)), date]) # rightmost x label

# delete old frames
frameDir <- "graphics/movies/frames"
oldFrames <- list.files(path = frameDir, full.names = TRUE)
if (length(oldFrames) > 0) file.remove(oldFrames)

# create individual frames
gname <- paste("graphics/movies/frames/g", 1:timemax, ".png", sep = "") # holds the names of the picture files
for (i in 1:timemax) {
  print(paste("working on frame ", i))
  png(gname[i],width = 1080)
  plot(combined.sites$date[left[i]:right[i]],combined.sites$O3_Avg_Palisade[left[i]:right[i]],col = "white",
       type = "l", ylim = c(0,80), xlim = c(left[i], right[i]), xaxt = "n", ylab = "", xlab = "time")
  axis.POSIXct(1, at = seq(from = leftlab[i], to = rightlab[i], by = "day"), format = "%m/%d %I %p")
  
  grid(nx = NA, ny = NULL, col = "gray")
  endDateforI <- combined.sites[i, date]
  temp.date <- combined.sites[date >= left[i] & date <= endDateforI,]
  lines(temp.date$date,temp.date$O3_Avg_Palisade, col = "black")
  lines(temp.date$date,temp.date$O3_Avg_Ute, col = "red")
  lines(temp.date$date,temp.date$O3_Avg_Pitkin, col = "green")
  lines(temp.date$date,temp.date$O3_Avg_DougPass, col = "blue")
  lines(temp.date$date,temp.date$O3_Avg_Highline, col = "darkblue")
  lines(temp.date$date,temp.date$O3_Avg_BangsCanyon, col = "orange")
  lines(temp.date$date,temp.date$O3_Avg_BookCliffs, col = "yellow")
  lines(temp.date$date,temp.date$O3_Avg_Escalante, col = "brown")
  lines(temp.date$date,temp.date$O3_Avg_Whitewater, col = "purple")
  lines(temp.date$date,temp.date$O3_Avg_GrandMesa, col = "pink")
  lines(temp.date$date,temp.date$pm10_Powell, col = "brown", lty = 2)
  lines(temp.date$date,temp.date$pm2.5_Powell, col = "brown", lty = 3, lwd = 4)
  legend("topright", title = "Monitoring locations", legend = c("Palisade", "Ute", "Pitkin", "Douglas Pass", "Highline Lake", "Bangs Canyon",
                                                                "Book Cliffs","Escalante", "Whitewater", "Grand Mesa", "Powell PM10", "Powell PM2.5"), 
         col = c("black", "red", "green", "blue", "darkblue", "orange", "yellow", "brown", "purple", "pink", "brown", "brown"), 
         lty = c(1,1,1,1,1,
                 1,1,1,1,1,2,3),
         lwd = c(1,1,1,1,1,
                 1,1,1,1,1,1,4))
  title(main = "Grand Valley ozone concentrations (ppb) and particulates (µg/m3) at the Powell Building ", sub = "Start date and time: April 1, 2016, 1 AM MDT")
  dev.off(dev.cur())
}

# call FFMPEG and make the video
infile <- "/Users/gcn/Documents/workspace/AQEAnalysis/graphics/movies/frames/g%d.png "
#outFileOptions <- " -b:v 64k -r 24 -ar 22050 -ab 96k "
outFileOptions <- " -b:v 96k -vcodec libx264 -crf 25 -pix_fmt yuv420p "
outfile <- paste("/Users/gcn/Documents/workspace/AQEAnalysis/graphics/movies/MesaCountyOzone", Sys.Date(), ".mp4", sep = "")

# delete movie file created earlier inthe day. Otherwise the system command hangs.
movieDir <- "graphics/movies"
oldMovies <- list.files(path = movieDir, full.names = TRUE)
if (outfile %in% paste(getwd(),oldMovies, sep = "/")) file.remove(outfile)

runffmpegcommand <- paste("ffmpeg -report", " -i ", infile, outFileOptions, outfile, sep = "")
system(runffmpegcommand)

county <- as.data.table(map_data("county"))
#colorado <- county[region %in% "colorado"]
mesa <- county[region %in% "colorado" & subregion %in% "mesa"]
#mesa <- mesa[lat > 38.8]
ozoneSites.points <- as.data.table(readOGR("data-raw/MesaCty2016/Grand_Junction.kml", layer = "Grand Junction", require_geomType="wkbPoint"))


ggMesa <- ggplot() + geom_polygon(data = mesa, aes(x=long, y = lat, group = group, fill = "white")) + 
  coord_fixed(1.3) +
  theme(legend.position="none")
ggMesa <- ggMesa + geom_point(data = ozoneSites.points, aes(x = coords.x1, y = coords.x2), size = 2) +
  geom_text(data = ozoneSites.points, aes(x = coords.x1, y = coords.x2,  label = Name),hjust=0, vjust=0, nudge_x = .05) +
  labs(title = "CDPHE Grand Valley Ozone Monitoring sites")
ggMesa



