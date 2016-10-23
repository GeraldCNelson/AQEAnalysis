# assumes zip file is in the data_raw directory
# writes png files to the results directory
library(data.table)
library(plyr)
library(ggplot2)
library(car)
library(grid)
library(gridExtra)
zipFile <- "54f0350f-fdd0-fdf8-3663-b669261406c7.zip"
timezone <- "America/Denver"
# Other timezone codes at https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
fileList <-
  unzip(paste("data_raw/", zipFile, sep = ""), list = TRUE)
# counters to keep track of specific egg types
v2a <- v2b <- v2c <- 0
# for loop over the number of files in the zip file.
for (i in 1:length(fileList)) {
  csvFileName <- fileList$Name[i]
  eggSerialNumber <- gsub(".csv", "", csvFileName)
  print(eggSerialNumber)
  csvFile <-
    unzip(
      zipfile = paste("data_raw/", zipFile, sep = ""),
      files = csvFileName,
      exdir = "data_raw/"
    )
  df.AQE <- read.csv(file = csvFile,
                     header = TRUE,
                     stringsAsFactors = FALSE,
  )
  
  # find out what kind of v2 egg it is
  if ("pm.ug.m.3." %in% colnames(df.AQE)) {
    eggType <- "AQEV2c"
    v2c <- v2c + 1
    colnames(df.AQE) <-
      c(
        "time_GMT",
        "temp_C",
        "humidity_percent",
        "pm_mgram_per_m3",
        "pm_v",
        "latitude_deg",
        "longitude_deg",
        "altitude_m"
      )
  }
  if ("so2.ppb." %in% colnames(df.AQE)) {
    eggType <- "AQEV2b"
    v2b <- v2b + 1
    
    colnames(df.AQE) <-
      c(
        "time_GMT",
        "temp_C",
        "humidity_percent",
        "so2_ppb",
        "o3_ppb",
        "so2_v",
        "o3_v",
        "latitude_deg",
        "longitude_deg",
        "altitude_m"
      )
  }
    if ("no2.ppb." %in% colnames(df.AQE)) {
      eggType <- "AQEV2a"
      v2a <- v2a + 1
      
      colnames(df.AQE) <-
        c(
          "time_GMT",
          "temp_C",
          "humidity_percent",
          "no2_ppb",
          "co_ppm",
          "no2_v",
          "co_v",
          "latitude_deg",
          "longitude_deg",
          "altitude_m"
        )
  }
  
  df.AQE$time_local <-
    as.POSIXct(df.AQE$time_GMT, format = "%Y-%m-%dT%H:%M:%S", tz = timezone)
  removeList <-
    c("time_GMT",
      "latitude.deg.",
      "longitude.deg.",
      "altitude.m.")
  df.AQE <- df.AQE[, -which(names(df.AQE) %in% removeList)]
  # average to minutes
  #colCt <- ncol(df.AQE)
  df.AQE.min <- aggregate(df.AQE[c(1:ncol(df.AQE))],
                          by = list(cut(df.AQE$time_local, breaks = "min")),
                          FUN = mean)
  df.AQE.min$Group.1 <- NULL
  # -------- plotting stuff
  timeStart <- df.AQE.min$time_local[1]
  #outputFileName <- "PalisadeAQEvoltagegraph.png"
  timeEnd <- df.AQE.min$time_local[nrow(df.AQE.min)]
  mainText <- paste(eggType, "data", timeStart, "to",
                    timeEnd, timezone, sep = " ")
  
  rect1 <-
    data.frame (
      xmin = timeStart,
      xmax = timeEnd,
      ymin = -Inf,
      ymax = Inf
    )
  rect2 <-
    data.frame (
      xmin = timeStart,
      xmax = timeEnd,
      ymin = -Inf,
      ymax = Inf
    )
  AQEplot <- function(dataIn, clr, colName) {
    return(
      ggplot(data = dataIn) +
        geom_line(aes_string(x = "time_local",
                             y = colName),
                  color = clr)
      +
        labs(x = "Time",
             y = colName) +
        #  scale_x_datetime(breaks = seq(1000,2000,200), limits = c(1000,2000)) +
        scale_y_continuous() +
        geom_rect(
          data = rect1,
          aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax
          ),
          alpha = 0.1,
          fill = "blue"
        ) +
        geom_rect(
          data = rect2,
          aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax
          ),
          alpha = 0.1,
          fill = "blue"
        ) +
        theme(plot.margin = unit(c(0.5, 0.5,-0, 0.5), "lines"))
    )
  }
  p1 <- AQEplot(dataIn = df.AQE.min,
                clr = "red",
                colName = "temp_C")
  p2 <-
    AQEplot(dataIn = df.AQE.min,
            clr = "green",
            colName = "humidity_percent")
  if (eggType %in% "AQEV2c") {
    p3 <-
      AQEplot(dataIn = df.AQE.min,
              clr = "blue",
              colName = "pm_mgram_per_m3")
    p3 <-
      p3 + geom_hline(aes(yintercept = 35),
                      colour = "black",
                      linetype = "dashed")
    p4 <- AQEplot(dataIn = df.AQE.min,
                  clr = "yellow",
                  colName = "pm_v")
    graphicFileName <- paste("results/",eggType, v2c, ".png", sep = "")
  }
  if (eggType %in% "AQEV2b") {
    p3 <- AQEplot(dataIn = df.AQE.min,
                  clr = "blue",
                  colName = "so2_ppb")
    p3 <-
      p3 + geom_hline(aes(yintercept = 75),
                      colour = "black",
                      linetype = "dashed")
    p4 <- AQEplot(dataIn = df.AQE.min,
                  clr = "yellow",
                  colName = "so2_v")
    p5 <- AQEplot(dataIn = df.AQE.min,
                  clr = "purple",
                  colName = "o3_ppb")
    p5 <-
      p5 + geom_hline(aes(yintercept = 70),
                      colour = "black",
                      linetype = "dashed")
    p6 <- AQEplot(dataIn = df.AQE.min,
                  clr = "brown",
                  colName = "o3_v")
    graphicFileName <- paste("results/",eggType, v2b, ".png", sep = "")
  }
  
  
  if (eggType %in% "AQEV2a") {
    p3 <- AQEplot(dataIn = df.AQE.min,
                  clr = "blue",
                  colName = "no2_ppb")
    p3 <-
      p3 + geom_hline(aes(yintercept = 75),
                      colour = "black",
                      linetype = "dashed")
    p4 <- AQEplot(dataIn = df.AQE.min,
                  clr = "yellow",
                  colName = "no2_v")
    p5 <- AQEplot(dataIn = df.AQE.min,
                  clr = "purple",
                  colName = "co_ppm")
    p5 <-
      p5 + geom_hline(aes(yintercept = 35),
                      colour = "black",
                      linetype = "dashed")
    p6 <- AQEplot(dataIn = df.AQE.min,
                  clr = "brown",
                  colName = "co_v")
    graphicFileName <- paste("results/",eggType, v2a, ".png", sep = "")
  }
  
  
  gp1 <- ggplot_gtable(ggplot_build(p1))
  gp2 <- ggplot_gtable(ggplot_build(p2))
  gp3 <- ggplot_gtable(ggplot_build(p3))
  gp4 <- ggplot_gtable(ggplot_build(p4))
  if (eggType %in% "AQEV2a" | eggType %in% "AQEV2b") {
    gp5 <- ggplot_gtable(ggplot_build(p5))
    gp6 <- ggplot_gtable(ggplot_build(p6))
  }
  maxWidth = unit.pmax(gp1$widths[2:3],
                       gp2$widths[2:3],
                       gp3$widths[2:3],
                       gp4$widths[2:3],
                       gp5$widths[2:3],
                       gp6$widths[2:3])
  gp1$widths[2:3] <-
    gp2$widths[2:3] <- gp3$widths[2:3] <- gp4$widths[2:3] <- maxWidth
  if (eggType %in% "AQEV2a" | eggType %in% "AQEV2b") {
    gp5$widths[2:3] <- gp6$widths[2:3] <- maxWidth
  }
  
  if (eggType %in% "AQEV2c") {
    # to create a png file, uncomment the next line and the dev.off line
    png(graphicFileName)
    grid.arrange(gp1,
                 gp2,
                 gp3,
                 gp4,
                 ncol = 2,
                 top = textGrob(mainText, gp = gpar(
                   fontsize = 10, font = 3
                 )),
                 bottom = textGrob(eggSerialNumber, gp = gpar(
                   fontsize = 8, font = 3
                 )))
    dev.off()
  } else {
    png(graphicFileName)
    grid.arrange(
      gp1,
      gp2,
      gp3,
      gp4,
      gp5,
      gp6,
      ncol = 2, nrow = 3,
      top = textGrob(mainText, gp = gpar(
        fontsize = 10, font = 3
      )),
      bottom = textGrob(eggSerialNumber, gp = gpar(
        fontsize = 8, font = 3
      ))
    )
    dev.off()
  }
}
