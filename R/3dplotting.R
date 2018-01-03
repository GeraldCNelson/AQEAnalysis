library(ggplot2)
library(ggmap)
library(maptools)
library(raster)
library(rgdal)
library(akima)
library(plotly)

x <- c(-108.6125,-108.5114,-108.805,-108.4014,-108.5615,-108.8349,-108.225,-108.3139,-108.5568,-108.4968)
y <- c(39.02205,39.22255,39.598,38.89478,39.06429,39.27625,39.03,39.1306,39.14823,38.89795)
z <- c(60.7735,56.45783,49.65,60.15,50,53.95417,50.825,56,55.843,38.73333)
df <- data.frame(x = x,y = y, z = z)

xo = seq(from = min(x), to = max(x), by = (max(x) - min(x))/9)
yo = seq(from = min(y), to = max(y), by = (max(y) - min(y))/9)
s <- interpp(x = df$x, y = df$y, z = df$z, xo = xo, yo = yo,  linear = FALSE, extrap = TRUE)
ss <- as.data.frame(s)
p <- plot_ly(data = data.frame(s), x = ~x, y = ~y, z = ~z) %>% add_surface()
p

# get google map of the grand valley
loc <- c(-109, 38.8, -108, 39.8)
tx_map_gmaps <- get_map(location=loc, zoom = 9, source="google", maptype="terrain")
gg <- ggmap(tx_map_gmaps)
gg
