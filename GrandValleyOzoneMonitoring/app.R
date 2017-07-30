# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio
library(shiny)
library(openair)
library(maps)
library(data.table)
library(ggplot2)
library(leaflet)
library(xtable)
# library(dplyr) # to do %>%
# library(dtplyr)
# library(DT) # needs to come after library(shiny)
# library(shinythemes)
# library(ggiraphExtra) #to do the interactive spider graphs. As of May 27, 2017, needs to be installed with devtools::install_github('cardiomoon/ggiraphExtra')
# #library(markdown)
# require(ggiraph)
# library(RColorBrewer)
# library(sjlabelled)
options(repos = c(CRAN = "https://cran.rstudio.com"))

# rsconnect::setAccountInfo(name = 'nutrientmodeling',
#                           token='D0257883A5409984C3DC39101FAACA2E',
#                           secret='xxBEXU7/AvSa7/10LHFCU7uzXVCHiHP+T7Q8fJDB')
combined.sites <- readRDS("data/combined.sites.RDS")
locationNames.complete <- c("BangsCanyon", "BookCliffs", "Escalante", "Highline", "Ute")
locationNames.noWind <- c("DougPass", "GrandMesa")
locationNames.noT <- c("Palisade", "Pitkin")
locationNames.O3only <- c("Whitewater")
sites.all <- c(locationNames.complete, locationNames.noWind, locationNames.noT, locationNames.O3only)
sites.wind <- c(locationNames.complete, locationNames.noT)
for (i in sites.wind) {
  title = i
  ws = paste("ws_", i, sep = "")
  wd = paste("wd_", i, sep = "")
  temp <- windRose(combined.sites, ws, wd, type =  "season", main = title, key.footer = "miles/hour")
  assign(paste("rose", i, sep = "."), temp)
}
ui <- fluidPage(
  #  useShinyjs(debug = TRUE),
  mainPanel(id = "mainPanel",
            includeMarkdown("www/GVOIntro.md"),
            includeMarkdown("www/GVOwindroses.md"),
            fluidRow(
              column(width = 4, plotOutput("rose.Highline")),
              column(width = 4, plotOutput("rose.BookCliffs")),
              column(width = 4, plotOutput("rose.Escalante"))),
            fluidRow(
              column(width = 4, plotOutput("rose.BangsCanyon")),
              column(width = 4, plotOutput("rose.Pitkin")),
              column(width = 4, plotOutput("rose.Palisade"))),
            fluidRow(
              column(width = 12, plotOutput("rose.Ute"))),
            includeMarkdown("www/GVOozoneanimation.md"),
            #    uiOutput("ozoneVideo"), 
            
            #  tags$video(src = "MesaCountyOzone20170711.mp4",  type = "video/mp4",
            tags$video(src = "MesaCountyOzone20170723.mp4",  type = "video/mp4",
                       width = "1080px", height = "480px", controls = NA, autoplay = NA),
            includeMarkdown("www/GVOsitelocations.md"),
            fluidRow(width = 12, leafletOutput("ozoneSites.point", height = "800px")),
            includeMarkdown("www/GVOsummarydata.md"),
            fluidRow(width = 12, tableOutput("sumstatstable"))
  )
)

server <- function(input, output) {
  output$ozoneVideo <- renderUI({
    # tags$video(src = "MesaCountyOzone20170711.mp4",  type = "video/mp4",
    #               width = "1080px", height = "480px", controls = NA, autoplay = NA)
  })
  
  output$rose.BangsCanyon <- renderPlot({
    rose.BangsCanyon$plot
  })
  output$rose.Highline <- renderPlot({
    rose.Highline$plot
  })
  output$rose.Pitkin <- renderPlot({
    rose.Pitkin$plot
  })
  output$rose.Palisade <- renderPlot({
    rose.Palisade$plot
  })
  output$rose.Ute <- renderPlot({
    rose.Ute$plot
  })
  output$rose.BookCliffs <- renderPlot({
    rose.BookCliffs$plot
  })
  output$rose.Escalante <- renderPlot({
    rose.Escalante$plot
  }) 
  
  output$sumstatstable <- renderTable({readRDS("data/dt.sumStats")})
  
  output$ozoneSites.point <- renderLeaflet({
    ozoneSites.points <- readRDS("data/ozoneSites.points.RDS")
    # county <- as.data.table(map_data("county"))
    # #colorado <- county[region %in% "colorado"]
    # mesa <- county[region %in% "colorado" & subregion %in% "mesa"]
    # ggMesa <- ggplot() + geom_polygon(data = mesa, aes(x=long, y = lat, group = group, fill = "white")) + 
    #   coord_fixed(1.3) +
    #   theme(legend.position="none")
    # ggMesa <- ggMesa + geom_point(data = ozoneSites.points, aes(x = coords.x1, y = coords.x2), size = 2) +
    #   geom_text(data = ozoneSites.points, aes(x = coords.x1, y = coords.x2,  label = Name),hjust=0, vjust=0) +
    #   labs(title = "CDPHE Grand Valley Ozone Monitoring sites")
    # ggMesa
    GJlat = 39.07
    GJlong = -108.7
    tileTypeTopo <- "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png" # topoMap
    tileTypeOpenStreeMap <- 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
    m <- leaflet(data = ozoneSites.points) %>%
      setView(lat = GJlat, lng = GJlong, zoom = 8) %>%
      addTiles(urlTemplate = tileTypeTopo) %>%
      # Add  OpentTopoMap map tiles
      addAwesomeMarkers(lat = ~coords.x2, lng = ~coords.x1, label =  ~as.character(Name), popup = ~siteCharacteristics)
    m
  })
}

# Run the application -----
shinyApp(ui = ui, server = server)


