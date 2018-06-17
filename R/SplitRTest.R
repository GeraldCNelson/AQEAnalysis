library(SplitR)
library(magrittr)
if (!grepl("SplitR", getwd())) {
newWD <- paste(getwd(), "SplitR", sep = "/")
setwd(newWD)
}
# Run the Hysplit model, determining the locations
# of air masses going back in time 3 days (72 h) from a
# site located in Grand Junction, CO
wind_trajectory_back_3d <-
  hysplit_trajectory(
    lat = 39.105,       # the latitude in decimal degrees
    lon = -108.7,    # the longitude in decimal degrees
    height = 10,      # site is 10 m above ground level
    duration = 72,  # model run is 96 h
    direction = "back",        # the trajectory goes back in time
    met_type = "reanalysis",      # the meteorology data to use
    run_period = c("2016-06-01", "2016-06-03"),            
    daily_hours = 0, #,     # the run will start at 00:00 UTC
    met_dir = "/Users/gcn/Documents/workspace/AQEAnalysis/SplitR/metData")
#    return_met_along_traj = TRUE) # provides extra met data to df

trajectory_model <-
  create_traj_model() %>%
  add_grid(
    lat = 39.105,
    lon = -108.7,
    range = c(0.8, 0.8),
    division = c(0.2, 0.2)) %>%
  add_params(
    height = 10,
    duration = 6,
    run_period = "2016-06-01",
    daily_hours = c(0, 12),
    direction = "backward",
    met_type = "reanalysis",
    met_dir = "/Users/gcn/Documents/workspace/AQEAnalysis/SplitR/metData"
  ) %>%
  run_model()

trajectory_df <- trajectory_model %>% get_output_df()

trajectory_plot(wind_trajectory_back_5d)

trajectory_model %>% trajectory_plot()
