### stationarity analysis

# set seed
set.seed(42)

# disable scientific notation
options(scipen = 999)

# load packages
library(move)
library(geosphere)
library(ggplot2)
library(leaflet)
library(magrittr)
library(htmlwidgets)
library(htmltools)

# select last n days to be analyzed (minimum 28)
last_n_days <- 365

# select species or test data
species <- "buffalo"
# species <- "bat"
# species <- "goose"
# test_data <- "greylgeese"
# test_data <- "whitefgeese"
# test_data <- "stork"
# test_data <- "goat"

# select file
if (species == "buffalo") {
  file <- "Kruger African Buffalo, GPS tracking, South Africa.rds"
} else if (species == "bat") {
  file <- "Straw-colored fruit bats (Eidolon helvum) in Africa 2009-2014.rds"
} else if (species == "goose") {
  file <- "Migration timing in white-fronted geese (data from Klzsch et al. 2016).rds"
} else if (test_data == "greylgeese") {
  file <- "input1_greylgeese.rds"
} else if (test_data == "whitefgeese") {
  file <- "input2_whitefgeese.rds"
} else if (test_data == "stork") {
  file <- "input3_stork.rds"
} else if (test_data == "goat") {
  file <- "input4_goat.rds"
} else {
  print("Selected species or test data not available.")
}

# read data
data <- readRDS(paste0("../data/", file))

# transform movestack to dataframe
data <- as.data.frame(data)

# cast tag.local.identifier to character
data$tag.local.identifier <- as.character(data$tag.local.identifier)

# make sure right coordinates are used
if ("coords.x1" %in% names(data)) {
  data$location.lat <- data$coords.x1
}

if ("coords.x2" %in% names(data)) {
  data$location.long <- data$coords.x2
}

# get dimensions and individuals
dim(data)
individuals <- unique(data$tag.local.identifier)
length(individuals)

# create year and date columns
data$date <- as.Date(format(data$timestamps, format = "%Y-%m-%d"))
data$year <- as.integer(format(data$timestamps, format = "%Y"))

# get number of observations per year
aggregate(cbind(count = year) ~ year, data = data, FUN = function(x){NROW(x)})

# get min and max date
min(data$date)
max(data$date)

# create empty dataframe to store processed individual data
processed_data <- data.frame(matrix(ncol = 5, nrow = 0))
processed_data_columns <- c("tag.local.identifier", "timestamps", "location.long", "location.lat", "date")
colnames(processed_data) <- processed_data_columns

# assume that a tag's stationarity is definite and thus only use last n days of observations per individual
for(individual in individuals) {
  
  # filter data based on individual
  individual_data <- data[data$tag.local.identifier == individual, ]
  
  # subset data to relevant columns
  individual_data <- individual_data[ , processed_data_columns]
  
  # drop rows with missing values
  individual_data <- na.omit(individual_data)
  
  # drop duplicated rows
  individual_data <- individual_data[!duplicated(individual_data[c("tag.local.identifier", "timestamps")]), ]
  
  # extract max and min date
  max_date <- max(individual_data$date)
  min_date <- max_date - last_n_days
  
  # filter data based on date range
  individual_data <- individual_data[(individual_data$date >= min_date) & (individual_data$date <= max_date), ]
  
  # append processed data to existing dataframe
  processed_data <- rbind(processed_data, individual_data)
  
}

# remove data that is not needed anymore
rm(data, individual_data)
gc()

# order data
processed_data <- processed_data[order(processed_data$tag.local.identifier, processed_data$timestamps), ]

# create lag columns
processed_data$tag.local.identifier.lag <- c(NA, head(processed_data$tag.local.identifier, -1))
processed_data$location.long.lag <- c(NA, head(processed_data$location.long, -1))
processed_data$location.lat.lag <- c(NA, head(processed_data$location.lat, -1))

processed_data$tag.local.identifier.lag <- ifelse(processed_data$tag.local.identifier == processed_data$tag.local.identifier.lag,
                                                  processed_data$tag.local.identifier.lag,
                                                  NA)

processed_data$location.long.lag <- ifelse(processed_data$tag.local.identifier == processed_data$tag.local.identifier.lag,
                                           processed_data$location.long.lag,
                                           NA)

processed_data$location.lat.lag <- ifelse(processed_data$tag.local.identifier == processed_data$tag.local.identifier.lag,
                                          processed_data$location.lat.lag,
                                          NA)

# calculate distance between two location measurements
calculate_distance_in_meters_between_coordinates <- function(lon_a, lat_a, lon_b, lat_b) {
  
  if(anyNA(c(lon_a, lat_a, lon_b, lat_b))) return(NA)
  
  distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)

}

processed_data$distance_meters <- mapply(lon_a = processed_data$location.long,
                                         lat_a = processed_data$location.lat,
                                         lon_b = processed_data$location.long.lag,
                                         lat_b = processed_data$location.lat.lag,
                                         FUN = calculate_distance_in_meters_between_coordinates)

# aggregate distances by time interval and individual
data_aggregated <- aggregate(distance_meters ~ date + tag.local.identifier, data = processed_data, FUN = sum)

# get number of aggregated observations per individual
aggregate(cbind(count = tag.local.identifier) ~ tag.local.identifier, data = data_aggregated, FUN = function(x){NROW(x)})

# select individual
individual <- sample(individuals, 1)

# plot timeseries for selected individual
data_to_plot <- data_aggregated[data_aggregated$tag.local.identifier == individual, ]
start_date <- min(data_to_plot$date)
end_date <- max(data_to_plot$date)

if (dim(data_to_plot)[1] > 31) {
  scale <- "1 week"
} else {
  scale <- "1 day"
}

ggplot(data_to_plot, aes(x = date, y = distance_meters, group = 1)) +
  geom_line(linewidth = 0.75) +
  scale_x_date(breaks = seq(start_date, end_date, by = scale)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggtitle(paste0("Distance in meters moved per day for individual ", individual, " between ", start_date, " and ", end_date))

# plot last coordinates for selected individual
lon <- tail(processed_data[processed_data$tag.local.identifier == individual, ], 1)$location.long
lat <- tail(processed_data[processed_data$tag.local.identifier == individual, ], 1)$location.lat

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 24px;
  }
"))

title <- tags$div(
  tag.map.title, HTML(paste0("Last location of individual ", individual, " on ", end_date))
)  

leaflet() %>% 
  addTiles() %>% 
  setView(lng = lon,
          lat = lat,
          zoom = 6) %>% 
  addCircleMarkers(lng = lon,
                   lat = lat,
                   label = paste0("lon: ", lon, "; lat: ", lat),
                   color = "red") %>%
  addControl(title, position = "topleft", className = "map-title")

# create empty dataframe to store movement summary data
movement_summary <- data.frame(matrix(ncol = 5, nrow = 0))
movement_summary_columns <- c("individual", "last_n_days", "number_observations", "total_distance", "mean_distance")
colnames(movement_summary) <- movement_summary_columns

# compute summary statistics for last n days per individual
for(individual in individuals) {
  
  for (last_n_days in c(1, 3, 7, 14, 21, 28)) {
    
  # filter data based on individual
  individual_data_aggregated <- data_aggregated[data_aggregated$tag.local.identifier == individual, ]
  
  # get max date
  max_date <- max(individual_data_aggregated$date)
  
  # filter data based on date
  individual_data_aggregated_filtered <- individual_data_aggregated[individual_data_aggregated$date > max_date - last_n_days, ]
  
  # create empty dataframe to store individual summary data
  individual_movement_summary <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(individual_movement_summary) <- movement_summary_columns
  individual_movement_summary[1, 1] = individual
  individual_movement_summary[1, 2] = last_n_days
  
  # compute summary statistics
  individual_movement_summary[1, 3] = dim(individual_data_aggregated_filtered)[1]
  individual_movement_summary[1, 4] = round(sum(individual_data_aggregated_filtered$distance_meters), 2)
  individual_movement_summary[1, 5] = round(mean(individual_data_aggregated_filtered$distance_meters), 2)
  
  # append individual movement summary data to existing dataframe
  movement_summary <- rbind(movement_summary, individual_movement_summary)
    
  }
  
}
