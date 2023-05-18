### stationarity analysis

# set seed
set.seed(42)

# disable scientific notation
options(scipen = 999)

# load packages
library(readr)
library(geosphere)
library(ggplot2)
library(leaflet)
library(magrittr)
library(htmlwidgets)
library(htmltools)

# select species
species <- "stork"
# species <- "buffalo"
# species <- "bat"

# specify file and relevant columns (note that . is automatically replaced with - for column names in readr's read_csv() function)
if (species == "stork") {
  file <- "LifeTrack White Stork SW Germany.csv"
} else if (species == "buffalo") {
  file <- "Kruger African Buffalo, GPS tracking, South Africa.csv"
} else if (species == "bat") {
  file <- "Straw-colored fruit bats (Eidolon helvum) in Africa 2009-2014.csv"
} else {
  print("Selected species not available. Please select one that is available.")
}

id_col <- "tag-local-identifier"
timestamp_col <- "timestamp"
lon_col <- "location-long"
lat_col <- "location-lat"

# read data
data <- read_csv(paste0("../data/", file), col_select = all_of(c(id_col, timestamp_col, lon_col, lat_col)))

# rename columns
names(data)[names(data) == id_col] <- "id"
names(data)[names(data) == timestamp_col] <- "timestamp"
names(data)[names(data) == lon_col] <- "lon"
names(data)[names(data) == lat_col] <- "lat"

# get dimensions and unique ids
dim(data)
individuals <- unique(data$id)
length(individuals)

# create year and date columns
data$date <- as.Date(format(data$timestamp, format = "%Y-%m-%d"))
data$year <- as.integer(format(data$timestamp, format = "%Y"))

# get number of observations per year
aggregate(cbind(count = year) ~ year, data = data, FUN = function(x){NROW(x)})

# get min and max date
min(data$date)
max(data$date)

# create empty dataframe to store processed individual data
processed_data <- data.frame(matrix(ncol = length(colnames(data)[-length(colnames(data))]), nrow = 0))
colnames(processed_data) <- colnames(data)[-length(colnames(data))]

# assume that a tag's stationarity is definite and thus only check out last n days of observations
# get observations within last n days for each individual and clean data on the way
if (species == "stork") {
  individuals <- sample(individuals, 10) # because the stork dataset is comparably large
} else {
  # do nothing and proceed
}

# set last n days
last_n_days <- 365

for(individual in individuals) {
  
  # filter data based on individual
  individual_data <- data[data$id == individual, ]
  
  # drop rows with missing values
  individual_data <- na.omit(individual_data)
  
  # drop duplicated rows
  individual_data <- individual_data[!duplicated(individual_data[c("id", "timestamp")]), ]
  
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
processed_data <- processed_data[order(processed_data$id, processed_data$timestamp), ]

# create lag columns
processed_data$id_lag <- c(NA, head(processed_data$id, -1))
processed_data$lon_lag <- c(NA, head(processed_data$lon, -1))
processed_data$lat_lag <- c(NA, head(processed_data$lat, -1))

processed_data$id_lag <- ifelse(processed_data$id == processed_data$id_lag,
                                processed_data$id_lag,
                                NA)

processed_data$lon_lag <- ifelse(processed_data$id == processed_data$id_lag,
                                 processed_data$lon_lag,
                                 NA)

processed_data$lat_lag <- ifelse(processed_data$id == processed_data$id_lag,
                                 processed_data$lat_lag,
                                 NA)

# calculate distance between two measurements
calculate_distance_in_meters_between_coordinates <- function(lon_a, lat_a, lon_b, lat_b) {
  
  if(anyNA(c(lon_a, lat_a, lon_b, lat_b))) return(NA)
  
  distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)

}

processed_data$distance_meters <- mapply(lon_a = processed_data$lon,
                                         lat_a = processed_data$lat,
                                         lon_b = processed_data$lon_lag,
                                         lat_b = processed_data$lat_lag,
                                         FUN = calculate_distance_in_meters_between_coordinates)

# aggregate distances by time interval and individual
data_agg_id_date <- aggregate(distance_meters ~ date + id, data = processed_data, FUN = sum)

# get number of aggregated observations per individual
aggregate(cbind(count = id) ~ id, data = data_agg_id_date, FUN = function(x){NROW(x)})

# select individual
id <- sample(individuals, 1)

# plot timeseries for selected individual
data_to_plot <- data_agg_id_date[data_agg_id_date$id == id, ]
start_date <- min(data_to_plot$date)
end_date <- max(data_to_plot$date)

if (dim(data_to_plot)[1] > 30) {
  scale <- "1 week"
} else {
  scale <- "1 day"
}

ggplot(data_to_plot, aes(x = date, y = distance_meters, group = 1)) +
  geom_line(linewidth = 0.75) +
  scale_x_date(breaks = seq(start_date, end_date, by = scale)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggtitle(paste0("Distance in meters moved per day for individual ", id, " between ", start_date, " and ", end_date))

# plot last coordinates for selected individual
lon <- tail(processed_data[processed_data$id == id, ], 1)$lon
lat <- tail(processed_data[processed_data$id == id, ], 1)$lat

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
  tag.map.title, HTML(paste0("Last location of individual ", id, " on ", end_date))
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

# create empty dataframe to store summary data
summary <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(summary) <- c("individual", "last_n_days", "number_observations", "total_distance", "mean_distance")

# compute summary statistics for last n days per individual
for(individual in individuals) {
  
  for (last_n_days in c(3, 7, 14, 21, 28)) {
    
  # filter data based on individual
  individual_aggregated_data <- data_agg_id_date[data_agg_id_date$id == individual, ]
  
  # get max date
  max_date <- max(individual_aggregated_data$date)
  
  # filter data based on date
  individual_aggregated_data_filtered <- individual_aggregated_data[individual_aggregated_data$date > max_date - last_n_days, ]
  
  # create empty dataframe to store individual summary data
  individual_summary_data <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(individual_summary_data) <- c("individual", "last_n_days", "number_observations", "total_distance", "mean_distance")
  individual_summary_data[1, 1] = individual
  individual_summary_data[1, 2] = last_n_days
  
  # compute summary statistics
  individual_summary_data[1, 3] = dim(individual_aggregated_data_filtered)[1]
  individual_summary_data[1, 4] = sum(individual_aggregated_data_filtered$distance_meters)
  individual_summary_data[1, 5] = mean(individual_aggregated_data_filtered$distance_meters)
  
  # append summary data to existing dataframe
  summary <- rbind(summary, individual_summary_data)
    
  }
  
}
