### stationarity analysis

# disable scientific notations
options(scipen = 999)

# load packages
library(geosphere)
library(ggplot2)

# read raw data
file <- "LifeTrack White Stork SW Germany SUBSET.csv"
raw_data <- read.csv(file)
dim(raw_data)

# set relevant columns
id_col <- "tag.local.identifier"
time_col <- "timestamp"
lon_col <- "location.long"
lat_col <- "location.lat"

# subset raw data and rename columns
preprocessed_data <- subset(raw_data, select = c(id_col, time_col, lon_col, lat_col))
names(preprocessed_data) <- c("id", "time", "lon", "lat")

dim(preprocessed_data)
length(unique(preprocessed_data$id))

# get number of observations per year
preprocessed_data$year <- substr(preprocessed_data$time, 1, 4)
aggregate(cbind(count = year) ~ year, data = preprocessed_data, FUN = function(x){NROW(x)})

# filter data by year
year <- "2022"
preprocessed_data <- preprocessed_data[preprocessed_data$year == year,]

# create lag columns
preprocessed_data$id_lag <- c(NA, head(preprocessed_data$id, -1))
preprocessed_data$lon_lag <- c(NA, head(preprocessed_data$lon, -1))
preprocessed_data$lat_lag <- c(NA, head(preprocessed_data$lat, -1))

preprocessed_data$id_lag <- ifelse(preprocessed_data$id == preprocessed_data$id_lag,
                                   preprocessed_data$id_lag,
                                   NA)

preprocessed_data$lon_lag <- ifelse(preprocessed_data$id == preprocessed_data$id_lag,
                                    preprocessed_data$lon_lag,
                                    NA)

preprocessed_data$lat_lag <- ifelse(preprocessed_data$id == preprocessed_data$id_lag,
                                    preprocessed_data$lat_lag,
                                    NA)

# calculate distance between two measurements
calculate_distance_in_meters_between_coordinates <- function(lon_a, lat_a, lon_b, lat_b) {
  if(anyNA(c(lon_a, lat_a, lon_b, lat_b))) return(NA)
  distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)
  }

preprocessed_data$distance_meters <- mapply(lon_a = preprocessed_data$lon,
                                            lat_a = preprocessed_data$lat,
                                            lon_b = preprocessed_data$lon_lag,
                                            lat_b = preprocessed_data$lat_lag,
                                            FUN = calculate_distance_in_meters_between_coordinates)

# aggregate distances by time interval
preprocessed_data$time <- as.POSIXct(preprocessed_data$time, format="%Y-%m-%d %H:%M:%S", tz="UTC")

preprocessed_data$day_hour <- droplevels(cut(preprocessed_data$time, breaks = "hour"))
preprocessed_data$day <- droplevels(cut(preprocessed_data$time, breaks = "day"))

data_agg_id_day_hour <- aggregate(distance_meters ~ day_hour + id, data = preprocessed_data, FUN = sum)
data_agg_id_day <- aggregate(distance_meters ~ day + id, data = preprocessed_data, FUN = sum)

length(unique(data_agg_id_day_hour$id))
length(unique(data_agg_id_day$id))

# plot aggregated distances by time interval
unique(data_agg_id_day$id)
id <- 3264

ggplot(data_agg_id_day[data_agg_id_day$id == id,], aes(day, distance_meters, group = 1)) +
  theme_bw() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(paste0("Distance in meters moved per day for id ", id, " and year ", year, "\n(location measurement every 5 minutes)"))
  


# identify stationary tags
# plot stationary tags on map