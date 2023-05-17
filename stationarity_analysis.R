### stationarity analysis

# disable scientific notation
options(scipen = 999)

# load packages
library(readr)
library(geosphere)
library(ggplot2)

# specify relevant columns (note that . is automatically replaced with - for column names in readr.read_csv())
id_col <- "tag-local-identifier"
time_col <- "timestamp"
lon_col <- "location-long"
lat_col <- "location-lat"

# read data
file <- "LifeTrack White Stork SW Germany.csv"
data <- read_csv(paste0("../data/", file), col_select = c(id_col, time_col, lon_col, lat_col))
dim(data)

# rename columns
names(data)[names(data) == id_col] <- "id"
names(data)[names(data) == time_col] <- "time"
names(data)[names(data) == lon_col] <- "lon"
names(data)[names(data) == lat_col] <- "lat"

# check dimensions and unique ids
dim(data)
length(unique(data$id))

# get number of observations per year
data$year <- as.integer(format(data$time, format = "%Y"))
aggregate(cbind(count = year) ~ year, data = data, FUN = function(x){NROW(x)})

# filter data by year
year <- 2022
data <- data[data$year == year, ]
dim(data)
length(unique(data$id))

# get number of observations per id
aggregate(cbind(count = id) ~ id, data = data, FUN = function(x){NROW(x)})

# check for missing values
colSums(is.na(data))

# get number of missing values per id
aggregate(cbind(count = id) ~ id, data = data[rowSums(is.na(data)) > 0, ], FUN = function(x){NROW(x)})

# drop duplicated rows
data <- data[!duplicated(data[c("id", "time")]), ]
dim(data)
length(unique(data$id))

# order data
data <- data[order(data$id, data$time), ]

# create lag columns
data$id_lag <- c(NA, head(data$id, -1))
data$lon_lag <- c(NA, head(data$lon, -1))
data$lat_lag <- c(NA, head(data$lat, -1))

data$id_lag <- ifelse(data$id == data$id_lag,
                      data$id_lag,
                      NA)

data$lon_lag <- ifelse(data$id == data$id_lag,
                       data$lon_lag,
                       NA)

data$lat_lag <- ifelse(data$id == data$id_lag,
                       data$lat_lag,
                       NA)

# calculate distance between two measurements
calculate_distance_in_meters_between_coordinates <- function(lon_a, lat_a, lon_b, lat_b) {
  if(anyNA(c(lon_a, lat_a, lon_b, lat_b))) return(NA)
  distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)
  }

data$distance_meters <- mapply(lon_a = data$lon,
                               lat_a = data$lat,
                               lon_b = data$lon_lag,
                               lat_b = data$lat_lag,
                               FUN = calculate_distance_in_meters_between_coordinates)

# extract hour and day from time column
data$day_hour <- droplevels(cut(data$time, breaks = "hour"))
data$day <- droplevels(cut(data$time, breaks = "day"))

# aggregate distances by time interval
data_agg_id_day_hour <- aggregate(distance_meters ~ day_hour + id, data = data, FUN = sum)
data_agg_id_day <- aggregate(distance_meters ~ day + id, data = data, FUN = sum)

length(unique(data_agg_id_day_hour$id))
length(unique(data_agg_id_day$id))

# plot aggregated distances by time interval
data_agg_id_day$day <- as.Date(data_agg_id_day$day)
ggplot(data_agg_id_day, aes(x = day, y = distance_meters, group = 1)) +
  geom_line(size = 0.75) +
  facet_wrap(id~., switch = "y", ncol = 1) +
  scale_x_date(breaks = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "1 month"), date_labels = "%b") +
  ggtitle(paste0("Distance in meters moved per day for year = ", year))

# check out individual ids
id <- 3921
ggplot(data_agg_id_day[data_agg_id_day$id == id, ], aes(x = day, y = distance_meters, group = 1)) +
  geom_line(size = 0.75) +
  scale_x_date(breaks = seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by = "1 month"), date_labels = "%b") +
  ggtitle(paste0("Distance in meters moved per day for id = ", id, " and year = ", year))



# take last n months per individual and calculate total and mean distance moved per set time interval
# compare distance in last n-m months to last n months
# movement in last time interval (e. g. 1 day) compared to average movement in last time interval * m but without outliers (95%)
# identify stationary tags
# plot stationary tags on map