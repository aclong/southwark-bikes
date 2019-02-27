#map out the different count locations

#there is a json file with all the locations - use that to create a spatial points dataframe with the count info.

#load in the libraries
library(tmap)
library(geojsonio)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(classInt)

#load in the location geojson
count_locs <- geojson_read("data/count_locations.json", what = "sp")

quietway_routes <- readOGR("data/london_quietways.kml")


#set mode to interactive
tmap_mode("view")

tm_shape(quietway_routes) +
  tm_lines(col = "green") +
#  tm_shape(count_locs) +
#  tm_dots(col = "yellow")
  tm_shape(bike_count_locs) +
  tm_dots(col = "red")

#now to load in the data to the locations frame to get yearly info
traffic_count_oob <- read.csv("data/traffic-counts-for-webmap20180110.csv")

#cut out only ones with bicycle counts
bike_count_oob <- traffic_count_oob[traffic_count_oob$DFT.PEDAL.CYCLE != "n/a", ]

#now create a map of bicycle only count locations
bike_count_locs <- count_locs[count_locs@data$cp %in% unique(bike_count_oob$ID),]

#there is only data for 2016/2017 in the data

#find a ratio for bicycles to motorcycles
ggplot(bike_count_oob, aes(x = TOTAL.FLOW, y = DFT.PEDAL.CYCLE, colour = DIRECTION)) + 
  geom_point()

bike_count_oob$DFT.PEDAL.CYCLE <- as.numeric(bike_count_oob$DFT.PEDAL.CYCLE)

bike_count_oob$DFT.MOTOR.CYCLE <- as.numeric(bike_count_oob$DFT.MOTOR.CYCLE)

max(bike_count_oob$DFT.PEDAL.CYCLE)
min(bike_count_oob$DFT.PEDAL.CYCLE)

#create some bins for road sizes - by number of flows
bike_count_oob$road_size_bin <- cut(bike_count_oob$TOTAL.FLOW, breaks = c(0,1000,2500,5000,10000,30000), labels = c("small", "medium", "large", "v_large", "uber_road"))

#now try by specifying the number of bins
bike_count_oob$road_size_bin <- classIntervals(bike_count_oob$TOTAL.FLOW, n = 5, cutlabels = TRUE, labels = c("small", "medium", "large", "v_large", "uber_road"))
  
#try out with classInt


#plot to see if the cutting worked
ggplot(bike_count_oob, aes(x = TOTAL.FLOW, y = DFT.PEDAL.CYCLE, colour = road_size_bin)) + 
  geom_point()

#check to see if the samples are of equal size
sample_size_count <- bike_count_oob %>%
  group_by(YEAR, road_size_bin) %>%
  summarise(bin_size = n())
