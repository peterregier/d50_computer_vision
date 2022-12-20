## This script tries to calculate metrics for each site (catchment and basin scale)
## Wish me luck.

require(pacman)
p_load(tidyverse,
       FedData,
       raster, 
       nhdplusTools,
       sf)

## coordinates for S13R
lat = 47.42055
long = -121.087

## Create an sf for the watershed boundary
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(4326)

## Read in flowlines using nhdplusTools
flowlines <- get_nhdplus(AOI = watershed)

x = get_nlcd(template = watershed, 
         year = 2019, 
         label = "yrb")




