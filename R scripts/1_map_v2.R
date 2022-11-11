## This is a revised version of the map script, which instead of relying on stream
## order will pull it in from the nhdplustools package...
##
## 2022-11-09
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse,
       sf,
       nhdplusTools, # pull in watershed boundaries and flowlines 
       cowplot, #plot_grid()
       ggsflabel) #geom_sf_label()

## Set CRS
common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"


# 2. Import data ---------------------------------------------------------------

## Load sites from VGC spreadsheet
site_info <- read_csv("data/RC2 Spatial Study_Responses_Form Responses 1_updated 2022-02-01.csv") %>% 
  clean_names() %>% 
  select(id, longitude_dd, latitude_dd) %>% 
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = common_crs)

# Create an sf for the watershed boundary
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

flowlines <- get_nhdplus(AOI = watershed)

## Time for some light spatial stats: use st_nearest_feature to identify the 
## index of the nearest flowline to each point, then pull the stream order of
## reach reach
sites <- site_info %>% 
  mutate(nearest_feature = st_nearest_feature(site_info, y = flowlines)) %>% 
  mutate(stream_order = flowlines$streamorde[nearest_feature])


## Load sites from VGC spreadsheet
sites_vgc <- read_csv("data/RC2_all_stream_Attributes_updated_VGC.csv") %>% 
  clean_names() %>% 
  rename("stream_order" = stream_orde) %>% 
  select(stream_order, site_id) 

inner_join(site_info, sites_vgc, by = c("id" = "site_id"))



## Plot it
ggplot() + 
  geom_sf(data = watershed) + 
  geom_sf(data = flowlines, aes(alpha = streamorde), 
          color = "blue", show.legend = F) + 
  geom_sf(data = sites, size = 3) + 
  geom_sf(data = sites, aes(color = stream_order), size = 2) + 
  scale_color_viridis_c()





i = 1
site <- sites %>% slice(i)
f1 <- flowlines %>% 
  st_crop(st_buffer(site, 1000))

ggplot() + 
  geom_sf(data = f1, aes(size = streamorde)) + 
  geom_sf(data = site)




