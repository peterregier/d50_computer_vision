## This script works with the initial RC2 site metadata to visualize and create
## a list of lat-longs for pulling USGS d50 estimates
##
## 2022-05-10
## Peter Regier
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse)


# 2. Import data ---------------------------------------------------------------

coordinates <- read_csv("data/022522_all_sites_NHD_streamline_matched_v2.csv") %>% 
  select(site_ID, stream_nam, NHD_lat, NHD_lon) %>% 
  rename("stream_name" = stream_nam) %>% 
  mutate(lat = as.numeric(str_trim(NHD_lat, side = "both")), 
         long = as.numeric(str_trim(NHD_lon, side = "both"))) %>% 
  select(-c(NHD_lat, NHD_lon))

site_metadata <- read_csv("data/RC2_all_stream_Attributes_updated.csv") %>% 
  mutate(site_ID = str_trim(str_replace_all(site_ID, "[^[:alnum:]]", "")), side = "both") %>% 
  select(COMID, D50_m, StreamOrder, site_ID) 
  

df <- left_join(site_metadata, coordinates, by = "site_ID")
write_csv(df, "data/220510_site_latlongs.csv")


