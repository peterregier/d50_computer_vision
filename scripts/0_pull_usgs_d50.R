## This script pulls in USGS data for particle size distribution for sites within
## the YRB
##
## 2022-11-14
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
       dataRetrieval,
       janitor,
       tmaptools,
       plotly,
       ggsflabel) #geom_sf_label()

## Set CRS
common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"

## parameter codes for the sediment sizes
par_codes = c("80164", #< 0.0625 mm
              "80165", #< 0.125 mm
              "80166", #< 0.25 mm
              "80167", #< 0.5 mm
              "80168", #< 1 mm
              "80169") #< 2 mm


# 2. Load in base data ---------------------------------------------------------

# Create an sf for the watershed boundary
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

## Pull in flowlines
flowlines <- get_nhdplus(AOI = watershed)


# 3. Now, pull in USGS data (Heavy lift) ---------------------------------------

## Start with a list of candidate sites (bounded by box around watershed)
x <- whatNWISsites(bBox = c(-121.52268, 45.98279, -119.20511, 47.59780 ), 
  #stateCd = "WA", 
              parameterCd = "80164") %>% 
  pull(site_no)

## Function to pull lat-longs for potential sites
pull_latlongs <- function(site_number){
  readNWISsite(site_number) %>% 
    rename("long" = dec_long_va, 
           "lat" = dec_lat_va) %>% 
    select(site_no, long, lat)
}

## Create a tibble of lat-longs (20 candidate sites)
sites <- x %>% 
  map(pull_latlongs) %>% 
  bind_rows() %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs) %>% 
  crop_shape(watershed, polygon = T)

## Load in raw USGS data
raw_data <- readNWISqw(siteNumbers = sites$site_no, 
             parameterCd = par_codes) %>% 
  rename("percent" = result_va) %>% 
  select(site_no, parm_cd, percent) 

## Now, bin multiple measurements for site-parameter combos, then filter to
## only include sites that enough data
bin_data <- raw_data %>% 
  group_by(site_no, parm_cd) %>% 
  summarize(percent = mean(percent)) %>%
  ungroup() %>% 
  group_by(site_no) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% #removes any sites with a single point
  mutate(min_perc = min(percent), 
         max_perc = max(percent)) %>% # calculate the minimum % by site
  filter(min_perc < 50 & max_perc > 50) # removes any sites without data < 50%


## recode data so parameter can be used quantitatively for slope calculations
coded_data <- bin_data %>% 
  mutate(size_mm = case_when(parm_cd == "80164" ~ 0.0625, 
                             parm_cd == "80165" ~ 0.125,
                             parm_cd == "80166" ~ 0.25,
                             parm_cd == "80167" ~ 0.5,
                             parm_cd == "80168" ~ 1,
                             parm_cd == "80169" ~ 2))
  
## Check that all sites should be included
ggplot(coded_data, aes(size_mm, percent)) + 
  geom_point() + 
  geom_hline(yintercept = 50) + 
  facet_wrap(~site_no) 

## Now, we start making some assumptions: Based on the graph above, we are going
## to assume that the relationships between size_mm and percent is linear in the
## lower ranges, such that lm() accurately represents the relationship between
## the data-points closest to 50 (both <50 and >50).

## This function calculates a lm fit based on the two nearest values (one above
## and one below) to 50.
est_d50 <- function(site){
  
  x <- coded_data %>% filter(site_no == site)
  
  y <- bind_rows(x %>% filter(percent > 50) %>% slice(which.min(abs(50 - percent))),
                 x %>% filter(percent <= 50) %>% slice(which.min(abs(percent - 50))))
  
  ggplot(y, aes(size_mm, percent)) + 
    geom_point() + 
    geom_line() + 
    geom_hline(yintercept = 50)
  
  m = summary(lm(percent ~ size_mm, data = y))[[4]][2, 1]
  b = summary(lm(percent ~ size_mm, data = y))[[4]][1, 1]
  
  d50_est = (50 - b) / m
  
  return(d50_est)
}


## Now, create a vector of the estimated d50 values
d50_estimates <- tibble(site_no = unique(coded_data$site_no), 
                        d50_mm = unique(coded_data$site_no) %>% 
                          map(est_d50) %>% 
                          unlist())


sites_nhd <- sites %>% 
  mutate(nearest_feature = st_nearest_feature(sites, y = flowlines)) %>% 
  mutate(stream_order = flowlines$streamorde[nearest_feature])

## Finally, merge with original USGS layer to get geometry
usgs_sites <- right_join(sites_nhd, d50_estimates, by = "site_no")

## Map to check things look right
ggplot() + 
  geom_sf(data = watershed) + 
  geom_sf(data = usgs_sites)


# 4. Spatial stats to ID sites that are close to RC sites ----------------------

## Load sites from VGC spreadsheet
site_info <- read_csv("data/RC2_Spatial_Study_Responses_Form_Responses_1_updated 2022-02-01.csv") %>% 
  clean_names() %>% 
  select(id, lat, long) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs)

## Quick visualization of producs together
ggplot() + 
  geom_sf(data = watershed) + 
  geom_sf(data = flowlines, aes(alpha = streamorde), 
          color = "blue", show.legend = F) + 
  geom_sf(data = site_info) + 
  geom_sf(data = usgs_sites, aes(color = d50_mm), pch = 9, size = 5) + 
  scale_color_viridis_c(option = 2)

## Unfortunately, there are only 4 sites that look like they pair with RC sites: 
## Calculate distance from nearest site to help ID useful sites
distances <- list(NA)
for(i in 1:nrow(usgs_sites)){
  distances[[i]] <- min(st_distance(x = usgs_sites %>% slice(i), y = site_info))
}

## add distances on to sites
usgs_sites$distance_from_rc_m <- unlist(distances)

## Now, an ugly workaround to match RC sites to USGS sites
rows <- list(NA)
for(i in 1:nrow(usgs_sites)){
  rows[[i]] <- which.min(st_distance(x = usgs_sites %>% slice(i), y = site_info))
}

usgs_matched <- usgs_sites %>% 
  mutate(distance_from_rc_m = unlist(distances), 
         index = unlist(rows), 
         rc_site = site_info$id[index]) %>% #add distances
  mutate(long = unlist(map(usgs_sites$geometry,1)),
         lat = unlist(map(usgs_sites$geometry,2))) %>% 
  st_drop_geometry() %>% 
  select(-index)
write_csv(usgs_matched, "data/221114_usgs_d50_matched.csv")
