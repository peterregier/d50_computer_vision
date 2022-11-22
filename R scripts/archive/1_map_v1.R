## Create a map showing the locations of sampling sites for the d50 paper
##
## 2022-04-26
## Peter Regier
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Read in packages
require(pacman)
p_load(tidyverse, # keep things tidy
       sf, # simple spatial features
       purrr,
       janitor,
       rnaturalearth, 
       ggthemes) # theme_map()

## Set a common crs
common_crs = 4326

# 2. Import data ---------------------------------------------------------------

## Archived because stream orders were wrongly attributed (downstream YR site = 2)
# site_info <- read_csv("data/RC2_all_stream_Attributes_updated.csv") %>% 
#   # remove white space after removing special characters
#   mutate(site_ID = str_trim(str_replace_all(site_ID, "[^[:alnum:]]", "")), side = "both")

# site_info2 <- read_csv("data/ForKS_New_Train1_Prediction1_10_40.csv") %>% 
#   clean_names() %>% 
#   select(latitude, longitude, stream_order)

site_info <- read_csv("data/RC2 Spatial Study_Responses_Form Responses 1_updated 2022-02-01.csv") %>% 
  clean_names()
  select(longitude_dd, latitude_dd, stream_gradient)

# Load shapefile of sites
sites_shp <- read_sf("data/gis/sites/022522_all_sites_NHD_streamline_matched.shp") %>% 
  st_transform(common_crs) %>% 
  mutate(site_ID = str_trim(site_ID, side = "both")) # remove random white space
  
# 
sites <- sites_shp %>% 
  inner_join(site_info, by = "site_ID")

# sites2 <- st_as_sf(site_info2, coords = c("longitude", "latitude"), 
#                    crs = common_crs)

# Create an sf for the watershed boundary
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)


# ggplot() + 
#   geom_sf(data = watershed) + 
#   geom_sf(data = sites2, size = 3, aes(color = as.factor(stream_order)))

# Set up list of shapefiles to read in
flowline_files <- c("data/gis/NHD_H_17030001_HU8_Shape/Shape/NHDFlowline.shp", 
                    "data/gis/NHD_H_17030002_HU8_Shape/Shape/NHDFlowline.shp", 
                    "data/gis/NHD_H_17030003_HU8_Shape/Shape/NHDFlowline.shp")

# Create a single sf object for the multiple flowline files
flowlines <- map(flowline_files, read_sf) %>% 
  bind_rows() %>% #combine into one file
  st_transform(common_crs) %>% # set crs
  st_zm() %>% 
  mutate(divide = ifelse(lengthkm > 1, "long", "short"))

# Remove NAs
flowlines2 <- flowlines %>% 
  filter(!is.na(gnis_name))


# 3. Make map ------------------------------------------------------------------

# Create map
ggplot() + 
  geom_sf(data = watershed, fill = "#C7E4CE") +
  geom_sf(data = flowlines2, color = "#3279B2") + 
  geom_sf(data = sites, size = 4, color = "white") +
  geom_sf(data = sites, aes(color = StreamOrder), size = 3, alpha = 0.8) +
  scale_color_viridis_c(option = "A") + 
  theme_map() + 
  theme(legend.position = c(0.8, 0.6))
#ggsave("figures/220426_map_v1.pdf", width = 6, height = 5)
ggsave("figures/map_v1.png", width = 6, height = 5)


