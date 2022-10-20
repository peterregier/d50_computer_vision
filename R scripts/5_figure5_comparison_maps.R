## This script makes comparison maps to compare how distributions vary spatially
##
## 2022-10-17
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, sf, cowplot, janitor,
       ggallin) #pseudolog10_trans()

## Set theme
theme_set(theme_bw())

## Set CRS for all layers
common_crs = 4326


# 2. Import map shapefiles -----------------------------------------------------

## Now, set up the outline of the watershed
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

# Set up list of shapefiles to read in
flowline_files <- c("data/gis/NHD_H_17030001_HU8_Shape/Shape/NHDFlowline.shp", 
                    "data/gis/NHD_H_17030002_HU8_Shape/Shape/NHDFlowline.shp", 
                    "data/gis/NHD_H_17030003_HU8_Shape/Shape/NHDFlowline.shp")

# Create a single sf object for the multiple flowline files
flowlines <- map(flowline_files, read_sf) %>% 
  bind_rows() %>% #combine into one file
  st_transform(common_crs) %>% # set crs
  st_zm() %>% 
  filter(gnis_name == "Yakima River")


# 3. Assemble data and convert to sf -------------------------------------------

## Read in YOLO estimates 
yolo <- read_csv("data/220728/d50/d50_Train1_Prediction1_40.csv") %>% 
  clean_names() %>% 
  separate(name, into = c("study", "site_id", "date", "id")) %>% 
  filter(study != "PNNL") %>% 
  group_by(site_id) %>% 
  summarize(YOLO = mean(long_axis_m) * 1000)

## Read the first two d50 estimate datasets in
nexss_guta <- read_csv("data/RC2_all_D50.csv") %>% 
  clean_names() %>% 
  mutate(site_id = str_trim(str_remove_all(site_id, "[[:punct:]]"))) %>% 
  rename("stream_order" = stream_orde) %>% 
  rename("NEXSS" = d50_mm_nexss, 
         "Abeshu" = d50_mm_guta) %>% 
  select(site_id, NEXSS, Abeshu, stream_order)

df <- inner_join(yolo, nexss_guta, by = "site_id") %>% 
  pivot_longer(cols = c(YOLO, NEXSS, Abeshu)) 

## First, bring in the shapefile with all the sites
sites_raw <- read_sf("data/gis/sites/022522_all_sites_NHD_streamline_matched.shp") %>% 
  clean_names() %>% 
  mutate(site_id = str_trim(site_id)) %>% 
  st_transform(common_crs) 

df_sf <- inner_join(sites_raw, df, by = "site_id")

plot_map <- function(d50_name){
  x <- df_sf %>% dplyr::filter(name == d50_name) %>% 
    mutate(divide_d50 = ifelse(value > median(value), "Above median", "Below median"))
  
  color_min = min(x$value)
  color_max = max(x$value)
  
  ggplot() + 
    geom_sf(data = watershed) +
    geom_sf(data = flowlines, color = "blue") +
    geom_sf(data = x, 
            aes(color = value, shape = divide_d50), size = 3) + 
    labs(color = "d50 (mm)", title = d50_name, shape = "") + 
    scale_color_viridis_c(limits = c(color_min, color_max)) + 
    theme(legend.position = c(0.8, 0.7), 
          legend.background = element_blank(), 
          legend.box.background = element_blank(), 
          legend.margin = margin(-0.5,0,0,0, unit="cm"))
}

plot_grid(plot_map("Abeshu"), plot_map("NEXSS"), plot_map("YOLO"), nrow = 1)
ggsave("figures/5_figure5.png", width = 15, height = 5)




