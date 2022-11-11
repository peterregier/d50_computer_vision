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
       ggpubr, #stat_compare_means()
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
  
  ggplot() + 
    geom_sf(data = watershed) +
    geom_sf(data = flowlines, color = "blue") +
    geom_sf(data = x, 
            aes(color = divide_d50), size = 4, alpha = 0.8) + 
    labs(color = "d50 (mm)", title = d50_name, shape = "") + 
    scale_color_manual(values = c("#82C46E", "#440F5A")) + 
    theme(legend.position = c(0.8, 0.7), 
          legend.background = element_blank(), 
          legend.box.background = element_blank(), 
          legend.margin = margin(-0.5,0,0,0, unit="cm"))
}

plot_grid(plot_map("Abeshu"), plot_map("NEXSS"), plot_map("YOLO"), nrow = 1)
ggsave("figures/5_figure5.png", width = 15, height = 5)


## Now, let's calculate the distance of each point from the Yakima and see how
## that shakes out

## For-loop to find the minimum straight-line distance from the the Yakima 
distances <- list(NA)
for(i in 1:nrow(df_sf)){
  distances[[i]] <- min(st_distance(x = df_sf %>% slice(i), y = flowlines))
}

## Create an easy-to-plot dataset (not sf)
df_distance <- st_drop_geometry(df_sf) %>% 
  mutate(distance_m = unlist(distances)) %>% 
  group_by(name) %>%  
  mutate(divide_d50 = ifelse(value > median(value), "Above median", "Below median"))

plot_grid(ggplot(df_distance, aes(x = divide_d50, distance_m)) + 
            geom_boxplot(aes(fill = divide_d50), width = 0.6, show.legend = F) +
            facet_wrap(~name, nrow = 1) + 
            stat_compare_means(label = "p.format", label.x = 1.3, label.y = 48000) + 
            labs(x = "d50 value", y = "Distance from main stem (m)"), 
          ggplot(df_distance, aes(x = divide_d50, gps_lat)) + 
            geom_boxplot(aes(fill = divide_d50), width = 0.6, show.legend = F) +
            facet_wrap(~name, nrow = 1) + 
            stat_compare_means(label = "p.format", label.x = 1.3, label.y = 47.4) + 
            labs(x = "d50 value", y = "Latitude"), 
          ggplot(df_distance, aes(x = divide_d50, gps_lon)) + 
            geom_boxplot(aes(fill = divide_d50), width = 0.6, show.legend = F) +
            facet_wrap(~name, nrow = 1) + 
            stat_compare_means(label = "p.format", label.x = 1.3, label.y = -119.8) + 
            labs(x = "d50 value", y = "Longitude"), 
          ncol = 1)
ggsave("figures/SI_boxplots_for_Fig5.png", width = 6, height = 8)

df_distance %>% 
  filter(name == "YOLO") %>% 
  group_by(divide_d50) %>% 
  summarize(median(distance_m))
          

#df_sf$distance_m = tibble(unlist(distances))

