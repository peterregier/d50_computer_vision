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
       nhdplusTools,
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

## Read in flowlines using nhdplusTools
flowlines <- get_nhdplus(AOI = watershed) %>% 
  st_transform(common_crs) %>% # set crs
  st_zm() %>%
  filter(gnis_name == "Yakima River")

flowlines_satus <- get_nhdplus(AOI = watershed) %>% 
  st_transform(common_crs) %>% # set crs
  st_zm() %>%
  filter(grepl("Satus", gnis_name))


# 3. Assemble data and convert to sf -------------------------------------------

df_raw <- read_csv("data/250314_rc2_master.csv") 

df_bin <- df_raw %>% 
  filter(type != "Train1") %>% 
  group_by(site_id) %>% 
  summarize(stream_order = median(stream_order),
            lat = median(lat), 
            long = median(long),
            d50_mm_yolo = median(d50_mm_yolo), 
            d50_mm_nexss = median(d50_mm_nexss), 
            d50_mm_abeshu = median(d50_mm_abeshu))

df_long_raw <- df_bin %>% 
  pivot_longer(cols = c(contains("d50_mm")), names_to = "source", values_to = "d50_mm") 

## Finally, join with USGS
usgs_raw <- read_csv("data/221114_usgs_d50_matched.csv") %>% 
  mutate(source = "d50_mm_a_usgs", 
         site_id = as.character(site_no)) %>% 
  select(site_id, stream_order, lat, long, source, d50_mm)

df_sf <- bind_rows(df_long_raw, usgs_raw) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs)


# 4. Make maps -----------------------------------------------------------------

plot_map <- function(d50_name, title){
  x <- df_sf %>% dplyr::filter(source == d50_name) %>% 
    mutate(divide_d50 = ifelse(d50_mm > median(d50_mm), "Above median", "Below median"))
  
  ggplot() + 
    geom_sf(data = watershed) +
    geom_sf(data = flowlines, color = "blue") +
    geom_sf(data = flowlines_satus, color = "lightblue") +
    geom_sf(data = x, 
            aes(color = divide_d50), size = 4, alpha = 0.8) + 
    labs(color = "D50 (mm)", title = title, shape = "") + 
    scale_color_manual(values = c("#82C46E", "#440F5A")) + 
    theme(legend.position = c(0.8, 0.7), 
          legend.background = element_blank(), 
          legend.box.background = element_blank(), 
          legend.margin = margin(-0.5,0,0,0, unit="cm"))
}

plot_grid(plot_map("d50_mm_a_usgs", "USGS"), 
          plot_map("d50_mm_abeshu", "Abeshu"), 
          plot_map("d50_mm_nexss", "NEXSS"), 
                   plot_map("d50_mm_yolo", "YOLO"), 
          nrow = 2, labels = c("A", "B", "C", "D"))
ggsave("figures/3_figure3_maps_revised.png", width = 10, height = 10)
ggsave("figures/3_figure3_maps_revised.pdf", width = 10, height = 10)
ggsave("figures/jpg/3_figure3_maps_revised.jpg", width = 10, height = 10)


# 5. Make boxplots to compare above/below median to lat/long/distance ----------

## For-loop to find the minimum straight-line distance from the the Yakima 
distances <- list(NA)
for(i in 1:nrow(df_sf)){
  distances[[i]] <- min(st_distance(x = df_sf %>% slice(i), y = flowlines))
}

## Create an easy-to-plot dataset (not sf)
df_distance <- df_sf %>% 
  mutate(long = unlist(map(df_sf$geometry,1)),
         lat = unlist(map(df_sf$geometry,2))) %>% 
  st_drop_geometry() %>% 
  mutate(distance_m = unlist(distances)) %>% 
  group_by(source) %>%  
  mutate(divide_d50 = ifelse(d50_mm > median(d50_mm), "Above median", "Below median"))

plot_grid(ggplot(df_distance, aes(x = divide_d50, distance_m)) + 
            geom_boxplot(aes(fill = divide_d50), width = 0.6, show.legend = F) +
            facet_wrap(~source, nrow = 1) + 
            stat_compare_means(label = "p.format", label.x = 1.3, label.y = 48000) + 
            labs(x = "D50 value", y = "Distance from main stem (m)"), 
          ggplot(df_distance, aes(x = divide_d50, lat)) + 
            geom_boxplot(aes(fill = divide_d50), width = 0.6, show.legend = F) +
            facet_wrap(~source, nrow = 1) + 
            stat_compare_means(label = "p.format", label.x = 1.3, label.y = 47.4) + 
            labs(x = "D50 value", y = "Latitude"), 
          ggplot(df_distance, aes(x = divide_d50, long)) + 
            geom_boxplot(aes(fill = divide_d50), width = 0.6, show.legend = F) +
            facet_wrap(~source, nrow = 1) + 
            stat_compare_means(label = "p.format", label.x = 1.3, label.y = -119.8) + 
            labs(x = "D50 value", y = "Longitude"), 
          ncol = 1)
ggsave("figures/S3_boxplots_for_Fig3_revised.png", width = 8, height = 8)


df_distance %>% 
  group_by(source, divide_d50) %>% 
  summarize(median(distance_m))
