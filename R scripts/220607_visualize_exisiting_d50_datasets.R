## This script brings together three potential d50 data sources for the Yakima 
## River Basin, and creates maps and figures that compare/summarize them in prep 
## for comparison to our image-derived estimates
## 
## 2022-06-07
## Peter Regier
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, sf, cowplot, 
       ggallin) #pseudolog10_trans()

## Set theme
theme_set(theme_bw())

## Set CRS for all layers
common_crs = 4326

# 2. Read in shapefiles for making maps ----------------------------------------

## First, bring in the shapefile with all the sites
rc2_sites <- read_sf("data/gis/sites/022522_all_sites_NHD_streamline_matched.shp") %>% 
  mutate(site_ID = str_trim(site_ID)) %>% 
  st_transform(common_crs)

## Now, set up the outline of the watershed
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

## And set up flowlines
flowline_files <- c("data/gis/NHD_H_17030001_HU8_Shape/Shape/NHDFlowline.shp", 
                    "data/gis/NHD_H_17030002_HU8_Shape/Shape/NHDFlowline.shp", 
                    "data/gis/NHD_H_17030003_HU8_Shape/Shape/NHDFlowline.shp")

flowlines <- map(flowline_files, read_sf) %>% 
  bind_rows() %>% 
  st_transform(common_crs) %>% 
  st_zm() %>% 
  mutate(divide = ifelse(lengthkm > 1, "long", "short"))

## Check that shapefiles are ready to rock
ggplot() + 
  geom_sf(data = watershed) + 
  geom_sf(data = flowlines, color = "blue", alpha = 0.1)


# 2. Bring in datasets ---------------------------------------------------------

## Read the first two d50 estimate datasets in
nexss_gupta_raw <- read_csv("data/RC2_all_D50.csv") %>% 
  mutate(site_ID = str_trim(str_remove_all(site_ID, "[[:punct:]]")))

r2 <- round(summary(lm(D50_mm_guta ~ D50_mm_nexss, data = nexss_gupta_raw))[[9]], 2)
p <- round(summary(lm(D50_mm_guta ~ D50_mm_nexss, data = nexss_gupta_raw))[[4]][2, 4], 4)

ggplot(nexss_gupta_raw, aes(D50_mm_nexss, D50_mm_guta)) + 
  geom_point() + geom_smooth(method = "lm", se = F) + 
  annotate("text", x = 100, y = 4.5, label = paste0("R2 = ", r2))  + 
  annotate("text", x = 100, y = 4.1, label = paste0("p = ", p))


d50_sf <- rc2_sites %>% select(site_ID) %>% 
  inner_join(nexss_gupta_raw, by = "site_ID")

boxplot <- nexss_gupta_raw %>% 
  #select(D50_mm_nexss, D50_mm_guta) %>% 
  pivot_longer(cols = c(D50_mm_nexss, D50_mm_guta), 
               names_to = "data_source", 
               values_to = "d50_mm") %>% 
  ggplot(aes(data_source, d50_mm)) + geom_boxplot() + 
  scale_y_continuous(trans = pseudolog10_trans)


nexss_map <- ggplot() +
  geom_sf(data = watershed) +
  geom_sf(data = d50_sf, aes(size = StreamOrde)) +
  geom_sf(data = d50_sf, aes(size = StreamOrde * 0.6, color = D50_mm_nexss), alpha = 0.8) + 
  scale_color_viridis_c(trans = "sqrt") + 
  labs(title = "D50 (mm) -  NEXSS", color = "d50 (mm)", size = "Stream \n Order")

gupta_map <- ggplot() +
  geom_sf(data = watershed) +
  geom_sf(data = d50_sf, aes(size = StreamOrde)) +
  geom_sf(data = d50_sf, aes(size = StreamOrde * 0.6, color = D50_mm_guta), alpha = 0.8) + 
  scale_color_viridis_c(trans = "sqrt") + 
  labs(title = "D50 (mm) -  Abeyshu", color = "d50 (mm)", size = "Stream \n Order")



plot_grid(nexss_map, gupta_map, boxplot, rel_widths = c(1, 1, 0.5), nrow = 1)
ggsave("figures/220607_d50_maps.pdf", width = 15, height = 5)


## Next steps: figure out how to import the USGS data. Maybe easier to pull from 
## USGS directly via dataRetrieval rather than hacking Kyongho's spreadsheet
