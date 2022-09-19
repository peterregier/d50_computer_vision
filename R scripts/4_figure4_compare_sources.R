## This script constructs maps and boxplots to compare YOLO estimates to other
## d50 measurements/estimates
##
## 2022-08-29
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


# 3. Bring in datasets ---------------------------------------------------------

## First, bring in the shapefile with all the sites
sites_raw <- read_sf("data/gis/sites/022522_all_sites_NHD_streamline_matched.shp") %>% 
  clean_names() %>% 
  mutate(site_id = str_trim(site_id)) %>% 
  st_transform(common_crs) %>% 
  select(site_id)

## Then, combine with slope and stream order info. You need sites_raw to provide
## coordinates.
sites <- inner_join(sites_raw, read_csv("data/RC2_all_D50.csv") %>% 
  clean_names() %>% 
  mutate(site_id = str_trim(str_remove_all(site_id, "[[:punct:]]"))) %>% 
  select(-c(contains("d50"))))

## Read in YOLO estimates 
yolo <- read_csv("data/220728/d50/d50_Train1_Prediction1_40.csv") %>% 
  clean_names() %>% 
  separate(name, into = c("study", "site_id", "date", "id")) %>% 
  filter(study != "PNNL") %>% 
  group_by(site_id) %>% 
  summarize(d50_mm = mean(long_axis_m) * 1000) %>% 
  mutate(source = "YOLO") %>% 
  select(site_id, source, d50_mm)

## Read the first two d50 estimate datasets in
nexss_guta <- read_csv("data/RC2_all_D50.csv") %>% 
  clean_names() %>% 
  mutate(site_id = str_trim(str_remove_all(site_id, "[[:punct:]]"))) %>% 
  rename("stream_order" = stream_orde) %>% 
  pivot_longer(cols = c(contains("d50_mm")), names_to = "source", values_to = "d50_mm") %>% 
  mutate(source = str_to_title(str_remove(source, "d50_mm_"))) %>% 
  select(site_id, source, d50_mm) %>% 
  filter(site_id %in% yolo$site_id)

df_raw <- bind_rows(yolo, nexss_guta) %>% 
  inner_join(sites)

df_sf <- inner_join(sites, df_raw)

maps <- ggplot() +
  geom_sf(data = watershed) +
  geom_sf(data = df_sf, aes(size = stream_orde)) +
  geom_sf(data = df_sf, aes(size = stream_orde * 0.6, color = d50_mm), alpha = 0.8) + 
  scale_color_viridis_c(trans = "sqrt") + 
  facet_wrap(~source, ncol = 1) + 
  labs(color = "d50 (mm)", size = "Stream \n Order")

by_source <- ggplot(df_raw, aes(x = source, y = d50_mm)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0.2) +
  scale_y_continuous(trans = "sqrt") + 
  labs(x = "Source", y = "d50 (mm)")

by_order <- ggplot(df_raw, aes(x = as.factor(stream_orde), y = d50_mm, fill = source)) + 
  geom_boxplot() +
  scale_y_continuous(trans = "sqrt") + 
  labs(x = "Stream Order", y = "d50 (mm)", fill = "") + 
  theme(legend.position = c(0.85, 0.85), 
        legend.background = element_blank())

boxplots <- plot_grid(by_source, by_order, ncol = 1)

plot_grid(maps, boxplots, nrow = 1)
ggsave("figures/4_figure4_compare_sources.png")



