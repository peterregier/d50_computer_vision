## This script constructs maps and boxplots to compare YOLO estimates to other
## d50 measurements/estimates
##
## 2022-08-29 (Updated 2022-10-06)
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
  summarize(d50_mm = mean(long_axis_m) * 1000, 
            accuracy = mean(yolo_accuracy_percent)) %>% 
  mutate(source = "YOLO") %>% 
  select(site_id, source, d50_mm, accuracy)

yolo_sf <- inner_join(sites, yolo, by = "site_id")

p_d50 <- ggplot() + 
  geom_sf(data = watershed) +
  geom_sf(data = yolo_sf, aes(size = stream_orde)) +
  geom_sf(data = yolo_sf, aes(size = stream_orde * 0.6, color = d50_mm), alpha = 0.8) + 
  scale_color_viridis_c(trans = "sqrt") + 
  labs(color = "d50 (mm)", size = "Stream \n Order", title = "YOLO d50")

p_accuracy <- ggplot() + 
  geom_sf(data = watershed) +
  geom_sf(data = yolo_sf, aes(size = stream_orde)) +
  geom_sf(data = yolo_sf, aes(size = stream_orde * 0.6, color = accuracy), alpha = 0.8) + 
  scale_color_viridis_c(trans = "sqrt") + 
  labs(color = "Accuracy (%)", size = "Stream \n Order", title = "YOLO accuracy")

plot_grid(p_d50, p_accuracy, nrow = 1)
ggsave("figures/4_figure4_comparison_maps.png", width = 9, height = 4)


yolo_so <- inner_join(yolo, read_csv("data/RC2_all_D50.csv") %>% 
             clean_names() %>% 
             select(site_id, stream_orde)) %>% 
  mutate(stream_order = as.factor(stream_orde))
  
yolo_so %>% 
  pivot_longer(cols = c(d50_mm, accuracy), names_to = "metric") %>% 
  ggplot() + 
  geom_boxplot(aes(stream_order, value)) + 
  geom_smooth(aes(stream_orde, value)) + 
  facet_wrap(~metric, ncol = 1, scales = "free_y") 

ggplot(yolo_so, aes(d50_mm, accuracy)) + 
  geom_point(aes(size = stream_order, color = stream_order)) + 
  scale_color_viridis_d()

summary(lm(accuracy~d50_mm, data = yolo_so))


## Read the first two d50 estimate datasets in
nexss_guta <- read_csv("data/RC2_all_D50.csv") %>% 
  clean_names() %>% 
  mutate(site_id = str_trim(str_remove_all(site_id, "[[:punct:]]"))) %>% 
  rename("stream_order" = stream_orde) %>% 
  pivot_longer(cols = c(contains("d50_mm")), names_to = "source", values_to = "d50_mm") %>% 
  mutate(source = str_to_title(str_remove(source, "d50_mm_"))) %>% 
  select(site_id, source, d50_mm) %>% 
  filter(site_id %in% yolo$site_id)

df_all <- bind_rows(yolo, nexss_guta) %>% 
  inner_join(sites) %>% 
  mutate(stream_order = as.factor(stream_orde))

boxplot <- ggplot(df_all, aes(source, d50_mm)) + 
  geom_boxplot(aes(fill = source), alpha = 0.5, show.legend = F) + 
  scale_y_continuous(trans = "sqrt")  + 
  labs(x = "Estimate source", y = "d50 (mm)")

## Last, read in the Abeyshu data: 
abeyshu_raw <- read_csv("data/abeyshu_fig1_digitized.csv")

df_histogram <- df_all %>% mutate(d50_mm_log2 = log2(d50_mm))
                                  
histograms <- ggplot() + 
  geom_col(data = abeyshu_raw, aes(d50_mm_log2, count), alpha = 0.5) + 
  geom_histogram(data = df_histogram, aes(d50_mm_log2, fill = source), 
                 color = "black", alpha = 0.5, show.legend = F) + 
  facet_wrap(~source, ncol = 1) +
  labs(x = "log2 d50", fill = "")  

stream_order_boxplot <- ggplot(df_all, aes(stream_order, d50_mm)) + 
  geom_boxplot(aes(fill = stream_order), width = 0.5, show.legend = F) + 
  facet_wrap(~source, ncol = 1, scales = "free_y") + 
  scale_fill_viridis_d() + 
  labs(x = "Strahler Stream Order", y = "d50 (mm)")

plot_grid(boxplot, histograms, stream_order_boxplot, 
          rel_widths = c(0.75, 1, 1), nrow = 1, labels = c("A", "B", "C"))
ggsave("figures/4_figure4.png", width = 7, height = 4)


df_scatterplots <- df_all %>% 
  select(site_id, stream_order, source, d50_mm) %>% 
  pivot_wider(names_from = source, values_from = d50_mm)

nexss_yolo <- ggplot(df_scatterplots, aes(Nexss, YOLO)) + 
  geom_point(aes(color = stream_order)) + 
  scale_color_viridis_d()

guta_yolo <- ggplot(df_scatterplots, aes(Guta, YOLO)) + 
  geom_point(aes(color = stream_order)) + 
  scale_color_viridis_d()

scatterplots <- plot_grid(nexss_yolo, guta_yolo, ncol = 1)

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



