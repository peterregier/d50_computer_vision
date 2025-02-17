## Compare NEXSS data between USGS and YOLO sites

require(pacman)
p_load(tidyverse,
       dataRetrieval,
       nhdplusTools,
       janitor,
       ggpubr,
       cowplot, 
       sf)

theme_set(theme_bw())

common_crs = 4236


# 1. Read in data --------------------------------------------------------------

nexss_d50 <- read_csv("data/ignore/model_resp_annual_yrb_input_output_df_01_16_2023.csv") %>% 
  clean_names() 

yolo_nexss <- read_csv("data/221115_rc2_master.csv") %>% 
  group_by(site_id) %>% 
  summarize(streamorde = first(stream_order), 
            d50_mm = mean(d50_mm_nexss)) %>% 
  mutate(source = "yolo")

usgs_sites <- read_csv("data/221114_usgs_d50_matched.csv") %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs)

pull_comids <- function(i){
  get_nhdplus(AOI = usgs_sites %>% slice(i)) %>% 
    dplyr::select(comid, contains("orde"))
}

usgs_comids <- 1:nrow(usgs_sites) %>% 
  map(pull_comids) %>% 
  bind_rows()

## We now have 8/11 instead of 6/11 (2 were NAs, 1 missing entirely)
usgs_nexss <- readRDS("data/dataHUC17_May29_2020.rds") %>% 
  as_tibble() %>% 
  filter(comid_nhd %in% unique(usgs_comids$comid)) %>% 
  rename("comid" = comid_nhd) %>% 
  mutate(d50_mm = D50_m * 1000) %>% 
  select(comid, d50_mm) %>% 
  left_join(usgs_comids) %>% 
  mutate(source = "usgs") %>% 
  st_drop_geometry()

# usgs_nexss <- nexss_d50 %>% 
#   filter(comid %in% unique(usgs_comids$comid)) %>% 
#   dplyr::select(comid, d50_m) %>% 
#   mutate(d50_mm = d50_m * 1000) %>% 
#   mutate(source = "usgs")

df <- bind_rows(yolo_nexss %>% dplyr::select(source, d50_mm, streamorde), 
          usgs_nexss %>% dplyr::select(source, d50_mm, streamorde)) 

## To better apples-to-apples, only include stream orders present in USGS
orders <- unique(df %>% drop_na() %>% filter(source == "usgs") %>% pull(streamorde))

df_trim <- df %>% 
  filter(streamorde %in% orders)

p1 <- ggplot(df_trim, aes(x = d50_mm, fill = source)) + 
  geom_density(alpha = 0.5, show.legend = F) + 
  scale_x_log10() 

p2 <- ggplot(df_trim, aes(x = source, y = d50_mm, fill = source)) + 
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) + 
  geom_jitter(width = 0.1, alpha = 0.4) +
  scale_y_log10() + 
  stat_compare_means()

p3 <- ggplot(df, aes(as.factor(streamorde), d50_mm, fill = source)) + 
  geom_boxplot()

plot_grid(p1, p2, nrow = 1)
ggsave("figures/240801_yolo_vs_usgs_from_nexss.png", width = 7, height = 3.5)





