
require(pacman)
p_load(tidyverse, sf)

## Set CRS
common_crs = 4326

## Load sites from original spreadsheet
sites_og <- read_csv("data/RC2_all_stream_Attributes_updated_UNTRUSTED.csv") %>% 
  clean_names() %>% 
  select(stream_order, site_id) %>% 
  mutate(source = "OG")

## Load sites from VGC spreadsheet
sites_vgc <- read_csv("data/RC2_all_stream_Attributes_updated_VGC.csv") %>% 
  clean_names() %>% 
  rename("stream_order" = stream_orde) %>% 
  select(stream_order, site_id) %>% 
  mutate(source = "VGC")

inner_join(sites_og %>% rename("so_og" = stream_order) %>% select(-source), 
           sites_vgc %>% rename("so_vgc" = stream_order) %>% select(-source), 
           by = "site_id") %>% 
  mutate(diff = so_og - so_vgc) %>% 
  filter(diff != 0) %>% 
  ggplot(aes(site_id, diff)) + geom_point()



sites_raw <- bind_rows(sites_og, sites_vgc)

# Create an sf for the watershed boundary
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

flowlines <- get_nhdplus(AOI = watershed)

sites_nhd <- site_info %>% 
  mutate(nearest_feature = st_nearest_feature(site_info, y = flowlines)) %>% 
  mutate(stream_order = flowlines$streamorde[nearest_feature])


inner_join(sites_nhd %>% rename("so_nhd" = stream_order) %>% select(id, stream_order), 
           sites_vgc %>% rename("so_vgc" = stream_order) %>% select(site_id, stream), 
           by = "site_id") %>% 
  mutate(diff = so_og - so_vgc) %>% 
  filter(diff != 0) %>% 
  ggplot(aes(site_id, diff)) + geom_point()


x <- read_sf("data/gis/sites/022522_all_sites_NHD_streamline_matched.shp") %>% 
  clean_names()

sites <- full_join(x, sites_raw, by = "site_id")

ggplot() + 
  geom_sf(data = watershed) + 
  geom_sf(data = flowlines, aes(color = streamorde)) + 
  geom_sf(data = sites, color = "white", size = 4) + 
  geom_sf(data = sites, aes(color = stream_order), size = 3) + 
  scale_color_viridis_c() 


