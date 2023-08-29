

require(pacman)
p_load(tidyverse, 
       sf, 
       janitor,
       nhdplusTools,
       cowplot)

common_crs = 4326

x <- read_csv("ignore/d50_Train11_Valid0_Test3_Predict11_50.csv") %>% 
  clean_names() %>% 
  st_as_sf(coords = c("longitude_o", "latitude_o"), crs = common_crs)

## Create an sf for the watershed boundary
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

flowlines <- get_nhdplus(watershed)


ggplot() + 
  geom_sf(data = watershed) +
  geom_sf(data = flowlines) + 
  geom_sf(data = x) 
  
ggplot() + 
  geom_sf(data = watershed, fill = background_color) + 
  geom_sf(data = flowlines, aes(alpha = streamorde), 
          color = "#3279B2", show.legend = F) + 
  geom_sf(data = df, aes(shape = source), size = 4, color = "black") + 
  geom_sf(data = df, aes(color = stream_order, shape = source), size = 3, alpha = 0.8)