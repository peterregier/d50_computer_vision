## This is a revised version of the map script, which instead of relying on stream
## order will pull it in from the nhdplustools package...
##
## 2022-11-09
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse,
       sf,
       sp, #deal with rasters
       FedData,
       janitor,
       rnaturalearth,
       ggspatial, # north arrow and compass
       ggsflabel, # add labels
       nhdplusTools, # pull in watershed boundaries and flowlines 
       cowplot, # plot_grid()
       plotly,
       ggsflabel) #geom_sf_label()

## Set CRS
common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"

## Background color for watershed
background_color = "#C7E4CE"


# 2. Import data ---------------------------------------------------------------

## Import RC points
df_rc <- read_csv("data/221115_rc2_master.csv") %>% 
  group_by(site_id) %>% 
  summarize(lat = mean(lat), 
            long = mean(long), 
            stream_order = mean(stream_order)) %>% 
  add_row(lat = 46.34519, 
          long = -119.2655,
          stream_order = 8) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs)

## Import USGS points
df_usgs <- read_csv("data/221114_usgs_d50_matched.csv") %>% 
  mutate(site_id = as.character(site_no)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = common_crs) %>% 
  select(site_id, stream_order)

## Merge into dataframe
df <- bind_rows(df_rc %>% mutate(source = "YOLO"), 
                df_usgs %>% mutate(source = "USGS"))

## Create an sf for the watershed boundary
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

## Read in flowlines using nhdplusTools
flowlines <- get_nhdplus(AOI = watershed)


# 3.  Plot it ------------------------------------------------------------------
p <- ggplot() + 
  geom_sf(data = watershed, fill = background_color) + 
  geom_sf(data = flowlines, aes(alpha = streamorde), 
          color = "#3279B2", show.legend = F) + 
  geom_sf(data = df, aes(shape = source), size = 4, color = "black") + 
  geom_sf(data = df, aes(color = stream_order, shape = source), size = 3, alpha = 0.8) + 
  #geom_sf(data = df %>% filter(site_id == "W20"), color = "black", size = 5) + 
  #geom_sf(data = df %>% filter(site_id == "U20"), color = "yellow", size = 5) + 
  labs(color = "Stream \n order", shape = "") + 
  scale_color_viridis_c(option = "D") + 
  theme_map() + 
  theme(legend.position = c(0.67, 0.52), 
        legend.box = "horizontal") + 
  guides(colour = guide_legend(order = 2), 
         shape = guide_legend(order = 1))

# p_test <- ggplot() + 
#   geom_sf(data = watershed, fill = background_color) + 
#   geom_sf(data = flowlines, color = "#3279B2", show.legend = F, alpha = 0.5) + 
#   geom_sf(data = flowlines %>% filter(gnis_name == "Yakima River"), color = "#3279B2", show.legend = F) + 
#   geom_sf(data = df, size = 3) + 
#   geom_sf(data = df, aes(color = stream_order, label = site_id), size = 2) + 
#   scale_color_viridis_c() + 
#   theme_map()
# 
# ggplotly(p_test)

## Add north arrow and 
p +
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("black", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black", "white"),
      line_col = "grey20")
  )
ggsave("figures/1_map_v2.png", width = 6, height = 5)


# 5. Set up the WA map ---------------------------------------------------------

## Bounding box for watershed
bbox <- st_bbox(watershed)
x_min <- bbox[[1]]
y_min <- bbox[[2]]
x_max <- bbox[[3]]
y_max <- bbox[[4]]

## Make a layer for the state outline
wa <- ne_states(country = "united states of america", returnclass = "sf") %>% 
  filter(name == "Washington") %>% 
  st_transform(crs = common_crs)

## Make the WA map figure
ggplot() + 
  geom_sf(data = wa, fill = "gray95") + 
  geom_sf(data = watershed, fill = background_color, color = "gray20") + 
  theme_map()
ggsave("figures/1_wa_map_for_Figure1.png", width = 4, height = 3)


x <- read_sf("data/gis/sites/022522_all_sites_NHD_streamline_matched.shp")

