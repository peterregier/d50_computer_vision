## This script is designed to create the final master dataset for the sites and
## their coordinates and stream orders for the YOLO paper. There have been 
## inconsistencies with the spatial points, and we're hoping to remedy that here.
## Two sets of coordinates are present: field coordinates ("_field") and 
## manually nhd-matched coordinates ("_nhd_match"). The nhd-matched coordinates
## will be fed to nhdplusTools to pull comids and stream orders. _field is 
## sourced from https://data.ess-dive.lbl.gov/view/doi:10.15485/1892052. 
## _nhd_match is sourced from 022522_all_sites_NHD_streamline_matched.shp
##
## 2022-11-30
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ------

require(pacman)
p_load(tidyverse,
       sf, 
       janitor,
       plotly,
       ggthemes,
       ggsflabel, # add labels
       nhdplusTools)

## Set a common crs so everything is projected the same
common_crs = 4326


# 2. Use Kaufmann shapefile with nhd-matched lat-longs to pull stream order ----

## First, load in the watershed for bounding data imports
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

## Load Kaufmann shapefile
nhd_matched_raw <- read_sf("data/gis/sites/022522_all_sites_NHD_streamline_matched.shp") %>% 
  clean_names() %>% 
  mutate(site_id = str_trim(site_id)) %>% 
  rename("stream_name" = stream_nam, 
         "nhd_long" = nhd_lon) %>% 
  select(site_id, stream_name, nhd_lat, nhd_long)

## Read in flowlines using nhdplusTools
flowlines_raw <- get_nhdplus(AOI = watershed) 

## Clean up for merging later
flowlines <- flowlines_raw%>% 
  rename("stream_order" = streamorde) %>% 
  select(comid, stream_order)

## Now, pull comids for each point
comid_raw <- nhdplusTools::get_flowline_index(flines = flowlines_raw, 
                                              points = nhd_matched_raw) %>% 
  clean_names() %>% 
  pull(comid)

## Next, attach comids to the shapefile
nhd_matched_comids <- nhd_matched_raw %>% 
  add_column(comid = comid_raw)

## Now, we're ready to merge with stream order
nhd_matched <- left_join(nhd_matched_comids, 
                         st_drop_geometry(flowlines), 
                         by = "comid")


# 3. Bring in field lat-longs --------------------------------------------------




# 4. Bring in YOLO estimates and trim to needed sites --------------------------
## File provided by YC on 7/28/22
yolo <- read_csv("data/220728/d50/d50_Train1_Prediction1_40.csv") %>% 
  clean_names() %>% 
  rename("type" = yolo) %>% 
  separate(name, into = c("study", "site_id", "date", "id")) %>% 
  filter(site_id != "label") %>% 
  mutate(d50_mm_yolo = long_axis_m * 1000) %>% 
  select(site_id, type, folder, label, d50_mm_yolo)

df <- right_join(nhd_matched, yolo, by = "site_id") %>% 
  group_by(site_id) %>% 
  summarize(stream_order = first(stream_order), 
            comid = first(comid))


df_for_ks <- df %>% 
  st_drop_geometry() %>% 
  mutate(ks_match = ifelse(comid %in% ks_comids_raw$comid, "TRUE", "FALSE"))

write_csv(df_for_ks, "data/221130_comids_for_ks.csv")

## Read in watershed variables from KS
ks_comids_raw <- read_csv("data/correlations/nhd_yrb_D50_variable.csv") %>% 
  clean_names() 

ks_comids <- flowlines %>% 
  filter(comid %in% ks_comids_raw$comid)



p <- ggplot() + 
  geom_sf(data = watershed) + 
  geom_sf(data = flowlines, show.legend = F, alpha = 0.2, color = "blue") + 
  geom_sf(data = flowlines_raw %>% filter(gnis_name == "Yakima River"), color = "#3279B2", show.legend = F) + 
  geom_sf(data = flowlines %>% filter(comid %in% df$comid), 
          aes(group = comid, label = stream_order), show.legend = F, color = "blue") + 
  geom_sf(data = ks_comids, 
          aes(group = comid), show.legend = F, color = "red", width = 1, alpha = 0.5) + 
  geom_sf(data = df, size = 3) + 
  geom_sf(data = df, aes(color = stream_order, label = site_id), size = 2) + 
  scale_color_viridis_c(option = "A") + 
  theme_map()

ggplotly(p)









