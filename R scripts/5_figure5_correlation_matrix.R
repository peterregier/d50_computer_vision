## This script constructs maps and boxplots to compare YOLO estimates to other
## d50 measurements/estimates
##
## 2022-10-06
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       janitor,
       cowplot, 
       corrplot, 
       ggcorrplot,
       ggsflabel,
       bestNormalize,
       nhdplusTools,
       party)


# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/221115_rc2_master.csv") 

df_bin <- df_raw %>% 
  group_by(site_id) %>% 
  summarize(stream_order = median(stream_order),
            lat = median(lat), 
            long = median(long),
            d50_mm_yolo = median(d50_mm_yolo), 
            d50_mm_nexss = median(d50_mm_nexss), 
            d50_mm_abeshu = median(d50_mm_abeshu))

df_sf <- st_as_sf(df_bin, coords = c("long", "lat"), crs = 4326)

## Create an sf for the watershed boundary
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)

## Read in flowlines using nhdplusTools
flowlines <- get_nhdplus(AOI = watershed)

## Now, pull comids for each point
comids <- nhdplusTools::get_flowline_index(flines = flowlines, 
                                           points = df_sf)

df_bin_comid <- df_bin %>%
 add_column(comid = comids %>% pull(COMID))


## Read in watershed variables from KS
variables_raw <- read_csv("data/correlations/nhd_yrb_D50_variable.csv") %>% 
  clean_names() %>% 
  select(-contains("d50")) %>% 
  group_by(comid) %>% 
  summarize(across(everything(), mean))

df <- left_join(df_bin_comid, variables_raw, by = "comid")

df %>% filter(if_any(everything(), is.na)) %>% 
  pull(comid)

## Here are the problem sites. Likely what is happening is I'm pulling COMIDs
## from the nhdplusTools package, and those are slightly different than the 
## COMIDs that KS assigned. Since there are no site names attached to the 
## spreadsheet provided by KS, that makes things difficult. 
df %>% filter(if_any(everything(), is.na)) %>% select(site_id, comid)

## Here are the sites in df_bin_comid that aren't in KS layer
setdiff(df_bin_comid$comid, variables_raw$comid)

## And here are the comids in KS layer that don't match
old_comids = setdiff(variables_raw$comid, df_bin_comid$comid)

reassigned_comids <- tibble(old_comid = old_comids, 
                            site = c("T07", "S26R", NA, NA, "S28RE", "S75"))

## Old_comids #3 is WRONG.
## old comid #4 is WRONG.

site = old_comids[1]

sf1 <- df %>% filter(if_any(everything(), is.na)) %>% 
  select(site_id, lat, long) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot() + 
  geom_sf(data = flowlines, color = "gray95") + 
  geom_sf(data = sf1, alpha = 0.3) + 
  geom_sf_label(data = sf1, aes(label = site_id), nudge_y = 0.1) + 
  geom_sf(data = flowlines %>% filter(comid %in% old_comids), color = "blue")



#write_csv(df_bin2, "data/221121_binned_sites_w_comid.csv")

yolo_raw <- read_csv("data/correlations/nhd_yrb_D50_variable.csv") %>% 
  clean_names() %>% 
  mutate(d50_mm_yolo = d50m_yolo * 1000) %>% 
  select(-d50m_yolo) %>% 
  group_by(comid) %>% 
  summarize(across(everything(), mean))

nexss_guta <- read_csv("data/RC2_all_D50.csv") %>% 
  clean_names() %>% 
  rename("d50_mm_abeshu" = d50_mm_guta) %>% 
  mutate(site_id = str_trim(str_remove_all(site_id, "[[:punct:]]"))) %>% 
  rename("stream_order" = stream_orde) %>% 
  select(comid, site_id, d50_mm_nexss, d50_mm_abeshu, stream_order) %>% 
  group_by(comid) %>% 
  summarize(site_id = first(site_id), 
            across(-site_id, mean))
  #summarize(across(everything(), mean))

df_raw <- right_join(nexss_guta, yolo_raw, by = "comid") %>% 
  select(-comid) %>% 
  drop_na()

setdiff(df_bin$site_id, df_raw$site_id)


# 3. Transform data ------------------------------------------------------------

my_list <- df_raw %>% select_if(is.numeric) %>% colnames()

yj_normalize <- function(var){
  x <- df_raw %>% dplyr::pull({{var}})
  bc <- bestNormalize::yeojohnson(x) #Boxcox can't handle negative values
  p <- predict(bc)
  return(p)
}

n_list <- list()
for(i in 1:length(my_list)){
  n_list[[i]] <- df_raw %>% 
    mutate(x = yj_normalize(my_list[[i]])) %>% 
    select(x)
}

yj_df <- do.call(cbind.data.frame, n_list)
colnames(yj_df) <- my_list

yolo_n <- as_tibble(yj_df)

corr_tot <- yolo_n %>% 
  select(contains("d50"), 
         tot_pet,
         tot_rfact,
         tot_kfact,
         tot_run7100,
         tot_stream_slope,
         tot_stream_length,
         tot_basin_slope,
         tot_basin_area, 
         tot_elev_mean, 
         tot_prsnow, 
         tot_pet, 
         turban) %>% 
  cor(method = "spearman") 


corr_cat <- yolo_n %>% 
  select(contains("d50"), 
         tot_pet,
         cat_rfact,
         cat_kfact,
         cat_run7100,
         cat_stream_slope,
         cat_stream_length,
         cat_basin_slope,
         cat_basin_area, 
         cat_elev_mean,
         cat_prsnow, 
         cat_pet,
         urban, 
     ) %>% 
  cor(method = "spearman") 


corr_all <- yolo_n %>% 
  cor(method = "spearman") 

p.mat_tot <- cor_pmat(cor(corr_tot))
p.mat_cat <- cor_pmat(cor(corr_cat))
p.mat_all <- cor_pmat(cor(corr_all))

make_corrplot <- function(corr, pmat, title){
  ggcorrplot(corr, 
             outline.col = "white", 
             type = "upper", 
             lab = TRUE, 
             p.mat = pmat, 
             sig.level = 0.05,
             colors = c("red", "white", "blue")) + 
    ggtitle(title) + 
    theme(legend.position = c(0.75, 0.25))
}


make_corrplot(corr_all, p.mat_all, "All correlations")


plot_grid(make_corrplot(corr_tot, p.mat_tot, "Basin-scale correlations"), 
          make_corrplot(corr_cat, p.mat_cat, "Catchment-scale correlations"), 
          nrow = 1, labels = c("A", "B"))
ggsave("figures/x_Figure6_correlations.png", width = 12, height = 6)


y_mlr <- bind_cols(yolo_n %>% select(d50_mm_yolo), yolo_n %>% select(-contains("d50")))

y_mlr2 <- y_mlr %>% 
  select(d50_mm_yolo, urban, cat_elev_mean, tot_evi_jas_2012, tot_prsnow, 
         tot_pet, sinuosity, tot_basin_area)

ranger(d50_mm_yolo ~ ., data = y_mlr)


write_csv(y_mlr, "data/y_mlr.csv")

yolo_n %>% 
  select(contains("d50"), urban) %>% 
  pivot_longer(cols = c(contains("d50"))) %>% 
  ggplot(aes(urban, value, color = name)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~name, ncol = 1) +
  theme_bw()




