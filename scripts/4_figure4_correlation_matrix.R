## This script constructs maps and boxplots to compare YOLO estimates to other
## d50 measurements/estimates
##
## 2022-10-06 (updated 2025-03-15)
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
       plotly,
       sf,
       party)


# 2. Bring in data -------------------------------------------------------------

df_raw <- read_csv("data/250314_rc2_master.csv") 

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
  st_transform(4326)

## Read in flowlines using nhdplusTools
flowlines <- get_nhdplus(AOI = watershed)

## Now, pull comids for each point
comids <- nhdplusTools::get_flowline_index(flines = flowlines, 
                                           points = df_sf)

## Add comids to d50 dataset
df_bin_comid <- df_bin %>%
 add_column(comid = comids %>% pull(COMID))

## Read in watershed variables from KS. Note that there are 4 COMIDs in here that
## do not match with the COMIDs pulled from NHD above. They are: 
## 24125567: this matches to site_id S28RE
## 24423655: this matches to site_id S49R
## 24128685: this matches to site_id S75
## 24423347: this matches to site_id W20
## These aren't a problem since we can match by site on the new datset
variables_raw <- read_csv("data/nhd_yrb_D50_variable_updated.csv") %>% 
  clean_names() %>% 
  select(-contains("d50")) %>% 
  group_by(site_id) %>% 
  summarize(across(everything(), mean))

df <- left_join(df_bin_comid, variables_raw, by = "site_id")


# 3. Set up correlations -------------------------------------------------------

## Create correlations for basin-scale variables
corr_basin <- df %>% 
  select(contains("d50"), 
         tot_pet,
         tot_prsnow, 
         tot_stream_slope,
         tot_stream_length,
         tot_basin_area, 
         tot_elev_mean, 
         turban) %>% 
  rename("Yolo D50 (mm)" = d50_mm_yolo, 
         "NEXSS D50 (mm)" = d50_mm_nexss, 
         "Abeshu D50 (mm)" = d50_mm_abeshu, 
         "Potential ET" = tot_pet, 
         "Precip. as Snow" = tot_prsnow, 
         "Stream Slope" = tot_stream_slope, 
         "Stream Length" = tot_stream_length, 
         "Basin Area" = tot_basin_area, 
         "Mean Elevation" = tot_elev_mean, 
         "% Urban" = turban) %>% 
  cor(method = "spearman") 

## Create correlations for catchment-scale variables
corr_catchment <- df %>% 
  select(contains("d50"), 
         cat_pet,
         cat_prsnow, 
         # cat_rfact,
         # cat_kfact,
         # cat_run7100,
         cat_stream_slope,
         cat_stream_length,
         cat_basin_area, 
         cat_elev_mean,
         urban) %>% 
  rename("Yolo D50 (mm)" = d50_mm_yolo, 
         "NEXSS D50 (mm)" = d50_mm_nexss, 
         "Abeshu D50 (mm)" = d50_mm_abeshu, 
         "Potential ET" = cat_pet, 
         "Precip. as Snow" = cat_prsnow, 
         "Stream Slope" = cat_stream_slope, 
         "Stream Length" = cat_stream_length, 
         "Basin Area" = cat_basin_area, 
         "Mean Elevation" = cat_elev_mean, 
         "% Urban" = urban) %>% 
  cor(method = "spearman") 


# 4. Make correlation plots ----------------------------------------------------

make_corrplot <- function(corr, title){
  
  pmat <- cor_pmat(cor(corr))
  
  ggcorrplot(corr, 
             outline.col = "white", 
             type = "upper", 
             lab = TRUE, 
             legend.title = "Correlation",
             #p.mat = pmat, 
             #sig.level = 0.001,
             colors = c("#DC3220", "white", "#005AB5")) + 
    ggtitle(title) + 
    theme(legend.position = c(0.75, 0.25))
}

plot_grid(make_corrplot(corr_basin, "Basin-scale correlations"), 
          make_corrplot(corr_catchment, "Catchment-scale correlations"), 
          nrow = 1, labels = c("A", "B"))
ggsave("figures/4_Figure4_correlations_revised.png", width = 12, height = 6)
ggsave("figures/4_Figure4_correlations_revised.pdf", width = 12, height = 6)
ggsave("figures/jpg/4_Figure4_correlations_revised.jpg", width = 12, height = 6)


# 5. Supplemental boxplots -----------------------------------------------------
make_boxplot <- function(var){
  
  mean_value = mean(df %>% drop_na() %>% pull({{var}}))
  ggplot(df, aes(factor(stream_order), {{var}})) + 
    geom_boxplot(outlier.alpha = 0, width = 0.2) + 
    geom_jitter(alpha = 0.5, width = 0.2) + 
    geom_hline(yintercept = mean_value) +
    xlab("Stream order")
}

plot_grid(make_boxplot(cat_stream_slope) + ylab("Slope (%)"), 
          make_boxplot(cat_elev_mean) + ylab("Elevation (m)"), ncol = 1)
ggsave("figures/7_S1_characteristics_by_streamorder_revised.png", width = 5, height = 4)

