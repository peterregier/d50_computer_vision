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
       bestNormalize,
       party)


# 2. Bring in data -------------------------------------------------------------

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
  select(comid, d50_mm_nexss, d50_mm_abeshu, stream_order) %>% 
  group_by(comid) %>% 
  summarize(across(everything(), mean))

df_raw <- right_join(nexss_guta, yolo_raw, by = "comid") %>% 
  select(-comid) %>% 
  drop_na()


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
  select(contains("d50"), 
         urban, 
         stream_order, 
         tot_stream_slope,
         tot_stream_length,
         tot_basin_slope,
         tot_basin_area, 
         tot_elev_mean, 
         tot_prsnow, 
         tot_pet,
         cat_stream_slope,
         cat_stream_length,
         cat_basin_slope,
         cat_basin_area, 
         cat_elev_mean,
         cat_prsnow, 
         cat_pet
  ) %>% 
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

plot_grid(make_corrplot(corr_tot, p.mat_tot, "Basin-scale correlations"), 
          make_corrplot(corr_cat, p.mat_cat, "Catchment-scale correlations"), 
          nrow = 1, labels = c("A", "B"))
ggsave("figures/6_Figure6_correlations.png", width = 12, height = 6)


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




