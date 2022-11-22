## This script makes a boxplot figure showing the intra-site variability for all
## sites with > 3 photos to understand how much d50 estimates vary within sites
##
## 2022-08-16
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, cowplot)

## Set ggplot theme
theme_set(theme_bw())


# 2. Import data ---------------------------------------------------------------


df_raw <- read_csv("data/221115_rc2_master.csv") 

df_bin <- df_raw %>% 
  filter(type != "Train1") %>% 
  group_by(site_id) %>% 
  summarize(stream_order = median(stream_order), 
            d50_mm_yolo = median(d50_mm_yolo), 
            d50_mm_nexss = median(d50_mm_nexss), 
            d50_mm_abeshu = median(d50_mm_abeshu))

## 
df_sites <- df_raw %>% 
  group_by(site_id) %>% 
  filter(n() > 5) %>% 
  slice_sample(n = 6) %>% 
  summarize(stream_order = first(stream_order), 
            median = median(d50_mm_yolo), 
            sd = sd(d50_mm_yolo))

## Because this is random, the results change each time. To help consistency, 
## I'm writing out the dataset used so it's clear
write_csv(df_sites, "data/221122_fig6_data_subset.csv")

## Calculate full dataset stats
d50_median <- median(df_raw$d50_mm_yolo)
d50_sd <- sd(df_raw$d50_mm_yolo)

df_to_plot <- df_sites %>% 
  add_row(site_id = "All", 
          median = d50_median, 
          sd = d50_sd)

# 3. Create plot ---------------------------------------------------------------

n_fun <- function(x){
  return(data.frame(y = 10, label = paste0("n = ", yolo$n)))
}

p1 <- ggplot(df_to_plot, aes(site_id)) + 
  geom_errorbar(aes(ymin = median - sd, ymax = median + sd), width = 0.3) + 
  geom_point(aes(y = median), color = "coral", size = 3) + 
  geom_point(data = df_to_plot %>% filter(site_id == "All"), 
             aes(y = median), color = "red", size = 4) +
 # geom_text(aes(y = 10, label =  paste0("n=", n))) +
  labs(x = "", y = "d50 (mm)")

p2 <- ggplot(df_to_plot, aes(site_id, sd / median)) + 
  geom_col(fill = "coral", color = "black") + 
  geom_col(data = df_to_plot %>% filter(site_id == "All"), 
           fill = "red", color = "black") + 
  labs(x = "", y = "SD / median")

plot_grid(p1, p2, rel_heights = c(1, 0.5), ncol = 1, align = "hv", labels = c("A", "B"))
ggsave("figures/6_intra_site_variability.png", width = 6, height = 5)


