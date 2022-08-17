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
yolo_raw <- read_csv("data/220728/d50/d50_Train1_Prediction1_40.csv") %>% 
  clean_names() %>% 
  filter(folder == "yes" | folder == "maybe") %>% 
  separate(name, into = c("study", "site", "date", "id")) %>% 
  filter(study != "PNNL") 

sites_to_plot <- yolo_raw %>% 
  group_by(site) %>% 
  summarize(count = n()) %>%
  filter(count > 5)

yolo <- yolo_raw %>% 
  filter(site %in% sites_to_plot$site) %>% 
  mutate(long_axis_mm = long_axis_m * 1000) %>% 
  group_by(site) %>% 
  summarize(min = min(long_axis_mm), 
            mean = mean(long_axis_mm), 
            max = max(long_axis_mm))

ggplot(yolo, aes(site)) + 
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3) + 
  geom_point(aes(y = mean), color = "red", size = 4) + 
  labs(x = "", y = "d50 (mm)")
ggsave("figures/220816_intra_site_variability.png", width = 3, height = 3)



