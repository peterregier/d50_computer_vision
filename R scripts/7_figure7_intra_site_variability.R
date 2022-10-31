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
  filter(study != "PNNL") %>% 
  mutate(long_axis_mm = long_axis_m * 1000)

sites_to_plot <- yolo_raw %>% 
  group_by(site) %>% 
  summarize(count = n()) %>%
  filter(count > 5)

d50_mean = mean(yolo_raw$long_axis_mm)
d50_sd = sd(yolo_raw$long_axis_mm)
d50_count = length(yolo_raw$long_axis_mm)

yolo <- yolo_raw %>% 
  filter(site %in% sites_to_plot$site)  %>% 
  group_by(site) %>% 
  summarize(min = mean(long_axis_mm) - sd(long_axis_mm), 
            mean = mean(long_axis_mm), 
            max = mean(long_axis_mm) + sd(long_axis_mm), 
            n = n()) %>% 
  add_row(site = "Dataset", 
          min = d50_mean - d50_sd, 
          mean = d50_mean, 
          max = d50_mean + d50_sd,
          n = d50_count)

n_fun <- function(x){
  return(data.frame(y = 10, label = paste0("n = ", yolo$n)))
}


ggplot(yolo, aes(site)) + 
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3) + 
  geom_point(aes(y = mean), color = "coral", size = 3) + 
  geom_point(data = yolo %>% filter(site == "Dataset"), 
             aes(y = mean), color = "red", size = 4) +
  geom_text(aes(y = 10, label =  paste0("n=", n))) +
  labs(x = "", y = "d50 (mm)")
ggsave("figures/7_intra_site_variability.png", width = 5, height = 4)


