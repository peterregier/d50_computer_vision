## This script makes a boxplot figure showing the intra-site variability for all
## sites with > 3 photos to understand how much d50 estimates vary within sites
##
## 2022-09-19
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, cowplot, janitor)

## Set ggplot theme
theme_set(theme_bw())


# 2. Inter-site variability ----------------------------------------------------

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

p_intersite <- ggplot(yolo, aes(site)) + 
  geom_errorbar(aes(ymin = min, ymax = max), width = 0.3) + 
  geom_point(aes(y = mean), color = "coral", size = 3) + 
  geom_point(data = yolo %>% filter(site == "Dataset"), 
             aes(y = mean), color = "red", size = 4) +
  geom_text(aes(y = 10, label =  paste0("n=", n))) +
  labs(x = "", y = "d50 (mm)")


# 2. visual assessment ---------------------------------------------------------

yolo_visual <- read_csv("data/220728/d50/d50_Train1_Prediction1_40.csv") %>% 
  clean_names() %>% 
  filter(folder == "yes" | folder == "maybe"| folder == "no") %>% 
  separate(name, into = c("study", "site", "date", "id")) %>% 
  filter(study != "PNNL")


p_visual <- ggplot(yolo_visual, aes(fct_reorder(folder, yolo_accuracy_percent), yolo_accuracy_percent)) + 
  geom_boxplot(outlier.color = NA) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  labs(x = "Image suitability", y = "YOLO accuracy (%)")

plot_grid(p_intersite, p_visual, labels = c("A", "B"), nrow = 1)
ggsave("figures/220919_Figure5_intersite_and_vis.png", width = 8, height = 3)




ggsave("figures/220816_intra_site_variability.png", width = 5, height = 4)



