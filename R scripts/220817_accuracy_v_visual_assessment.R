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
  filter(folder == "yes" | folder == "maybe"| folder == "no") %>% 
  separate(name, into = c("study", "site", "date", "id")) %>% 
  filter(study != "PNNL")
  

ggplot(yolo_raw, aes(fct_reorder(folder, yolo_accuracy_percent), yolo_accuracy_percent)) + 
  geom_boxplot(outlier.color = NA) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  labs(title = "Visual assessment", x = "Image suitability", y = "YOLO accuracy (%)")
ggsave("figures/220817_accuracy_v_visual_assessment.png", width = 3, height = 3)
