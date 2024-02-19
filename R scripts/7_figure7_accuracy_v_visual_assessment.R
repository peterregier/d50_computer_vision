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

## This script uses older data that has accuracy. 
yolo_visual <- read_csv("data/d50_Train1_Prediction1_40.csv") %>% 
  clean_names() %>% 
  filter(folder == "yes" | folder == "maybe"| folder == "no") %>% 
  separate(name, into = c("study", "site", "date", "id")) %>% 
  filter(study != "PNNL")

## For fun, let's compare datasets
df_raw <- read_csv("data/221115_rc2_master.csv") %>% 
  filter(type != "Train1") %>% 
  filter(folder == "yes" | folder == "maybe"| folder == "no")
  

# 3. Make Plot -----------------------------------------------------------------
ggplot(yolo_visual, aes(fct_reorder(folder, yolo_accuracy_percent), yolo_accuracy_percent)) + 
  geom_boxplot(outlier.color = NA) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  geom_rect(xmin = 2.8, xmax = 3.2, ymin = 48, ymax = 53, fill = NA, color = "red") + 
  labs(title = "Visual assessment", x = "Image suitability", y = "YOLO accuracy (%)")
ggsave("figures/7_accuracy_v_visual_assessment.png", width = 3, height = 3)
ggsave("figures/7_accuracy_v_visual_assessment.pdf", width = 3, height = 3)

## For stats in paper: 
compare_means(yolo_accuracy_percent ~ folder, data = yolo_visual)
yolo_visual %>% group_by(folder) %>% summarize(mean(yolo_accuracy_percent), 
                                           sd(yolo_accuracy_percent))
