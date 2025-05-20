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
yolo_visual <- read_csv("data/d50_TrainNone_ValidNone_TestNone_Predict3b_35.csv") %>% 
  clean_names() %>% 
  separate(folder, into = c("x1", "x2", "folder")) %>% 
  filter(folder == "yes" | folder == "maybe"| folder == "no") %>% 
  separate(name, into = c("study", "site", "date", "id")) %>% 
  filter(study != "PNNL")

## For fun, let's compare datasets
df_raw <- read_csv("data/221115_rc2_master.csv") %>% 
  filter(type != "Train1") %>% 
  filter(folder == "yes" | folder == "maybe"| folder == "no")
  

# 3. Make Plot -----------------------------------------------------------------
ggplot(yolo_visual, aes(fct_reorder(folder, accuracy_threshold_percent), accuracy_threshold_percent)) + 
  geom_boxplot(outlier.color = NA) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  stat_compare_means(comparisons = list(c("no", "maybe"), 
                                        c("no", "yes"), 
                                        c("maybe", "yes")), 
                     label = "p.signif") + 
  labs(title = "Visual assessment of suitability of images for YOLO", 
       x = "Image suitability category", y = "YOLO accuracy (%)")
ggsave("figures/6_accuracy_v_visual_assessment_revised.png", width = 5, height = 5)
ggsave("figures/6_accuracy_v_visual_assessment_revised.pdf", width = 5, height = 5)
ggsave("figures/jpg/6_accuracy_v_visual_assessment_revised.jpg", width = 5, height = 5)

## For stats
ggplot(yolo_visual, aes(fct_reorder(folder, accuracy_threshold_percent), accuracy_threshold_percent)) + 
  geom_boxplot(outlier.color = NA) + 
  geom_jitter(alpha = 0.5, width = 0.1) + 
  stat_compare_means(comparisons = list(c("no", "maybe"), c("no", "yes"), c("maybe", "yes")))

## For stats in paper: 
compare_means(accuracy_threshold_percent ~ folder, data = yolo_visual)
yolo_visual %>% group_by(folder) %>% summarize(mean(accuracy_threshold_percent), 
                                           sd(accuracy_threshold_percent))
