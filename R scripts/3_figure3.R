## This script creates a multi-panel figure comparing manually integrated and
## YOLO-estimated values for 10 images used to train the model across three metrics
##
## 2022-08-16
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, cowplot, janitor, purrr)

## Set ggplot theme
theme_set(theme_bw())


# 2. Import model fit data and calculate stats ---------------------------------

yolo <- read_csv("data/220728/d50/d50_Train1_Prediction1_40.csv") %>% 
  clean_names() %>% 
  filter(label == 0 | label == 4) %>% 
  separate(name, into = c("study", "site", "date", "id")) %>% 
  filter(study != "PNNL") %>% 
  mutate(long_axis_mm = long_axis_m * 1000) %>% 
  select(study, site, label, long_axis_mm, yolo_box_number, area_covered_percent) %>% 
  mutate(label = case_when(label == 0 ~ "train", 
                           label == 4 ~ "test"))

plot_yolo <- yolo %>%  
  pivot_wider(names_from = "label", values_from = c("long_axis_mm",
                                                    "yolo_box_number", 
                                                    "area_covered_percent")) 


# 3. Create plots --------------------------------------------------------------

make_plots <- function(data, 
                       train_col, 
                       test_col,
                       title,
                       x_lab, 
                       y_lab){
  x <- data %>% 
    select(train_col, test_col) %>% 
    rename("train" = train_col,
           "test" = test_col) 
  
  ## Fit for actual v estimated d50
  m = round(summary(lm(test ~ train, data = x))[[4]][2, 1], 2)
  b = round(summary(lm(test ~ train, data = x))[[4]][1, 1], 2)
  r2 = round(summary(lm(test ~ train, data = x))[[9]], 2)
  fit_line = paste0("y = ", m, "x + ", b)
  
  x_position = max(x$train) * 0.5
  y_position = max(x$test)
  
  p <- ggplot(x, aes(train, test)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = F) + 
    labs(title = title, x = x_lab, y = y_lab) +
    annotate("text", x = x_position, y = y_position, label = fit_line) + 
    annotate("text", x = x_position, y = y_position * 0.95, label = paste0("R2adj = ", r2)) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

p1 <- make_plots(plot_yolo, 
           "long_axis_mm_train", 
           "long_axis_mm_test", 
           "d50",
           "Manual: d50 (mm)", 
           "YOLO: d50 (mm)")

p2 <- make_plots(plot_yolo, 
           "yolo_box_number_train", 
           "yolo_box_number_test", 
           "Particles identified",
           "Manual: # particles", 
           "YOLO: # particles")

p3 <- make_plots(plot_yolo, 
           "area_covered_percent_train", 
           "area_covered_percent_test", 
           "Area covered by particles",
           "Manual: coverage (%)", 
           "YOLO: coverage (%)")

plot_grid(p2, p3, p1, nrow = 1)
ggsave("figures/figure3.png", width = 9, height = 4) 
