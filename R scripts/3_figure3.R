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
p_load(tidyverse, cowplot, janitor, purrr, hydroGOF)

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
  
  xmin = min(x$train)
  xmax = max(x$train)
  ymin = min(x$test)
  ymax = max(x$test)
  axis_min = min(xmin, ymin)
  axis_max = max(xmax, ymax)
  
  ## Fit for actual v estimated d50
  m = round(summary(lm(test ~ train, data = x))[[4]][2, 1], 2)
  b = round(summary(lm(test ~ train, data = x))[[4]][1, 1], 2)
  #r2 = round(summary(lm(test ~ train, data = x))[[9]], 2)
  fit_line = paste0("y = ", m, "x + ", b)
  
  ## Calculate metrics to assess model performance
  rmse = round((hydroGOF::rmse(x$train, x$test) / mean(x$train)) * 100, 1) 
  mae = round((hydroGOF::mae(x$train, x$test) / mean(x$train)) * 100, 1) 
  r2 = hydroGOF::gof(x$train, x$test)["R2", ]
  nse = round(hydroGOF::NSE(x$train, x$test), 2)
  
  x_position = max(x$train) * 0.5
  y_position = axis_max
  
  p <- ggplot(x, aes(train, test)) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_smooth(method = "lm", se = F) +
    geom_point() + 
    labs(title = title, x = x_lab, y = y_lab) +
    annotate("text", x = x_position, y = y_position, label = fit_line) + 
    annotate("text", x = x_position, y = y_position * 0.95, label = paste0("R2 = ", r2)) + 
    annotate("text", x = x_position, y = y_position * 0.9, label = paste0("NSE = ", nse)) + 
    annotate("text", x = x_position, y = y_position * 0.85, label = paste0("RMSE = ", rmse, "%")) + 
    annotate("text", x = x_position, y = y_position * 0.8, label = paste0("MAE = ", mae, "%")) + 
    scale_x_continuous(limits = c(axis_min, axis_max)) + 
    scale_y_continuous(limits = c(axis_min, axis_max)) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

p1 <- make_plots(plot_yolo, 
           "long_axis_mm_train", 
           "long_axis_mm_test", 
           "",
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

p1
ggsave("figures/3_figure3.png", width = 4, height = 4) 



\