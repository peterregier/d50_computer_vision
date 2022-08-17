
require(pacman)
p_load(tidyverse, cowplot, janitor)
theme_set(theme_bw())

yolo <- read_csv("data/220728/d50/d50_Train1_Prediction1_40.csv") %>% 
  clean_names() %>% 
  filter(label == 0 | label == 4) %>% 
  separate(name, into = c("study", "site", "date", "id")) %>% 
  filter(study != "PNNL") %>% 
  mutate(long_axis_mm = long_axis_m * 1000) %>% 
  select(study, site, label, long_axis_mm) %>% 
  mutate(label = case_when(label == 0 ~ "train", 
                           label == 4 ~ "test"))

plot_yolo <- yolo %>%  
  pivot_wider(names_from = "label", values_from = c("long_axis_mm")) 

m = round(summary(lm(test ~ train, data = plot_yolo))[[4]][2, 1], 2)
b = round(summary(lm(test ~ train, data = plot_yolo))[[4]][1, 1], 2)
r2 = round(summary(lm(test ~ train, data = plot_yolo))[[9]], 2)

fit_line = paste0("y = ", m, "x + ", b)

ggplot(plot_yolo, aes(train, test)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x = "Manual d50 (mm)", y = "Test d50 (mm)") + 
  annotate("text", x = 20, y = 42, label = fit_line) + 
  annotate("text", x = 20, y = 40, label = paste0("R2adj = ", r2)) 
ggsave("figures/220728_train_test_plot.png", width = 5, height = 4)

