## This script creates a multi-panel figure showing 1) the strength of the 
## YOLO model as the correlation between manually integrated and modeled d50, and
## 2) the distribution of particle sizes
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
  select(study, site, label, long_axis_mm) %>% 
  mutate(label = case_when(label == 0 ~ "train", 
                           label == 4 ~ "test"))

plot_yolo <- yolo %>%  
  pivot_wider(names_from = "label", values_from = c("long_axis_mm")) 

m = round(summary(lm(test ~ train, data = plot_yolo))[[4]][2, 1], 2)
b = round(summary(lm(test ~ train, data = plot_yolo))[[4]][1, 1], 2)
r2 = round(summary(lm(test ~ train, data = plot_yolo))[[9]], 2)

fit_line = paste0("y = ", m, "x + ", b)


# 3. Import particle size datasets ---------------------------------------------

## Set the path for the files we'll read in
path <- "data/220728/particles_by_site"

## Create a list of files to read in
file <- as.tibble(list.files(path)) %>% 
  filter(grepl("longaxis", value)) %>% 
  filter(grepl("_40.txt", value)) %>%
  filter(!grepl("PNNL_label", value)) %>% 
  separate(value, into = c("campaign", "site", "date", "site_number", "etc"), remove = F) %>% 
  select(value, site, site_number)

## Set up a function to read in and label each file for each site
read_in_txt <- function(value = value, 
                        site = site, 
                        site_number = site_number){
  read_delim(paste0(path, "/", value), col_names = F) %>% 
    rename("diameter_m" = X4) %>% 
    mutate("percent" = X7*100) %>% 
    select(diameter_m, percent) %>% 
    mutate(site = site, 
           site_number = site_number)
}

## Make the dataset
df <- file %>%
  pmap(read_in_txt) %>% 
  bind_rows()


# 5. Create plots --------------------------------------------------------------

## Model fit plot
p0 <- ggplot(plot_yolo, aes(train, test)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x = "Manual d50 (mm)", y = "Test d50 (mm)") + 
  annotate("text", x = 20, y = 42, label = fit_line) + 
  annotate("text", x = 20, y = 40, label = paste0("R2adj = ", r2)) 
#ggsave("figures/220728_train_test_plot.png", width = 5, height = 4)

p1 <- ggplot(df, aes(diameter_m, percent)) + 
  geom_path(aes(group = interaction(site, site_number)), alpha = 0.1) + 
  geom_smooth() + 
  geom_hline(yintercept = 50) + 
  scale_x_log10() +
  theme_bw()

plot_grid(p0, p1, nrow = 1)
ggsave("figures/figure3.png", width = 8, height = 4) 
