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


df_raw <- read_csv("data/221115_rc2_master.csv") 

df_bin <- df_raw %>% 
  filter(type != "Train1") %>% 
  group_by(site_id) %>% 
  summarize(stream_order = median(stream_order), 
            d50_mm_yolo = median(d50_mm_yolo), 
            d50_mm_nexss = median(d50_mm_nexss), 
            d50_mm_abeshu = median(d50_mm_abeshu))

## Find sites with more than 5 values
df_sites <- df_raw %>% 
  group_by(site_id) %>% 
  filter(n() > 5) %>% 
  slice_sample(n = 6)
  
## Function to calculate mean and sd 1000 times for each site
calc_variability_stats <- function(site){
  
  x <- df_sites %>% 
    filter(site_id == site)
  
  stat_list <- list(mean = NA, sd = NA)
  for(i in 1:1000){
    
    y <- x %>% slice_sample(n = 5)
    stat_list$mean[[i]] = mean(y$d50_mm_yolo)
    stat_list$sd[[i]] = sd(y$d50_mm_yolo)
  }
  
  z <- as_tibble(stat_list) %>% 
    mutate(site_id = site) %>% 
    summarize(site = first(site), 
              mean = mean(mean), 
              sd = mean(sd))

  print(site)
  
  return(z)
}

calc_variability_stats_dataset <- function(data){
  
  stat_list <- list(mean = NA, sd = NA)
  for(i in 1:1000){
    
    y <- data %>% slice_sample(n = 5)
    stat_list$mean[[i]] = mean(y$d50_mm_yolo)
    stat_list$sd[[i]] = sd(y$d50_mm_yolo)
  }
  
  z <- as_tibble(stat_list) %>% 
    mutate(site = "All") %>% 
    summarize(site = first(site), 
              mean = mean(mean), 
              sd = mean(sd))
  
  return(z)
}

df_stats <- unique(df_sites %>% pull(site_id)) %>% 
  map(calc_variability_stats) %>% 
  bind_rows() %>% 
  add_row(calc_variability_stats_dataset(df_bin)) %>% 
  mutate(sd_mean = sd / mean)


# 3. Create plot ---------------------------------------------------------------

n_fun <- function(x){
  return(data.frame(y = 10, label = paste0("n = ", yolo$n)))
}

p1 <- ggplot(df_stats, aes(site)) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.3) + 
  geom_point(aes(y = mean), color = "coral", size = 3) + 
  geom_point(data = df_stats %>% filter(site == "All"), 
             aes(y = mean), color = "red", size = 4) +
  #geom_text(aes(y = 10, label =  paste0("n=", n))) +
  labs(x = "", y = "d50 (mm)")

p2 <- ggplot(df_stats, aes(site, sd / mean)) + 
  geom_col(fill = "coral", color = "black") + 
  geom_col(data = df_stats %>% filter(site == "All"), 
           fill = "red", color = "black") + 
  labs(x = "", y = "SD / mean")

plot_grid(p1, p2, rel_heights = c(1, 0.5), ncol = 1, align = "hv", labels = c("A", "B"))
ggsave("figures/6_intra_site_variability.png", width = 6, height = 5)
ggsave("figures/6_intra_site_variability.pdf", width = 6, height = 5)
ggsave("figures/jpg/6_intra_site_variability.jpg", width = 6, height = 5)

