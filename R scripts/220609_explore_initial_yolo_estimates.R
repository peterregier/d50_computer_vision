## This script compares initial estimates for 37 images to NEXSS and Gupta
## estimates

require(pacman)
p_load(tidyverse, janitor, cowplot, corrplot)

# 2. Read in data --------------------------------------------------------------

## Read in NEXSS and Gupta d50 estimate datasets
nexss_gupta_raw <- read_csv("data/RC2_all_D50.csv") %>% 
  mutate(site_ID = str_trim(str_remove_all(site_ID, "[[:punct:]]"))) %>% 
  clean_names() %>% 
  rename("stream_order" = stream_orde)

## Read in YOLO d50 estimates
yolo_raw <- read_csv("data/220609_initial_d50_estimates.csv") %>% 
  separate(Photo_name, into = c("survey", "site_ID", "date", "something")) %>% 
  clean_names() %>% 
  mutate(d50_mm_width = d50_width_m * 1000, 
         d50_mm_height = d50_height_m * 1000, 
         d50_mm_long_axis = d50_long_axis_m * 1000)

## Create dataset for plotting
df_raw <- inner_join(nexss_gupta_raw, 
                     yolo_raw %>% select(site_id, contains("d50_mm")), 
                     by = "site_id")

## Last, read in the Abeyshu data: 
abeyshu_raw <- read_csv("data/abeyshu_fig1_digitized.csv")

ggplot(df_raw, aes(log_slope)) + 
  geom_point(aes(y = d50_mm_nexss), color = "blue") + 
  geom_point(aes(y = d50_mm_guta), color = "red") + 
  geom_point(aes(y = d50_mm_long_axis), color = "gray")

df_raw %>% 
  select(d50_mm_nexss, d50_mm_guta, d50_mm_long_axis) %>% 
  cor() %>% 
  corrplot()

ggplot(df_raw, aes(d50_mm_guta, d50_mm_nexss, color = stream_order)) + 
  geom_point() + 
  scale_color_viridis_c()




df_plot <- df_raw %>% select(site_id, d50_mm_nexss, d50_mm_guta, d50_mm_long_axis) %>% 
  pivot_longer(cols = contains("d50_mm"), names_to = "source", values_to = "d50_mm") %>% 
  mutate(d50_mm_log2 = log2(d50_mm), 
         source = str_replace_all(source, "d50_mm_", "")) 

ggplot() + 
  geom_col(data = abeyshu_raw, aes(d50_mm_log2, count), alpha = 0.5) + 
  geom_histogram(data = df_plot, aes(d50_mm_log2, fill = source), color = "black", alpha = 0.5) + 
  theme_bw() + 
  labs(x = "log2 d50", fill = "") + 
  theme_bw()
ggsave("figures/220609_d50_log2_comparison.pdf", width = 5, height = 4)



