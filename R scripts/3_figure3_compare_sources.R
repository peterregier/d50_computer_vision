## This script constructs maps and boxplots to compare YOLO estimates to other
## d50 measurements/estimates
##
## 2022-08-29 (Updated 2022-10-06)
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse, 
       sf,
       cowplot, 
       ggpubr, 
       janitor,
       ggallin) #pseudolog10_trans()

## Set theme
theme_set(theme_bw())

## Set CRS for all layers
common_crs = 4326

## set a manual colorblind-friendly color scheme for comparing methods
method_color_scheme = c("#1b9e77", "#d95f02", "#7570b3", "#1B1F3B")


# 2. Import map shapefiles -----------------------------------------------------

## Now, set up the outline of the watershed
watershed <- read_sf("data/gis/NHD_H_17030001_HU8_Shape/Shape/WBDHU4.shp") %>% 
  st_transform(common_crs)


# 3. Bring in datasets ---------------------------------------------------------

df_raw <- read_csv("data/221115_rc2_master.csv") 

df_bin <- df_raw %>% 
  filter(type != "Train1") %>% 
  group_by(site_id) %>% 
  summarize(stream_order = median(stream_order), 
            d50_mm_yolo = median(d50_mm_yolo), 
            d50_mm_nexss = median(d50_mm_nexss), 
            d50_mm_abeshu = median(d50_mm_abeshu))

df_long_raw <- df_bin %>% 
  pivot_longer(cols = c(contains("d50_mm")), names_to = "source", values_to = "d50_mm") 

## Finally, join with USGS
usgs_raw <- read_csv("data/221114_usgs_d50_matched.csv") %>% 
  mutate(source = "d50_mm_a_usgs", 
         site_id = as.character(site_no)) %>% 
  select(site_id, stream_order, source, d50_mm)

df_long <- bind_rows(df_long_raw, usgs_raw)


# 4. Create by-method boxplot (Panel A) ----------------------------------------

panel_a <- ggplot(df_long, aes(source, d50_mm)) + 
  geom_boxplot(aes(fill = source), alpha = 0.5, show.legend = F) + 
  scale_y_continuous(trans = "sqrt")  + 
  scale_fill_manual(values = method_color_scheme) +
  labs(x = "Estimate source", y = "d50 (mm)") + 
  scale_x_discrete(labels = c("USGS", "Abeshu", "NEXSS", "YOLO"))

## For stats in paper: 
compare_means(d50_mm ~ source, data = df_long)
df_long %>% group_by(source) %>% summarize(mean(d50_mm), 
                                           sd(d50_mm))

# 5. Create distribution plots (Panel B) ---------------------------------------

## First, set up some stuff (a list and function) to relabel facets
facet_names <- list(
  'd50_mm_a_usgs' = "USGS", 
  'd50_mm_abeshu' = "Abeshu",
  'd50_mm_nexss' = "NEXSS", 
  'd50_mm_yolo' = "YOLO"
)

relabeller <- function(variable,value){
  return(facet_names[value])
}

abeyshu_raw <- read_csv("data/abeshu_fig1_digitized.csv")

df_histogram <- df_long %>% mutate(d50_mm_log2 = log2(d50_mm))

panel_b <- ggplot() + 
  geom_col(data = abeyshu_raw, aes(d50_mm_log2, count), alpha = 0.5) + 
  geom_histogram(data = df_histogram, aes(d50_mm_log2, fill = source), 
                 color = "black", alpha = 0.5, show.legend = F) + 
  scale_fill_manual(values = method_color_scheme) +
  facet_wrap(~source, ncol = 1, labeller = relabeller) +
  labs(x = "log2 d50", fill = "")


# 6. Create stream-order -------------------------------------------------------

panel_c <- ggplot(df_long, aes(as.factor(stream_order), d50_mm)) + 
  geom_boxplot(aes(fill = as.factor(stream_order)), width = 0.5, show.legend = F) + 
  facet_wrap(~source, ncol = 1, scales = "free_y", labeller = relabeller) + 
  scale_fill_viridis_d() + 
  labs(x = "Strahler Stream Order", y = "d50 (mm)")


# 7. Combine and export --------------------------------------------------------

plot_grid(panel_a, panel_b, panel_c, 
          rel_widths = c(0.75, 1, 1), nrow = 1, labels = c("A", "B", "C"))
ggsave("figures/3_figure3.png", width = 9, height = 6)

