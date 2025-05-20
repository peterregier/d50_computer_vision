## This script constructs maps and boxplots to compare YOLO estimates to other
## d50 measurements/estimates
##
## 2022-08-29 (Updated 2025-03-15)
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

df_raw <- read_csv("data/250314_rc2_master.csv") 

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
  #rename("d50_mm" = d50_mm_usgs) %>% 
  mutate(source = "d50_mm", 
         site_id = as.character(site_no)) %>% 
  dplyr::select(site_id, stream_order, source, d50_mm)

df_long <- bind_rows(df_long_raw, usgs_raw)


# 4. Create by-method boxplot (Panel A) ----------------------------------------

my_comparisons = list(c("d50_mm", "d50_mm_abeshu"), 
                      c("d50_mm", "d50_mm_nexss"), 
                      c("d50_mm", "d50_mm_yolo"), 
                      c("d50_mm_abeshu", "d50_mm_nexss"), 
                      c("d50_mm_abeshu", "d50_mm_yolo"), 
                      c("d50_mm_yolo", "d50_mm_nexss"))

panel_a <- ggplot(df_long, aes(source, d50_mm)) + 
  geom_boxplot(aes(fill = source), alpha = 0.5, show.legend = F) + 
  scale_y_continuous(trans = "sqrt")  + 
  scale_fill_manual(values = method_color_scheme) +
  labs(x = "Estimate source", y = "D50 (mm)") + 
  scale_x_discrete(labels = c("USGS", "Abeshu", "NEXSS", "YOLO")) + 
  stat_compare_means(comparisons = my_comparisons, 
                     label = "p.signif")

## For stats in paper: 
compare_means(d50_mm ~ source, data = df_long)
df_long %>% group_by(source) %>% summarize(mean(d50_mm), 
                                           sd(d50_mm))

# 5. Create distribution plots (Panel B) ---------------------------------------

## First, set up some stuff (a list and function) to relabel facets
facet_names <- list(
  'd50_mm' = "USGS", 
  'd50_mm_abeshu' = "Abeshu",
  'd50_mm_nexss' = "NEXSS", 
  'd50_mm_yolo' = "YOLO"
)

relabeller <- function(variable,value){
  return(facet_names[value])
}

abeshu_raw <- read_csv("data/abeshu_fig1_digitized.csv")

df_histogram <- df_long %>% mutate(d50_mm_log2 = log2(d50_mm))

panel_b <- ggplot() + 
  geom_col(data = abeshu_raw, aes(d50_mm_log2, count), alpha = 0.5) + 
  geom_histogram(data = df_histogram, aes(d50_mm_log2, fill = source), 
                 color = "black", alpha = 0.5, show.legend = F) + 
  scale_fill_manual(values = method_color_scheme) +
  facet_wrap(~source, ncol = 1, labeller = relabeller) +
  labs(x = "log2 D50 (mm)", y = "Count of measurements", fill = "")
ggsave("figures/230523_density_plots_revised.png", width = 3, height = 6)


# 6. Create stream-order -------------------------------------------------------

panel_c <- ggplot(df_long, aes(as.factor(stream_order), d50_mm)) + 
  geom_boxplot(aes(fill = as.factor(stream_order)), width = 0.5, show.legend = F) + 
  facet_wrap(~source, ncol = 1, scales = "free_y", labeller = relabeller) + 
  scale_fill_viridis_d() + 
  labs(x = "Strahler Stream Order", y = "D50 (mm)")


# 7. Combine and export --------------------------------------------------------

plot_grid(panel_a, panel_b, panel_c, 
          rel_widths = c(0.75, 1, 1), nrow = 1, labels = c("A", "B", "C"))
ggsave("figures/2_figure2_revised.png", width = 9, height = 6)
ggsave("figures/2_Figure2_revised.pdf", width = 9, height = 6)
ggsave("figures/jpg/2_Figure2_revised.jpg", width = 9, height = 6)

# 8. Make supplemental figure --------------------------------------------------

ggplot(df_long, aes(as.factor(stream_order), d50_mm)) + 
  geom_boxplot(aes(fill = as.factor(stream_order)), width = 0.5, show.legend = F) + 
  facet_wrap(~source, ncol = 1, labeller = relabeller) + 
  scale_fill_viridis_d() + 
  labs(x = "Strahler Stream Order", y = "D50 (mm)")
ggsave("figures/SA_Figure_2C_revised.png", width = 3.5, height = 6)


