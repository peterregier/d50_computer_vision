## Try and find a way to compare particles between train and test image output
##

# 1. Setup -----

require(pacman)
p_load(cowplot, 
       tidyverse)

data_path <- "data/train_test_comparison/"

dat_colnames <- c("class", "x", "y", "width", "height", "prob", "long_axis")

read_dat <- function(path, type){
  x <- read_delim(paste0(data_path, path), col_names = F)
  colnames(x) <- dat_colnames
  x$type = type
  
  return(x)
}

process_data <- function(data){
  data %>% 
    mutate(x_bin = ntile(x, 4), 
           y_bin = ntile(y, 4)) %>% 
    group_by(x_bin, y_bin) %>% 
    summarize(n = n(), 
              median_mm = median(long_axis * 1000), 
              type = first(type))
}


test <- process_data(read_dat("SPS_S15_20210901_18_label_test.dat", "test"))
train <- process_data(read_dat("SPS_S15_20210901_18_label_train.dat", "train"))

df <- inner_join(test %>% select(-type), 
           train %>% select(-type), 
           by = c("x_bin", "y_bin"), 
           suffix = c("_test", "_train"))


p0 <- read_dat("SPS_S15_20210901_18_label_test.dat", "test") %>% 
  mutate(x_bin = ntile(x, 4), 
         y_bin = ntile(y, 4)) %>% 
  ggplot(aes(x, y, color = interaction(x_bin, y_bin))) + 
  geom_point() +
  ggtitle("raw data") + 
  scale_color_viridis_c()

p0_grid <- read_dat("SPS_S15_20210901_18_label_test.dat", "test") %>% 
  ggplot(aes(x, y, color = long_axis)) + 
  geom_point() +
  geom_hline(yintercept = 0.2, color = "red") +
  geom_hline(yintercept = 0.4, color = "red") +
  geom_hline(yintercept = 0.6, color = "red") +
  geom_vline(xintercept = 0.2, color = "red") +
  geom_vline(xintercept = 0.4, color = "red") +
  geom_vline(xintercept = 0.6, color = "red") +
  ggtitle("raw data")

p1 <- ggplot(df, aes(n_train, n_test)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("# of grains (S15)")
  

p2 <- ggplot(df, aes(median_mm_train, median_mm_test)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("median grain size (mm, S15)")
          
plot_grid(p0, p1, p2, nrow = 1, rel_widths = c(1, 0.7, 0.7))
ggsave("figures/binning_grains_for_fig2.png", width = 12, height = 3)





