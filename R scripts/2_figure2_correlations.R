## This script creates a multi-panel figure comparing manually integrated and
## YOLO-estimated values for 10 images used to train the model across three metrics.
## In the updated version, an additional panel is created that compares a 2x2 
## grid subsetting each image into 4 to increase n to 40
##
## 2022-08-16 (Updated 2022-11-16)
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


# 2. Create Panel A: d50 comparison --------------------------------------------

df_d50 <- read_csv("data/221115_rc2_master.csv") %>%
  filter(label == 0 | label == 4) %>% 
  mutate(label = case_when(label == 0 ~ "train",
                           label == 4 ~ "test")) %>% 
  select(label, d50_mm_yolo) %>% 
  pivot_wider(names_from = "label", 
              values_from = "d50_mm_yolo") %>% 
  unnest()

## Fit for actual v estimated d50
m = round(summary(lm(test ~ train, data = df_d50))[[4]][2, 1], 2)
b = round(summary(lm(test ~ train, data = df_d50))[[4]][1, 1], 2)
fit_line = paste0("y = ", m, "x + ", b)

## Calculate metrics to assess model performance
r2 = hydroGOF::gof(df_d50$train, df_d50$test)["R2", ]
r2_formatted = paste("R^2 == ", r2)
rmse = round((hydroGOF::rmse(df_d50$train, df_d50$test) / mean(df_d50$train)) * 100, 1) 


p_all <- ggplot(df_d50, aes(train, test)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", se = F) +
  geom_point() + 
  labs(title = "Full-image d50", x = "d50 (Train, mm)", y = "d50 (Test, mm)") +
  annotate("text", x = 20, y = 42, label = fit_line) + 
  annotate("text", x = 20, y = 42 * 0.95, label = r2_formatted, parse=TRUE) + 
  annotate("text", x = 20, y = 42 * 0.9, label = paste0("RMSE = ", rmse, "%")) + 
  annotate("text", x = 20, y = 42 * 0.85, label = "n = 10") + 
  theme(plot.title = element_text(hjust = 0.5))


# 3. Create Panel B: subset comparison -----------------------------------------

## Set the column names provided by YC
dat_colnames <- c("class", "x", "y", "width", "height", "prob", "long_axis")

## Set up functions to read in data
read_dat <- function(path){
  x <- read_delim(path, col_names = F)
  colnames(x) <- dat_colnames
  
  x$image = stringr::str_split(path, "/")[[1]][4]
  
  return(x)
}

## Set up function to process data into quadrats
process_data <- function(data){
  data %>% 
    mutate(x_bin = ifelse(x > 0.4, 1, 2), 
           y_bin = ifelse(y > 0.4, 1, 2)) %>% 
    group_by(x_bin, y_bin, site) %>% 
    summarize(n = n(), 
              median_mm = median(long_axis * 1000))
}


## Set common data path
data_path <- "data/TrainTest_IndividualGrains/"

## Read in specific files for test and train images
train_files <- list.files(paste0(data_path, "Train"), full.names = T)
test_files <- list.files(paste0(data_path, "Test"), full.names = T)

## Create a test dataset
test_df <- test_files[!grepl("PNNL", test_files)] %>% 
  map(read_dat) %>% 
  bind_rows() %>% 
  separate(image, sep = "_", into = c("campaign", "site", "scrap")) 

## Create a train dataset
train_df <- train_files[!grepl("PNNL", train_files)] %>% 
  map(read_dat) %>% 
  bind_rows() %>% 
  separate(image, sep = "_", into = c("campaign", "site", "scrap")) 

## Construct binned data
df_binned_raw <- inner_join(process_data(test_df) %>% 
  rename("n_test" = n, 
         "d50_test" = "median_mm"), 
  process_data(train_df) %>% 
  rename("n_train" = n, 
         "d50_train" = "median_mm"), 
  by = c("x_bin" = "x_bin", 
         "y_bin" = "y_bin", 
         "site" = "site")) 

df_binned <- df_binned_raw %>% 
  filter(d50_train < 75)


## Fit for actual v estimated d50
m1 = round(summary(lm(d50_test ~ d50_train, data = df_binned))[[4]][2, 1], 2)
b1 = round(summary(lm(d50_test ~ d50_train, data = df_binned))[[4]][1, 1], 2)
fit_line_bin = paste0("y = ", m1, "x + ", b1)

## Calculate metrics to assess model performance
r2_bin = hydroGOF::gof(df_binned$d50_train, df_binned$d50_test)["R2", ]
r2_bin_formatted = paste("R^2 == ", r2_bin)
rmse_bin = round((hydroGOF::rmse(df_binned$d50_train, df_binned$d50_test) / mean(df_binned$d50_train)) * 100, 1) 

## Make subset plot
p_subset <- ggplot(df_binned, aes(d50_train, d50_test)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", se = F) +
  geom_point() + 
  labs(title = "Subsetted image median grain size", 
       x = "d50 (Train, mm)", 
       y = "d50 (Test, mm)") + 
  scale_color_viridis_c() + 
  annotate("text", x = 20, y = 50, label = fit_line_bin) + 
  annotate("text", x = 20, y = 50 * 0.95, label = r2_bin_formatted, parse = TRUE) + 
  annotate("text", x = 20, y = 50 * 0.9, label = paste0("RMSE = ", rmse_bin, "%")) + 
  annotate("text", x = 20, y = 50 * 0.85, label = "n = 39") + 
  theme(plot.title = element_text(hjust = 0.5))


# 4. Combine graphs and export -------------------------------------------------

plot_grid(p_all, p_subset, labels = c("A", "B"), nrow = 1)
ggsave("figures/2_Figure2_y_by_x.png", width = 8, height = 4)


# 5. Make supplemental figure with outlier -------------------------------------

## Make subset plot that includes outlier
ggplot(df_binned_raw, aes(d50_train, d50_test)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", se = F) +
  geom_point() + 
  labs(title = "Subsetted image median grain size", x = "Median grain size (Train, mm)", y = "Median grain size (Test, mm)") + 
  scale_color_viridis_c() + 
  annotate("text", x = 20, y = 50, label = "n = 40") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/S1_Figure2B_with_outlier.png", width = 4, height = 4)


