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


# 2. Import YOLO particle sizes ------------------------------------------------

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

p1 <- ggplot(df, aes(diameter_m, percent)) + 
  geom_path(aes(group = interaction(site, site_number)), alpha = 0.1) + 
  geom_smooth() + 
  geom_hline(yintercept = 50) + 
  scale_x_log10() +
  theme_bw()