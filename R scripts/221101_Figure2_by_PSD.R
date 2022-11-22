## This script calculates correlations between manually measured and YOLO-estimated
## particles as an alternative to the figure comparing d50 values (larger n)
##
## 2022-11-01
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
all_files <- as.tibble(list.files(path)) %>% 
  filter(grepl("longaxis", value)) %>% 
  filter(grepl("_40.txt", value)) %>%
  filter(!grepl("PNNL_label", value)) %>% 
  separate(value, into = c("campaign", "site", "date", "site_number", "type"), remove = F) %>% 
  select(value, site, site_number, type)

## Now, pick the 10 sites with "label"
labeled_sites <- all_files %>% 
  filter(type == "label")

files <- inner_join(all_files, labeled_sites %>% select(site, site_number), 
                    by = c("site" = "site", "site_number" = "site_number"))

## Set up a function to read in and label each file for each site
read_in_txt <- function(value = value, 
                        site = site, 
                        site_number = site_number, 
                        type = type){
  read_delim(paste0(path, "/", value), col_names = F) %>% 
    mutate("percent" = X7 * 100, 
           diameter_mm = X4 * 1000) %>% 
    select(diameter_mm, percent) %>% 
    mutate(site = site, 
           site_number = site_number, 
           type = type)
}

## Make the dataset
df <- files %>%
  pmap(read_in_txt) %>% 
  bind_rows()

df %>% 
  mutate(percent = round(percent, 0)) %>% 
  group_by(site, type, percent) %>% 
  summarize(diameter_mm = mean(diameter_mm)) %>% 
  pivot_wider(names_from = type, values_from = diameter_mm) %>% 
  ggplot(aes(label, longaxis)) + 
  geom_point(aes(color = site)) + 
  geom_smooth(se = F, method = "lm")



  


  



