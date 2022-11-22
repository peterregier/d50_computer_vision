## Plot all distributions on a single plot
##
## 2022-08-07
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse)


# 2. Import data ---------------------------------------------------------------

path <- "data/220728/Data"

file <- as.tibble(list.files(path)) %>% 
  filter(grepl("longaxis", value)) %>% 
  filter(grepl("_40.txt", value)) %>%
  filter(!grepl("PNNL_label", value)) %>% 
  separate(value, into = c("campaign", "site", "date", "site_number", "etc"), remove = F) %>% 
  select(value, site, site_number)


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


df %>% 
  #group_by(site, site_number) %>% 
  ggplot(aes(diameter_m, percent)) + 
  geom_path(aes(group = interaction(site, site_number)), alpha = 0.1) + 
  geom_smooth() + 
  geom_hline(yintercept = 50) + 
  scale_x_log10() +
  theme_bw()
ggsave("figures/220807_particle_size_distributions.png", width = 4, height = 3)






