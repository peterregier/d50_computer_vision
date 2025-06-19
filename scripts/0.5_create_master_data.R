## This script is designed to construct a trustworthy dataset containing site
## characteristics (stream order, location) and d50 estimates for YOLO, 
## NEXSS, and Abeshu. Since data provenance has been an issue, the source of
## each data file used to construct master data will be marked in comments
## 
## 2022-11-15
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse)


# 2. Import raw datasets -------------------------------------------------------

## This spreadsheet provided from VGC via teams on 11/9/22 in chat with JS/YC
stream_order_raw <- read_csv("data/RC2_all_stream_Attributes_updated.csv") %>% 
  clean_names() %>% 
  #rename("stream_order" = stream_orde) %>% 
  select(stream_order, site_id)

## This spreadsheet is missing two sites (W20 and U20), which are manually added.
## Stream orders from these sites are taken from an older file originally provided
## by MK: "RC2_all_stream_Attributes_updated_UNTRUSTED.csv" 
stream_order_edited <- stream_order_raw %>% 
  add_row(stream_order = 4, site_id = "W20")  %>% 
  add_row(stream_order = 3, site_id = "U20")

## Second, import lat-longs from a file also provided by VGC (same chat)
site_info_raw <- read_csv("data/RC2 Spatial Study_Responses_Form Responses 1_updated 2022-02-01.csv") %>% 
  clean_names() %>% 
  rename("site_id" = id) %>% 
  select(site_id, long, lat) 

## This also is missing W20 and U20, lat-longs are manually added from spreadsheet: 
## https://pnnl.sharepoint.com/:x:/r/teams/RC-3RiverCorridorSFA/_layouts/15/Doc.aspx?sourcedoc=%7BF6515F21-CAA3-463A-B89E-119DEF7CC6D5%7D&file=RC3_All_Site_Details_and_Permit_Status.xlsx&action=default&mobileredirect=true
site_info_edited <- site_info_raw %>% 
  add_row(site_id = "W20", long = -120.710935, lat = 46.832844)  %>% 
  add_row(site_id = "U20", long = -120.642142, lat = 46.898654)

## Third, add YOLO estimates from file provided by YC on 7/28/22
yolo <- #read_csv("data/d50_Train1_Prediction1_40.csv") %>% 
  read_csv("data/d50_TrainNone_ValidNone_TestNone_Predict3b_35.csv") %>% 
  clean_names() %>% 
  mutate(type = "yolo") %>% 
  separate(name, into = c("study", "site_id", "date", "id")) %>% 
  filter(site_id != "label") %>% 
  mutate(d50_mm_yolo = d50_area_meter * 1000) %>% 
  dplyr::select(site_id, type, folder, label, d50_mm_yolo) %>% 
  unique()

yolo %>% 
  dplyr::select(site_id, d50_mm_yolo) %>% 
  unique()

## Fourth, add NEXSS and Abeshu estimates provided by KS
nexss_abeshu <- read_csv("data/RC2_all_D50.csv") %>% 
  clean_names() %>% 
  mutate(site_id = str_trim(str_remove_all(site_id, "[[:punct:]]"))) %>% 
  rename("d50_mm_abeshu" = d50_mm_guta) %>% 
  select(site_id, d50_mm_nexss, d50_mm_abeshu) %>% 
  filter(site_id %in% yolo$site_id)

## Join into a single dataframe
df <- inner_join(stream_order_edited, site_info_edited) %>% 
  inner_join(yolo) %>% 
  inner_join(nexss_abeshu)

## Double-check that the 40 sites present in the YOLO dataset are all present
unique(df$site_id)

#write_csv(df, "data/221115_rc2_master.csv")
write_csv(df, "data/250314_rc2_master.csv")


yolo_stats <- read_csv("data/d50_TrainNone_ValidNone_TestNone_Predict3b_35.csv") %>% 
  clean_names()
  
## What is the resolution of images in mm/pixel
yolo_stats %>% 
  summarize(min(l1l2_res_millimeter_per_pixel), 
            mean(l1l2_res_millimeter_per_pixel), 
            median(l1l2_res_millimeter_per_pixel), 
            max(l1l2_res_millimeter_per_pixel))

## What are minimum and maximum grain sizes detected?
yolo_stats %>% 
  mutate(minimum_size_mm = minimum_size_meter * 1000) %>% 
  summarize(min(minimum_size_mm), 
            max(maximum_size_meter * 1000))

maximum_size_meter

unique(yolo_stats$name)




calc_stats <- function(var){
  yolo_stats %>% 
    summarize(min({{var}}), 
              mean({{var}}), 
              median({{var}}), 
              max({{var}}))
}

calc_stats(l1_res_millimeter_per_pixel)

yolo_stats %>% 
  summarize(min(l1_res_millimeter_per_pixel), 
            mean(l1l2_res_millimeter_per_pixel), 
            median(l1l2_res_millimeter_per_pixel), 
            max(l1l2_res_millimeter_per_pixel))



