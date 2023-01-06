## This is a simple script to rerun all analyses in the project
##
## 2022-12-21
## Peter Regier
##
# ########### #
# ########### #

require(pacman)
p_load(tictoc)

tic("Run all scripts")
## Prep datasets
source("R scripts/0_pull_usgs_d50.R")
source("R scripts/0.5_create_master_data.R")

## Make figures
source("R scripts/1_map_v2.R")
source("R scripts/2_figure2_correlations.R")
source("R scripts/3_figure3_compare_sources.R")
source("R scripts/4_figure4_comparison_maps.R")
source("R scripts/5_figure5_correlation_matrix.R")
source("R scripts/6_figure6_intra_site_variability.R")
source("R scripts/7_figure7_accuracy_v_visual_assessment.R")

toc()