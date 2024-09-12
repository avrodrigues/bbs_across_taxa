# Visualise indirect effects in SEMs

library(tidyverse)
library(ggplot2)
library(egg)

setwd("C:/LocalData/tuulriss/OneDrive - University of Helsinki/REC/BBS/bbs_across_taxa")

source("./function/group_variables_plotting.R")
source("./function/plot_indirect_effects.R")

var_groups <- c("Environment","Fun_traits","Richness")
var_groups_col = c("#a9344f", "#83a6c4" ,"#3d291a")
names(var_groups_col) <- var_groups

# Birds ----
birds_gdd <- readRDS("./output/SEM_results/indir_effects/birds_indir_gdd.rds")
birds_fdd <- readRDS("./output/SEM_results/indir_effects/birds_indir_fdd.rds")

birds_gdd <- group_variables_for_plotting(birds_gdd)
birds_fdd <- group_variables_for_plotting(birds_fdd)

birds_gdd_plot <- plot_indirect_effects(birds_gdd, var_groups_col, "Birds_gdd")
birds_fdd_plot <- plot_indirect_effects(birds_fdd, var_groups_col, "Birds_fdd")

# Butterflies ----
buts_gdd <- readRDS("./output/SEM_results/indir_effects/butterflies_indir_gdd.rds")
buts_fdd <- readRDS("./output/SEM_results/indir_effects/butterflies_indir_fdd.rds")

buts_gdd <- group_variables_for_plotting(buts_gdd)
buts_fdd <- group_variables_for_plotting(buts_fdd)

buts_gdd_plot <- plot_indirect_effects(buts_gdd, var_groups_col, "Butterflies_gdd")
buts_fdd_plot <- plot_indirect_effects(buts_fdd, var_groups_col, "Butterflies_fdd")


ggsave(
  "output/figs/SEM_plot/SM/01_birds.pdf",
  SM_birds_plot,
  width = 10,
  height = 12.5
)



