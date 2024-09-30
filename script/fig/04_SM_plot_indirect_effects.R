# Visualise indirect effects in SEMs

library(tidyverse)
library(ggplot2)
library(egg)

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

# Moths ----
moths_gdd <- readRDS("./output/SEM_results/indir_effects/moths_indir_gdd.rds")
moths_fdd <- readRDS("./output/SEM_results/indir_effects/moths_indir_fdd.rds")

moths_gdd <- group_variables_for_plotting(moths_gdd)
moths_fdd <- group_variables_for_plotting(moths_fdd)

moths_gdd_plot <- plot_indirect_effects(moths_gdd, var_groups_col, "Moths_gdd")
moths_fdd_plot <- plot_indirect_effects(moths_fdd, var_groups_col, "Moths_fdd")

# Phytoplankton ----
phyto_gdd <- readRDS("./output/SEM_results/indir_effects/phyto_indir_gdd.rds")
phyto_fdd <- readRDS("./output/SEM_results/indir_effects/phyto_indir_fdd.rds")

phyto_gdd <- group_variables_for_plotting(phyto_gdd)
phyto_fdd <- group_variables_for_plotting(phyto_fdd)

phyto_gdd_plot <- plot_indirect_effects(phyto_gdd, var_groups_col, "Phytoplankton_gdd")
phyto_fdd_plot <- plot_indirect_effects(phyto_fdd, var_groups_col, "Phytoplankton_fdd")

# Small mammals ----
rods_gdd <- readRDS("./output/SEM_results/indir_effects/rodents_indir_gdd.rds")
rods_fdd <- readRDS("./output/SEM_results/indir_effects/rodents_indir_fdd.rds")

rods_gdd <- group_variables_for_plotting(rods_gdd)
rods_fdd <- group_variables_for_plotting(rods_fdd)

rods_gdd_plot <- plot_indirect_effects(rods_gdd, var_groups_col, "Rodents_gdd")
rods_fdd_plot <- plot_indirect_effects(rods_fdd, var_groups_col, "Rodents_fdd")

# Large mammals ----
wgame_gdd <- readRDS("./output/SEM_results/indir_effects/wgame_indir_gdd.rds")
wgame_fdd <- readRDS("./output/SEM_results/indir_effects/wgame_indir_fdd.rds")

wgame_gdd <- group_variables_for_plotting(wgame_gdd)
wgame_fdd <- group_variables_for_plotting(wgame_fdd)

wgame_gdd_plot <- plot_indirect_effects(wgame_gdd, var_groups_col, "Wgame_gdd")
wgame_fdd_plot <- plot_indirect_effects(wgame_fdd, var_groups_col, "Wgame_fdd")

# Arrange and save plots ----
gdd_plots <- grid.arrange(birds_gdd_plot, buts_gdd_plot,
             moths_gdd_plot, phyto_gdd_plot,
             rods_gdd_plot,  wgame_gdd_plot,
             nrow=3, ncol=2)

ggsave("output/figs/indir_effects/indir_eff_gdd_SEMs.pdf",
       gdd_plots,
       width = 10,
       height = 12.5)

fdd_plots <- grid.arrange(birds_fdd_plot, buts_fdd_plot,
             moths_fdd_plot, phyto_fdd_plot,
             rods_fdd_plot, wgame_fdd_plot,
             nrow=3, ncol=2)

ggsave("output/figs/indir_effects/indir_eff_fdd_SEMs.pdf",
       fdd_plots,
       width = 10,
       height = 12.5)




