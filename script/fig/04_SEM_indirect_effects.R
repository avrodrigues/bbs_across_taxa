# Calculate indirect effects in SEMs

#install_version("piecewiseSEM", version="2.3.0")
install.packages("semEff")

library(remotes)
library(tidyverse)
library(piecewiseSEM)
library(semEff)

library(gt)
library(patchwork)
library(grid)

source("function/indirect_effects.R")
source("function/gt_dsep.R")
source("function/gt_model.R")
source("function/update_dag.R")
source("function/plot_sem.R")
source("function/add_icon.R")

setwd("C:/LocalData/tuulriss/OneDrive - University of Helsinki/REC/BBS/bbs_across_taxa")

# Birds ----

birds_sem <- readRDS("output/SEM_results/model_output/birds_sem.rds")
birds_gdd <- birds_sem$SEM_gdd

birds_gdd_coefs <- summary(birds_gdd)$coefficients

# Use semEff to get paths for indirect effects
model_data <- birds_gdd$data
birds_gdd_eff <- semEff(birds_gdd, R=1000)

birds_indir_eff <- getAllInd(birds_gdd_eff)
birds_indir_stab <- as.data.frame(birds_indir_eff$log.stability)
birds_indir_stab %>% rownames_to_column(var="path") -> birds_indir_stab


birds_indir_eff <- indir_effects(birds_indir_stab, birds_gdd_coefs)


