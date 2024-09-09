# Calculate and visualise direct and indirect effects in SEMs

#install_version("piecewiseSEM", version="2.3.0")
install.packages("semEff")

library(remotes)
library(tidyverse)
library(piecewiseSEM)
library(semEff)

library(gt)
library(patchwork)
library(grid)

source("function/gt_dsep.R")
source("function/gt_model.R")
source("function/update_dag.R")
source("function/plot_sem.R")
source("function/add_icon.R")



# Birds ----

birds_sem <- readRDS("output/SEM_results/model_output/birds_sem.rds")
birds_gdd <- birds_sem$SEM_gdd

birds_gdd_summary <- summary(birds_gdd)
birds_gdd_coeff <- birds_gdd_summary$coefficients

model_data <- birds_gdd$data
birds_gdd_eff <- semEff(birds_gdd, R=1000)

summary(birds_gdd_eff)

birds_indir_eff <- getAllInd(birds_gdd_eff,)
birds_indir_eff_stab <- birds_indir_eff$log.stability


indir <- birds_fdd_eff$`All Indirect Effects`
