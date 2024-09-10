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

setwd("C:/LocalData/tuulriss/OneDrive - University of Helsinki/REC/BBS/bbs_across_taxa")

# Birds ----

birds_sem <- readRDS("output/SEM_results/model_output/birds_sem.rds")
birds_gdd <- birds_sem$SEM_gdd

birds_gdd_summary <- summary(birds_gdd)
birds_gdd_coeff <- birds_gdd_summary$coefficients

model_data <- birds_gdd$data
birds_gdd_eff <- semEff(birds_gdd, R=1000)

summary(birds_gdd_eff)

birds_indir_eff <- getAllInd(birds_gdd_eff,)
birds_indir_eff_stab <- as.data.frame(birds_indir_eff$log.stability)
birds_indir_eff_stab %>% rownames_to_column(var="path") -> birds_indir_stab

birds_indir <- as.data.frame(rownames(birds_indir_eff_stab))
names(birds_indir) <- "path"
birds_indir$path_nro <- paste0("path",seq(1:nrow(birds_indir)))

birds_indir$path <- gsub(".", ":", birds_indir$path, fixed=TRUE)

birds_indir$var1 <- str_split_i(birds_indir$path, ":", 3)
birds_indir$var2 <- str_split_i(birds_indir$path, ":", 2)
birds_indir$var3 <- str_split_i(birds_indir$path, ":", 1)
birds_indir$resp_var <- "log_stability"

birds_indir$var1xvar2 <- NA
birds_indir$var2xvar3 <- NA
birds_indir$var3xresp_var <- NA
birds_indir$indir_effect <- NA

birds_gdd_coeff %>% select(Response, Predictor, Std.Estimate) -> birds_coeffs
paths <- birds_indir$path_nro
i <- "path1"

for(i in paths){
  print(i)
  path <- birds_indir[birds_indir$path_nro==i,]
  var1 <- path$var1
  var2 <- path$var2
  var3 <- path$var3
  resp <- path$resp_var

  if(!is.na(var1)) {
    birds_coeffs %>% filter(Response==var2 & Predictor==var1) -> coef1
    coef1 <- coef1$Std.Estimate
  } else {
    coef1 <- NA
  }

  birds_coeffs %>% filter(Response==var3 & Predictor==var2) -> coef2
  coef2 <- coef2$Std.Estimate
  birds_coeffs %>% filter(Response==resp & Predictor==var3) -> coef3
  coef3 <- coef3$Std.Estimate

  birds_indir[birds_indir$path_nro==i,"var1xvar2"] <- coef1
  birds_indir[birds_indir$path_nro==i,"var2xvar3"] <- coef2
  birds_indir[birds_indir$path_nro==i,"var3xresp_var"] <- coef3

  if(!is.na(coef1)) {
    birds_indir[birds_indir$path_nro==i,"indir_effect"] <- coef1 * coef2 * coef3
  } else {
    birds_indir[birds_indir$path_nro==i,"indir_effect"] <- coef2 * coef3
  }

}

