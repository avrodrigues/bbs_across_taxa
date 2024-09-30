# Investigate indirect effect sizes

library(tidyverse)

source("./function/group_variables_plotting.R")

# Read in best fit models ----
birds_gdd <- readRDS("./output/SEM_results/indir_effects/birds_indir_gdd.rds")
buts_fdd <- readRDS("./output/SEM_results/indir_effects/butterflies_indir_fdd.rds")
moths_gdd <- readRDS("./output/SEM_results/indir_effects/moths_indir_gdd.rds")
phyto_gdd <- readRDS("./output/SEM_results/indir_effects/phyto_indir_gdd.rds")
rods_gdd <- readRDS("./output/SEM_results/indir_effects/rodents_indir_gdd.rds")
wgame_gdd <- readRDS("./output/SEM_results/indir_effects/wgame_indir_gdd.rds")

birds_gdd <- group_variables_for_plotting(birds_gdd)
buts_fdd <- group_variables_for_plotting(buts_fdd)
moths_gdd <- group_variables_for_plotting(moths_gdd)
phyto_gdd <- group_variables_for_plotting(phyto_gdd)
rods_gdd <- group_variables_for_plotting(rods_gdd)
wgame_gdd <- group_variables_for_plotting(wgame_gdd)

df_list <- list(birds_gdd, buts_fdd, moths_gdd, phyto_gdd, rods_gdd, wgame_gdd)
names(df_list) <- c("birds", "butterflies", "moths", "phyto", "rods", "wgame")

# Collect effects to variable-specific dataframes
taxa <- names(df_list)

variable_effects <- function(var){
  df_var <- as.data.frame(matrix(ncol=ncol(birds_gdd)+1, nrow = 0))
  colnames(df_var) <- c(names(birds_gdd),"taxon")
  df_var[,c(1:6,11:13)] <- sapply(df_var[,c(1:6,11:13)], as.character)
  df_var[,7:10] <- sapply(df_var[,7:10], as.numeric)

  for(i in taxa){
    df <- df_list[[i]]
    df$taxon <- i
    dd <- df[df$first_var==var,]
    df_var <- bind_rows(df_var, dd)
  }
  return(df_var)
}

mpd_Efs <- variable_effects("mpd")
fdis_Efs <- variable_effects("f_dis")
cwm_Efs <- variable_effects("cwm_pc1")
mGDD5_Efs <- variable_effects("avg_GDD5")
sdGDD5_Efs <- variable_effects("sd_GDD5")
mFDD5_Efs <- variable_effects("avg_FDD")
sdFDD5_Efs <- variable_effects("sd_FDD")
SR_Efs <- variable_effects("avg_richness")





