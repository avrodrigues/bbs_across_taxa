
# load packages -----------------------------------------------------------

library(tidyverse)
library(piecewiseSEM)
library(gt)
source("function/fit_bbs_sem.R")
source("function/plot_sem.R")
source("function/gt_dsep.R")
source("function/gt_model.R")
source("function/update_dag.R")

# load direct acyclic graph (DAG)
SEM_dag <- readRDS("output/sem_dag.rds")

# load data
model_df <- read.csv("data/cleaned/model_df_all_taxa.csv")


## Description of the modeling approach ----

# The model will be considered supported if both fisher's C and Chi square
# statistics are higher than 0.05.
# When the rule not holds, we will use the d-sep test as guide to include new
# arrows or correlation terms

# When comparing the model with FDD or GDD as environemental variable,
# we will choose the suppoted model with lower AICc value.

# BIRDS data ---------------------------------------------------------------

bird_sem_data <- filter(model_df, taxa == "Birds")

# fit model and results
birds_sem <- fit_bbs_sem(bird_sem_data)

# model comparison
gt_model(birds_sem, "Birds")

# d-separation tests
dSep(birds_sem$SEM_gdd5) |> gt_dsep("Birds")
dSep(birds_sem$SEM_fdd) |> gt_dsep("Birds")


# summary results
summary(birds_sem$SEM_gdd5)$R2

saveRDS(birds_sem, "output/SEM_results/model_output/birds_sem.rds")


# BUTTERFLY data ---------------------------------------------------------------

# load data
bf_sem_data <- filter(model_df, taxa == "Butterflies")

# fit model and results
bf_sem <- fit_bbs_sem(bf_sem_data)

# model comparison
gt_model(bf_sem, "Butterflies")

# d-separation tests
dSep(bf_sem$SEM_gdd5) |> gt_dsep("Butterflies")
dSep(bf_sem$SEM_fdd) |> gt_dsep("Butterflies")

#update model
model_data <- bf_sem_data
bf_sem$SEM_gdd5 <- update(
  bf_sem$SEM_gdd5,
  log_pop_stab ~  avg_richness + mpd + f_dis + cwm_pc1 + avg_GDD5 + sd_GDD5
)

bf_sem$SEM_fdd <- update(
  bf_sem$SEM_fdd,
  log_pop_stab ~  avg_richness + mpd + f_dis + cwm_pc1 + avg_FDD + sd_FDD
)

# model comparison
gt_model(bf_sem, "Butterflies") # FDD

# summary results
summary(bf_sem$SEM_fdd)$R2

saveRDS(bf_sem, "output/SEM_results/model_output/butterflies_sem.rds")


# PHYTOPLANTON data --------------------------------------------------------

# load data
phyto_sem_data <- filter(model_df, taxa == "Phytoplankton")

# fit model and results
phyto_sem <- fit_bbs_sem(phyto_sem_data)

# model comparison
gt_model(phyto_sem, "Phytoplankton") #GDD

# d-separation tests
dSep(phyto_sem$SEM_gdd5) |> gt_dsep("Phytoplankton")
dSep(phyto_sem$SEM_fdd) |> gt_dsep("Phytoplankton")

# summary results
summary(phyto_sem$SEM_gdd5)$R2

saveRDS(phyto_sem, "output/SEM_results/model_output/phyto_sem.rds")
# MOTHS data ---------------------------------------------------------------

# load data
moths_sem_data <- filter(model_df, taxa == "Moths")

# fit model and results
moths_sem <- fit_bbs_sem(moths_sem_data)

# model comparison
gt_model(moths_sem, "Moths") #GDD

# d-separation tests and model update
dSep(moths_sem$SEM_gdd5) |> gt_dsep("Moths")
dSep(moths_sem$SEM_fdd) |> gt_dsep("Moths")

# summary results

summary(moths_sem$SEM_gdd5)$R2


saveRDS(moths_sem, "output/SEM_results/model_output/moths_sem.rds")
# RODENTS data -------------------------------------------------------------

# load data
rodents_sem_data <- filter(model_df, taxa == "Rodents")

# fit model and results
rodents_sem <- fit_bbs_sem(rodents_sem_data)

# model comparison
gt_model(rodents_sem, "Rodents")

# d-separation tests and model update
dSep(rodents_sem$SEM_gdd5) |> gt_dsep("Rodents")
dSep(rodents_sem$SEM_fdd) |> gt_dsep("Rodents")

model_data <- rodents_sem_data
rodents_sem$SEM_gdd5 <- update(
  rodents_sem$SEM_gdd5,
  #log_stability ~ log_pop_stab + log_sqrt_async_l + cwm_pc1,
  cwm_pc1 ~ avg_richness + avg_GDD5 + sd_GDD5,
  log_sqrt_async_l  %~~% log_pop_stab
  )


rodents_sem$SEM_fdd <- update(
  rodents_sem$SEM_fdd,
  #log_stability ~ log_pop_stab + log_sqrt_async_l + cwm_pc1,
  cwm_pc1 ~ avg_richness + avg_FDD + sd_FDD,
  log_sqrt_async_l  %~~% log_pop_stab
)

# model comparison
gt_model(rodents_sem, "Rodents") #GDD


# summary results
summary(rodents_sem$SEM_gdd5)$R2

saveRDS(rodents_sem, "output/SEM_results/model_output/rodents_sem.rds")


# WINTER GAME data ---------------------------------------------------------

# load data
wgame_sem_data <- filter(model_df, taxa == "WinterGame")

# fit model and results
wgame_sem <- fit_bbs_sem(wgame_sem_data)

# model comparison
gt_model(wgame_sem, "Mammals")

# d-separation tests
dSep(wgame_sem$SEM_gdd5) |> gt_dsep("Mammals")
dSep(wgame_sem$SEM_fdd) |> gt_dsep("Mammals")

model_data <- wgame_sem_data
wgame_sem$SEM_gdd5 <- update(
  wgame_sem$SEM_gdd5,
  log_pop_stab ~  avg_richness + mpd + f_dis + cwm_pc1 + avg_GDD5 + sd_GDD5,
  cwm_pc1 ~ avg_richness + avg_GDD5 + sd_GDD5,
  log_sqrt_async_l  %~~% log_pop_stab
)

wgame_sem$SEM_fdd <- update(
  wgame_sem$SEM_fdd,
  log_pop_stab ~  avg_richness + mpd + f_dis + cwm_pc1 + avg_FDD + sd_FDD,
  cwm_pc1 ~ avg_richness + avg_FDD + sd_FDD,
  log_sqrt_async_l  %~~% log_pop_stab
)

# model comparison
gt_model(wgame_sem, "Mammals") #GDD

# summary results
summary(wgame_sem$SEM_gdd5)$R2
summary(wgame_sem$SEM_fdd)



saveRDS(wgame_sem, "output/SEM_results/model_output/wgame_sem.rds")

