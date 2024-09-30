# Calculate indirect effects in SEMs

library(remotes)
library(tidyverse)
library(piecewiseSEM)
library(semEff)

source("function/indirect_effects.R")

# Birds ----

birds_sem <- readRDS("output/SEM_results/model_output/birds_sem.rds")
birds_gdd <- birds_sem$SEM_gdd
birds_fdd <- birds_sem$SEM_fdd

birds_gdd_coefs <- summary(birds_gdd)$coefficients
birds_fdd_coefs <- summary(birds_fdd)$coefficients

# Get paths for indirect effects
model_data <- birds_gdd$data
birds_gdd_eff <- semEff(birds_gdd, R=1000)
birds_fdd_eff <- semEff(birds_fdd, R=1000)

birds_indir_gdd <- getAllInd(birds_gdd_eff)
birds_indir_gdd <- as.data.frame(birds_indir_gdd$log.stability)

birds_indir_fdd <- getAllInd(birds_fdd_eff)
birds_indir_fdd <- as.data.frame(birds_indir_fdd$log.stability)

birds_indir_eff_gdd <- indir_effects(birds_indir_gdd, birds_gdd_coefs)
birds_indir_eff_fdd <- indir_effects(birds_indir_fdd, birds_fdd_coefs)

saveRDS(birds_indir_eff_gdd, "./output/SEM_results/indir_effects/birds_indir_gdd.rds")
saveRDS(birds_indir_eff_fdd, "./output/SEM_results/indir_effects/birds_indir_fdd.rds")

# Butterflies ----

but_sem <- readRDS("output/SEM_results/model_output/butterflies_sem.rds")
buts_gdd <- but_sem$SEM_gdd
buts_fdd <- but_sem$SEM_fdd

buts_gdd_coefs <- summary(buts_gdd)$coefficients
buts_fdd_coefs <- summary(buts_fdd)$coefficients

model_data <- buts_gdd$data
buts_gdd_eff <- semEff(buts_gdd, R=1000)
buts_fdd_eff <- semEff(buts_fdd, R=1000)

buts_indir_gdd <- getAllInd(buts_gdd_eff)
buts_indir_gdd <- as.data.frame(buts_indir_gdd$log.stability)
buts_indir_fdd <- as.data.frame(buts_indir_fdd$log.stability)

buts_indir_eff_gdd <- indir_effects(buts_indir_gdd, buts_gdd_coefs)
buts_indir_eff_fdd <- indir_effects(buts_indir_fdd, buts_fdd_coefs)

saveRDS(buts_indir_eff_gdd, "./output/SEM_results/indir_ef

buts_indir_fdd <- getAllInd(buts_fdd_eff)fects/butterflies_indir_gdd.rds")
saveRDS(buts_indir_eff_fdd, "./output/SEM_results/indir_effects/butterflies_indir_fdd.rds")

# Moths ----

moth_sem <- readRDS("output/SEM_results/model_output/moths_sem.rds")
moth_gdd <- moth_sem$SEM_gdd
moth_fdd <- moth_sem$SEM_fdd

moth_gdd_coefs <- summary(moth_gdd)$coefficients
moth_fdd_coefs <- summary(moth_fdd)$coefficients

model_data <- moth_gdd$data
moth_gdd_eff <- semEff(moth_gdd, R=1000)
moth_fdd_eff <- semEff(moth_fdd, R=1000)

moth_indir_gdd <- getAllInd(moth_gdd_eff)
moth_indir_gdd <- as.data.frame(moth_indir_gdd$log.stability)

moth_indir_fdd <- getAllInd(moth_fdd_eff)
moth_indir_fdd <- as.data.frame(moth_indir_fdd$log.stability)

moth_indir_eff_gdd <- indir_effects(moth_indir_gdd, moth_gdd_coefs)
moth_indir_eff_fdd <- indir_effects(moth_indir_fdd, moth_fdd_coefs)

saveRDS(moth_indir_eff_gdd, "./output/SEM_results/indir_effects/moths_indir_gdd.rds")
saveRDS(moth_indir_eff_fdd, "./output/SEM_results/indir_effects/moths_indir_fdd.rds")

# Phytoplankton ----

phyto_sem <- readRDS("output/SEM_results/model_output/phyto_sem.rds")
phyto_gdd <- phyto_sem$SEM_gdd
phyto_fdd <- phyto_sem$SEM_fdd

phyto_gdd_coefs <- summary(phyto_gdd)$coefficients
phyto_fdd_coefs <- summary(phyto_fdd)$coefficients

model_data <- phyto_gdd$data
phyto_gdd_eff <- semEff(phyto_gdd, R=1000)
phyto_fdd_eff <- semEff(phyto_fdd, R=1000)

phyto_indir_gdd <- getAllInd(phyto_gdd_eff)
phyto_indir_gdd <- as.data.frame(phyto_indir_gdd$log.stability)

phyto_indir_fdd <- getAllInd(phyto_fdd_eff)
phyto_indir_fdd <- as.data.frame(phyto_indir_fdd$log.stability)

phyto_indir_eff_gdd <- indir_effects(phyto_indir_gdd, phyto_gdd_coefs)
phyto_indir_eff_fdd <- indir_effects(phyto_indir_fdd, phyto_fdd_coefs)

saveRDS(phyto_indir_eff_gdd, "./output/SEM_results/indir_effects/phyto_indir_gdd.rds")
saveRDS(phyto_indir_eff_fdd, "./output/SEM_results/indir_effects/phyto_indir_fdd.rds")

# Small mammals ----

rod_sem <- readRDS("output/SEM_results/model_output/rodents_sem.rds")
rod_gdd <- rod_sem$SEM_gdd
rod_fdd <- rod_sem$SEM_fdd

rod_gdd_coefs <- summary(rod_gdd)$coefficients
rod_fdd_coefs <- summary(rod_fdd)$coefficients

model_data <- rod_gdd$data
rod_gdd_eff <- semEff(rod_gdd, R=1000)
rod_fdd_eff <- semEff(rod_fdd, R=1000)

rod_indir_gdd <- getAllInd(rod_gdd_eff)
rod_indir_gdd <- as.data.frame(rod_indir_gdd$log.stability)

rod_indir_fdd <- getAllInd(rod_fdd_eff)
rod_indir_fdd <- as.data.frame(rod_indir_fdd$log.stability)

rod_indir_eff_gdd <- indir_effects(rod_indir_gdd, rod_gdd_coefs)
rod_indir_eff_fdd <- indir_effects(rod_indir_fdd, rod_fdd_coefs)

saveRDS(rod_indir_eff_gdd, "./output/SEM_results/indir_effects/rodents_indir_gdd.rds")
saveRDS(rod_indir_eff_fdd, "./output/SEM_results/indir_effects/rodents_indir_fdd.rds")

# Large mammals ----

wgame_sem <- readRDS("output/SEM_results/model_output/wgame_sem.rds")
wgame_gdd <- wgame_sem$SEM_gdd
wgame_fdd <- wgame_sem$SEM_fdd

wgame_gdd_coefs <- summary(wgame_gdd)$coefficients
wgame_fdd_coefs <- summary(wgame_fdd)$coefficients

model_data <- wgame_gdd$data
wgame_gdd_eff <- semEff(wgame_gdd, R=10000)
wgame_fdd_eff <- semEff(wgame_fdd, R=10000)

wgame_indir_gdd <- getAllInd(wgame_gdd_eff)
wgame_indir_gdd <- as.data.frame(wgame_indir_gdd$log.stability)

wgame_indir_fdd <- getAllInd(wgame_fdd_eff)
wgame_indir_fdd <- as.data.frame(wgame_indir_fdd$log.stability)

wgame_indir_eff_gdd <- indir_effects(wgame_indir_gdd, wgame_gdd_coefs)
wgame_indir_eff_fdd <- indir_effects(wgame_indir_fdd, wgame_fdd_coefs)

saveRDS(wgame_indir_eff_gdd, "./output/SEM_results/indir_effects/wgame_indir_gdd.rds")
saveRDS(wgame_indir_eff_fdd, "./output/SEM_results/indir_effects/wgame_indir_fdd.rds")



