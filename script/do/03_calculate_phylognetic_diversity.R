
# load packages -----------------------------------------------------------

library(tidyverse)
library(ape)
library(picante)
library(future)
library(furrr)
source("function/calc_mpd.R")

# Birds data ---------------------------------------------------------------

birds_comm <- read.csv("data/cleaned/Birds/birds_comm.csv") |>
  mutate(Species = str_replace_all(Species, " ", "_"))
birds_tree <- read.tree("data/cleaned/Birds/birds_phy_n100.tre")

## Calculate MPD -------

birds_mpd <- calc_mpd(birds_comm, birds_tree,
                      abundance.var = "Abund_Stnd")

# save MPD Average
write.csv(birds_mpd, "data/cleaned/Birds/birds_mpd_df.csv", row.names = F)

# Butterflies data ---------------------------------------------------------------

butterflies_comm <- read.csv("data/cleaned/Butterflies/butterflies_comm.csv") |>
  mutate(Species = str_replace_all(Species, " ", "_"))
butterflies_tree <- read.tree("data/cleaned/TraitsPhylogeny/Butterfly_tree_final.tre")

## Calculate MPD -------

butterflies_mpd <- calc_mpd(butterflies_comm, butterflies_tree,
                            abundance.var = "Abund_Stnd")

# save MPD Average
write.csv(butterflies_mpd, "data/cleaned/Butterflies/butterflies_mpd_df.csv",
          row.names = F)

# Phytoplankton data ---------------------------------------------------------------

phyto_comm <- read.csv("data/cleaned/Phytoplankton/phytoplankton_comm.csv") |>
  mutate(Species = str_replace_all(Species, " ", "_"))
phyto_tree <- read.tree("data/cleaned/Phytoplankton/Phytoplankton_tree_Weigel2022.tre")

## Calculate MPD -------

phyto_mpd <- calc_mpd(phyto_comm, phyto_tree,
                      abundance.var = "Abund_Stnd")

# save MPD Average
write.csv(phyto_mpd, "data/cleaned/Phytoplankton/phytoplankton_mpd_df.csv",
          row.names = F)

# Moths data ---------------------------------------------------------------

moth_outliers <- readRDS("data/cleaned/Moths/moths_sp_trait_outliers.rds")
moths_comm <- read.csv("data/cleaned/Moths/moths_comm.csv") |>
  filter(!Species %in% moth_outliers)
moths_tree <- read.tree("data/cleaned/TraitsPhylogeny/Moth_tree_final.tre")

## Calculate MPD -------
moths_mpd <- calc_mpd(
  moths_comm,
  moths_tree,
  species.var = "Abbrev_in_phylo",
  abundance.var = "Abund_Stnd")

# save MPD Average
write.csv(moths_mpd, "data/cleaned/Moths/moths_mpd_df.csv",
          row.names = F)

# Rodents data ---------------------------------------------------------------

rodents_comm <- read.csv("data/cleaned/Rodents/rodents_comm.csv") |>
  mutate(Species = str_replace_all(Species, " ", "_"))
rodents_tree <- read.tree("data/cleaned/TraitsPhylogeny/SmallRodents_100trees_final.tre")

## Calculate MPD -------
rodents_mpd <- calc_mpd(rodents_comm, rodents_tree,
                        abundance.var = "Abund_Stnd")

# save MPD Average
write.csv(rodents_mpd, "data/cleaned/Rodents/rodents_mpd_df.csv", row.names = F)

# Wintergame data ---------------------------------------------------------------

wintergame_comm <- read.csv("data/cleaned/WinterGame/wintergame_comm.csv")|>
  mutate(Species = str_replace_all(Species, " ", "_"))
wintergame_tree <- read.tree("data/cleaned/TraitsPhylogeny/WinterGame_100trees_final.tre")

## Calculate MPD -------

wintergame_mpd <- calc_mpd(wintergame_comm, wintergame_tree,
                      abundance.var = "Abund_Stnd")


# save MPD Average
write.csv(wintergame_mpd, "data/cleaned/WinterGame/wintergame_mpd_df.csv",
          row.names = F)
