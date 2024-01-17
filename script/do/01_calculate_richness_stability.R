
# Load packages -----------------------------------------------------------

library(tidyverse)
library(codyn)

source("./function/comm_dyn_metrics.R")


# Birds data ---------------------------------------------------------------

birds_comm <- read.csv("data/cleaned/Birds/birds_comm.csv")

birds_metrics <- comm_dyn_metrics(birds_comm,
                                  abundance.var = "Abund_Stnd")

write.csv(birds_metrics, "data/cleaned/Birds/birds_rich_stab_df.csv")

# Butterflies data --------------------------------------------------------

butterflies_comm <- read.csv("data/cleaned/Butterflies/butterflies_comm.csv")

butterflies_metrics <- comm_dyn_metrics(butterflies_comm,
                                        abundance.var = "Abund_Stnd")

write.csv(butterflies_metrics,
          "data/cleaned/Butterflies/butterflies_rich_stab_df.csv")

# Phytoplankton data --------------------------------------------------------

phyto_comm <- read.csv("data/cleaned/Phytoplankton/phytoplankton_comm.csv")

phyto_metrics <- comm_dyn_metrics(phyto_comm,
                                  abundance.var = "Abund_Stnd")

write.csv(phyto_metrics,
          "data/cleaned/Phytoplankton/phytoplankton_rich_stab_df.csv")

# Moths data --------------------------------------------------------

moth_outliers <- readRDS("data/cleaned/Moths/moths_sp_trait_outliers.rds")
moths_comm <- read.csv("data/cleaned/Moths/moths_comm.csv") |>
  filter(!Species %in% moth_outliers)

moths_metrics <- comm_dyn_metrics(moths_comm,
                                  abundance.var = "Abund_Stnd")

write.csv(moths_metrics, "data/cleaned/Moths/moths_rich_stab_df.csv")


# Rodents data --------------------------------------------------------

rodents_comm <- read.csv("data/cleaned/Rodents/rodents_comm.csv")

rodents_metrics <- comm_dyn_metrics(rodents_comm,
                                    abundance.var = "Abund_Stnd")

write.csv(rodents_metrics, "data/cleaned/Rodents/rodents_rich_stab_df.csv")


# Winter game data --------------------------------------------------------

wintergame_comm <- read.csv("data/cleaned/WinterGame/wintergame_comm.csv")

wintergame_metrics <- comm_dyn_metrics(wintergame_comm,
                                       abundance.var = "Abund_Stnd")

write.csv(wintergame_metrics,
          "data/cleaned/WinterGame/wintergame_rich_stab_df.csv")

