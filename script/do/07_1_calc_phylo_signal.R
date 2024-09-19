
# load packages -----------------------------------------------------------

library(tidyverse)
library(future)
library(furrr)
source("function/calc_phylo_signal.R")

# Birds data ---------------------------------------------------------------

### load data ----
birds_traits <- read.csv("data/cleaned/Birds/birds_trait.csv")
birds_tree <- read.tree("data/cleaned/Birds/birds_phy_n100.tre")

### prepare trait data ----

# select traits to analysis
birds_traits_selected <- birds_traits %>%
  mutate(log_mass = log(Mass),
         log_gen_length = log(Gen_Length),
         log_max_brood = log(Max_Brood))  %>%
  # fast_slow strategy
  dplyr::select(Species, Mig, log_gen_length, log_mass, log_max_brood, Feeding)

trait_df <- birds_traits_selected[,-1]
rownames(trait_df) <- birds_traits_selected[,1] %>% str_replace_all(" ", "_")



### paralellel computation for multiple trees ----
# setting plan for future_walk function
workers <- (availableCores()*.8) %>% round()
plan(multisession, workers = workers)

# 100 trees in birds
future_walk(seq_along(birds_tree), function(i){
  file_name <- glue::glue(
    "output/phylo_signal/bird_trees/birds_p_signal_{sprintf('%03d', i)}.csv"
    )

  # calculate phylogenetic signal
  # parallelization is used to compute p-value based on bootstrap
  ps_parallel <- calc_phylo_signal(
    tree = birds_tree[[i]],
    trait_df = trait_df,
    n_rep = 1000,
    file = file_name,
    parallel = T,
    progress = FALSE
  )

}, .options = furrr_options(seed = TRUE)
)
plan(sequential)

# Butterflies data --------------------------------------------------------

### load data ----
butterflies_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/Butterfly_traits_final.csv") %>%
  mutate(Species = str_replace_all(Species, "_", " "))
butterflies_tree <- read.tree("data/cleaned/TraitsPhylogeny/Butterfly_tree_final.tre")


### prepare trait data ----

# select traits to analysis
butterflies_traits_selected <-
  butterflies_traits %>%
  mutate(log_wingspan = log(wingspan)) %>%
  # fast_slow strategy
  dplyr::select(Species, wintering,  voltinism,  host_usage, log_wingspan)

trait_df <- butterflies_traits_selected[,-1]
rownames(trait_df) <- butterflies_traits_selected[,1] %>% str_replace_all(" ", "_")

#### calculate phylogenetic signal ----
# parallelization is used to compute p-value based on bootstrap


ps_parallel <- calc_phylo_signal(
  butterflies_tree,
  trait_df,
  n_rep = 1000,
  file = "output/phylo_signal/butterfly_p_signal.csv",
  parallel = T,
  progress = FALSE
)


# Phytoplankton data ---------------------------------------------------------------

### load data ----
phyto_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/Phytoplankton_traits_final.csv")
phyto_tree <- read.tree("data/cleaned/Phytoplankton/Phytoplankton_tree_Weigel2022.tre")

### prepare trait data ----

## tree is not rooted AND fully dichotomous!

#
# # select traits to analysis
# phyto_traits_selected <-
#   phyto_traits %>%
#   # fast_slow strategy
#   dplyr::select(Species, nfix, motility, chain, cell_vol)
#
# trait_df <- phyto_traits_selected[,-1]
# rownames(trait_df) <- phyto_traits_selected[,1] %>% str_replace_all(" ", "_")
#
# #### calculate phylogenetic signal ----
# # parallelization is used to compute p-value based on bootstrap
#
#
# ps_parallel <- calc_phylo_signal(
#   phyto_tree,
#   trait_df[,1:2],
#   n_rep = 10,
#   file = "output/phylo_signal/butterfly_p_signal.csv",
#   parallel = T,
#   progress = FALSE
# )

# Moths data ---------------------------------------------------------------

## tree is not rooted AND fully dichotomous!

# moth_outliers <- readRDS("data/cleaned/Moths/moths_sp_trait_outliers.rds")
# moths_traits <-
#   read.csv("data/cleaned/TraitsPhylogeny/Moth_traits_final.csv") %>%
#   filter(!Species %in% moth_outliers)
# moths_tree <- read.tree("data/cleaned/TraitsPhylogeny/Moth_tree_final.tre")
#
#
# ### prepare trait data ----
#
# # select traits to analysis
# moths_traits_selected <-
#   moths_traits %>%
#   mutate(
#     log_wingspan = log(wingspan + 1),
#   ) %>%
#   # fast_slow strategy
#   dplyr::select(Abbrev_in_phylo, wintering,  voltinism,  host_usage, log_wingspan)
#
#
# trait_df <- moths_traits_selected[,-1]
# rownames(trait_df) <- moths_traits_selected[,1]
#
# #### calculate phylogenetic signal ----
# # parallelization is used to compute p-value based on bootstrap
#
#
# ps_parallel <- calc_phylo_signal(
#   moths_tree,
#   trait_df,
#   n_rep = 10,
#   file = "output/phylo_signal/butterfly_p_signal.csv",
#   parallel = T,
#   progress = FALSE
# )

# Rodents data --------------------------------------------------------

### load data ----

rodents_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/SmallRodents_traits_final.csv")
rodents_tree <- read.tree("data/cleaned/TraitsPhylogeny/SmallRodents_100trees_final.tre")

### prepare trait data ----

# select traits to analysis
rodents_traits_selected <-
  rodents_traits %>%
  mutate(
    log_body_mass = log(adult_mass_g),
    log_gen_length = log(generation_length_d),
    log_dispersal = log(dispersal_km)
  ) %>%
  # fast_slow strategy
  dplyr::select(phylacine_binomial , log_gen_length,
                litter_size_n, det_diet_breadth_n,
                log_body_mass, log_dispersal)

rodents_traits_selected <- rodents_traits_selected %>%
  rename("Species" = "phylacine_binomial")

trait_df <- rodents_traits_selected[,-1]
rownames(trait_df) <- rodents_traits_selected[,1] %>% str_replace_all(" ", "_")


### paralellel computation for multiple trees ----
# setting plan for future_walk function
workers <- (availableCores()*.8) %>% round()
plan(multisession, workers = workers)

# 100 trees in rodents

future_walk(seq_along(rodents_tree), \(i){
  file_name <- glue::glue(
    "output/phylo_signal/rodents_trees/rodents_p_signal_{sprintf('%03d', i)}.csv"
  )

  # calculate phylogenetic signal
  # parallelization is used to compute p-value based on bootstrap
  ps_parallel <- calc_phylo_signal(
    rodents_tree[[i]],
    trait_df,
    n_rep = 1000,
    file = file_name,
    parallel = T,
    progress = FALSE
  )

}, .options = furrr_options(seed = TRUE)
)

plan(sequential)

# Winter game data --------------------------------------------------------

### load data ----
wintergame_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/WinterGame_traits_final.csv")

wintergame_tree <- read.tree("data/cleaned/TraitsPhylogeny/WinterGame_100trees_final.tre")

### prepare trait data ----

# select traits to analysis
wintergame_traits_selected <-
  wintergame_traits %>%
  mutate(
    log_body_mass = log(adult_mass_g),
    log_dispersal = log(dispersal_km)
  ) %>%
  # fast_slow strategy
  dplyr::select(phylacine_binomial , generation_length_d,
                litter_size_n, det_diet_breadth_n,
                log_body_mass, log_dispersal)


wintergame_traits_selected <- wintergame_traits_selected %>%
  rename("Species" = "phylacine_binomial")


trait_df <- wintergame_traits_selected[,-1]
rownames(trait_df) <- wintergame_traits_selected[,1] %>% str_replace_all(" ", "_")


### paralellel computation for multiple trees ----
# setting plan for future_walk function
workers <- (availableCores()*.8) %>% round()
plan(multisession, workers = workers)

# 100 trees in rodents

future_walk(seq_along(wintergame_tree)[1:5], \(i){
  file_name <- glue::glue(
    "output/phylo_signal/wgame_trees/wgame_p_signal_{sprintf('%03d', i)}.csv"
  )

  # calculate phylogenetic signal
  # parallelization is used to compute p-value based on bootstrap
  ps_parallel <- calc_phylo_signal(
    wintergame_tree[[i]],
    trait_df,
    n_rep = 1000,
    file = file_name,
    parallel = T,
    progress = FALSE
  )

}, .options = furrr_options(seed = TRUE)
)

plan(sequential)
