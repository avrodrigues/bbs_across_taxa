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
# workers <- (availableCores()) %>% round()
# plan(multisession, workers = workers)


phylo = 1:100
#array job at CSC with a number of parallel runs
args = commandArgs(trailingOnly=TRUE)
jobin = as.numeric(args[1])

phylo_job = phylo[jobin]
tree = birds_tree[[phylo_job]]



# 100 trees in birds


file_name <- glue::glue(
    "output/phylo_signal/bird_trees/birds_p_signal_{sprintf('%03d', phylo_job)}.csv"
  )

  # calculate phylogenetic signal
  # parallelization is used to compute p-value based on bootstrap
ps_parallel <- calc_phylo_signal(
    tree = tree,
    trait_df = trait_df,
    n_rep = 1000,
    file = file_name,
    parallel = T,
    progress = FALSE
  )


