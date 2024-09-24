
# Check the renv set ------------------------------------------------------

need_pkg <- c("dplyr",
              "furrr",
              "future",
              "glue",
              "tidyverse",
              "dplyr",
              "furrr",
              "phytools",
              "picante",
              "progressr",
              "ape",
              "expm")



inst_pkg <- installed.packages()[,"Package"]

to_inst <- !need_pkg %in% inst_pkg

if(any(to_inst)){
  renv::restore(packages = need_pkg)
}

inst_pkg <- installed.packages()[,"Package"]

to_inst <- !need_pkg %in% inst_pkg

if(any(to_inst)){
  install.packages(need_pkg[to_inst])
}


# load packages -----------------------------------------------------------

library(tidyverse)
library(future)
library(furrr)
source("function/calc_phylo_signal.R")
library(progressr)
library(picante)
library(phytools)
source("function/delta_stat_code.R")

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
#
# t0 <-Sys.time()
#   # calculate phylogenetic signal
#   # parallelization is used to compute p-value based on bootstrap
# ps_parallel <- calc_phylo_signal(
#     tree = tree,
#     trait_df = trait_df,
#     n_rep = 10,
#     file = file_name,
#     parallel = T,
#     progress = FALSE
#   )
#
# t1 <- Sys.time()
#
# cat("Total time taken: ", t1 - t0, "\n")





###

classify_traits <- function(df) {
  sapply(df, function(column) {
    unique_vals <- length(unique(column))
    total_vals <- length(column)

    # Exclude columns with all unique values (likely identifiers)
    if (unique_vals == 2) {
      return("Binary")
    } else if (is.numeric(column) || is.integer(column)) {
      return("Continuous")
    } else if (is.factor(column) || is.character(column)) {
      return("Categorical")
    } else {
      return(NA)
    }
  }) %>% na.omit()
}

compute_phylo_signal <- function(name) {
  type <- trait_types[name]

  # Calculate Pagel's lambda for continuous traits
  if (type == "Continuous") {

    trait <- trait_data[[name]]
    names(trait) <- phy$tip.label

    # Calculate Pagel's lambda and p-value
    lambda <- phylosig(phy, trait, method = "lambda", test = TRUE)

    new_row <- data.frame(
      trait_name = name,
      trait_type = type,
      stat_name  = "Pagel's lambda",
      stat_value = lambda$lambda,
      p_value    = lambda$P,
      signal     = lambda$P <= 0.05
    )

    # Calculate Delta statistic for categorical traits
  } else if (type == "Binary" | type == "Categorical") {

    trait <- trait_data[[name]]
    names(trait) <- phy$tip.label

    # Delta stat
    deltaA <- delta(trait, phy, 0.1, 0.0589, 10000, 10, 100)

    # P-value by randomization
    random_delta <- future_map_dbl(1:10, ~ {
      rtrait <- sample(trait)
      delta(rtrait, phy, 0.1, 0.0589, 10000, 10, 100)
    }, .options = furrr_options(seed = TRUE))

    # p-value for Delta statistic
    p_value <- sum(random_delta > deltaA) / length(random_delta)

    new_row <- data.frame(
      trait_name = name,
      trait_type = type,
      stat_name  = "Delta",
      stat_value = deltaA,
      p_value    = p_value,
      signal     = p_value <= 0.05
    )

  }

  return(new_row)
}


trait_data <- trait_df
# Classify traits
trait_types <- classify_traits(trait_data)
trait_names <- names(trait_types)

# Initialize result dataframe
phy_sig_df <- data.frame(
  trait_name = character(),
  trait_type = character(),
  stat_name  = character(),
  stat_value = numeric(),
  p_value    = numeric(),
  signal     = logical()
)


plan(multicore, workers = availableCores())
match_data <- match.phylo.data(tree, trait_df)

phy <- match_data$phy

# this step is only to avoid a bug in ape::ace()
if(!is.null(phy$node.label))  phy$node.label <- NULL

(t0 <- Sys.time())
phy_sig_df <- future_map_dfr(trait_names, compute_phylo_signal)

write.csv(phy_sig_df, file, row.names = FALSE)
t1 <- Sys.time()

t1 - t0
