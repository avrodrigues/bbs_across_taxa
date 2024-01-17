
# Load packages -----------------------------------------------------------

library(tidyverse)
library(FD)
library(janitor)
library(future)
library(furrr)
library(fastDummies)
library(ggfortify)
source("function/calc_fd.R")
source("function/calc_pca_traits.R")

# Birds data ---------------------------------------------------------------

### load data ----
birds_comm <- read.csv("data/cleaned/Birds/birds_comm.csv")
birds_traits <- read.csv("data/cleaned/Birds/birds_trait.csv")

### prepare trait data ----

# select traits to analysis
birds_traits_selected <- birds_traits |>
  mutate(log_mass = log(Mass),
         log_gen_length = log(Gen_Length),
         log_max_brood = log(Max_Brood)) |>
  # fast_slow strategy
  dplyr::select(Species, Mig, log_gen_length, log_mass, log_max_brood, Feeding)

# Standardize traits
birds_traits_std <- birds_traits_selected |>
  mutate(across(where(is_double), ~as.numeric(scale(.x))))


birds_traits_std <- dummy_columns(
  birds_traits_std,
  select_columns = c("Mig", "Feeding"),
  remove_selected_columns = TRUE
) |>
  drop_na()

# calculate PCA of traits and extract the first axis
pca_birds <- calc_pca_traits(birds_traits_std)

# changing direction to have a gradient from fast to slow species
pc1 <- pca_birds$pca_traits$x[,1]*(-1)

t_pc1 <- matrix(pc1, ncol = 1, dimnames = list(names(pc1), "PC1"))

# Calculate functional diversity, pca cwm
birds_l_fd <- calc_fd(birds_comm, birds_traits_std, t_pc1)

# save data ----
write.csv(birds_l_fd, "data/cleaned/Birds/birds_fd_cwm.csv")
saveRDS(pca_birds$pca_traits, "data/cleaned/Birds/birds_traits_pca.rds")

ggsave(
  "output/figs/pca_traits/birds_traits_pca.png",
  pca_birds$pca_plot,
  width = 4,
  height = 4
  )


# Butterflies data --------------------------------------------------------

### load data ----
butterflies_comm <- read.csv("data/cleaned/Butterflies/butterflies_comm.csv")
butterflies_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/Butterfly_traits_final.csv") |>
  mutate(Species = str_replace_all(Species, "_", " "))

### prepare trait data ----

# select traits to analysis
butterflies_traits_selected <-
  butterflies_traits |>
  mutate(log_wingspan = log(wingspan)) |>
  # fast_slow strategy
  dplyr::select(Species, wintering,  voltinism,  host_usage, log_wingspan)

# Standardize traits
butterflies_traits_std <- butterflies_traits_selected |>
  mutate(across(where(is_double), ~as.numeric(scale(.x))))

butterflies_comm_tr <- butterflies_comm |> filter(
  Species %in% butterflies_traits_std$Species
)

butterflies_traits_std <- dummy_columns(
  butterflies_traits_std,
  select_columns = c("wintering", "voltinism", "host_usage"),
  remove_selected_columns = TRUE
) |>
  drop_na()

# calculate PCA of traits and extract the first axis
pca_butterflies <- calc_pca_traits(butterflies_traits_std)

# changing direction to have a gradient from fast to slow species
pc1 <- pca_butterflies$pca_traits$x[,1]*(-1)

t_pc1 <- matrix(pc1, ncol = 1, dimnames = list(names(pc1), "PC1"))

# Calculate functional diversity, pca cwm
butterflies_l_fd <- calc_fd(butterflies_comm, butterflies_traits_std, t_pc1, abundance.var = "Abund_Stnd")

# save data ----
write.csv(butterflies_l_fd, "data/cleaned/Butterflies/butterflies_fd_cwm.csv")
saveRDS(pca_butterflies$pca_traits, "data/cleaned/Butterflies/butterflies_traits_pca.rds")

ggsave(
  "output/figs/pca_traits/butterflies_traits_pca.png",
  pca_butterflies$pca_plot,
  width = 4,
  height = 4
)

# # Phytoplankton data --------------------------------------------------------

### load data ----
phyto_comm <- read.csv("data/cleaned/Phytoplankton/phytoplankton_comm.csv")
phyto_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/Phytoplankton_traits_final.csv")

### prepare trait data ----

# select traits to analysis
phyto_traits_selected <-
  phyto_traits |>
  # fast_slow strategy
  dplyr::select(Species, nfix, motility, chain, cell_vol)

# Standardize traits
phyto_traits_std <- phyto_traits_selected |>
  mutate(across(where(is_double), ~as.numeric(scale(.x)))) |>
  drop_na()

# calculate PCA of traits and extract the first axis
pca_phyto <- calc_pca_traits(phyto_traits_std)

# changing direction to have a gradient from fast to slow species
pc1 <- pca_phyto$pca_traits$x[,1]*(-1)

t_pc1 <- matrix(pc1, ncol = 1, dimnames = list(names(pc1), "PC1"))

# Calculate functional diversity, pca cwm
phyto_l_fd <- calc_fd(phyto_comm, phyto_traits_std, t_pc1)

# save data ----
write.csv(phyto_l_fd, "data/cleaned/Phytoplankton/phytoplankton_fd_cwm.csv")
saveRDS(pca_phyto$pca_traits, "data/cleaned/Phytoplankton/phytoplankton_traits_pca.rds")

ggsave(
  "output/figs/pca_traits/phytoplankton_traits_pca.png",
  pca_phyto$pca_plot,
  width = 4,
  height = 4
)


# Moths data --------------------------------------------------------

### load data ----
moth_outliers <- readRDS("data/cleaned/Moths/moths_sp_trait_outliers.rds")
moths_comm <- read.csv("data/cleaned/Moths/moths_comm.csv") |>
  filter(!Species %in% moth_outliers)

moths_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/Moth_traits_final.csv") |>
  filter(!Species %in% moth_outliers)

### prepare trait data ----

# select traits to analysis
moths_traits_selected <-
  moths_traits |>
  mutate(
    log_wingspan = log(wingspan + 1),
    ) |>
  # fast_slow strategy
  dplyr::select(Species, wintering,  voltinism,  host_usage, log_wingspan)

# Standardize traits
moths_traits_std <- moths_traits_selected |>
  mutate(across(where(is_double), ~as.numeric(scale(.x))))


moths_traits_std <- dummy_columns(
  moths_traits_std,
  select_columns = c("wintering", "voltinism", "host_usage"),
  remove_selected_columns = TRUE
) |>
  drop_na()

# calculate PCA of traits and extract the first axis
pca_moths <- calc_pca_traits(moths_traits_std)

# changing direction to have a gradient from fast to slow species
pc1 <- pca_moths$pca_traits$x[,1]*(-1)

t_pc1 <- matrix(pc1, ncol = 1, dimnames = list(names(pc1), "PC1"))

# Calculate functional diversity, pca cwm
moths_l_fd <- calc_fd(moths_comm, moths_traits_std, t_pc1, abundance.var = "Abund_Stnd")

# save data ----
write.csv(moths_l_fd, "data/cleaned/Moths/moths_fd_cwm.csv")
saveRDS(pca_moths$pca_traits, "data/cleaned/Moths/moths_traits_pca.rds")

ggsave(
  "output/figs/pca_traits/moths_traits_pca.png",
  pca_moths$pca_plot,
  width = 4,
  height = 4
)


# Rodents data --------------------------------------------------------

### load data ----
rodents_comm <- read.csv("data/cleaned/Rodents/rodents_comm.csv")
rodents_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/SmallRodents_traits_final.csv")

### prepare trait data ----

# select traits to analysis
rodents_traits_selected <-
  rodents_traits |>
  mutate(
    log_body_mass = log(adult_mass_g),
    log_gen_length = log(generation_length_d),
    log_dispersal = log(dispersal_km)
    ) |>
  # fast_slow strategy
  dplyr::select(phylacine_binomial , log_gen_length,
                litter_size_n, det_diet_breadth_n,
                log_body_mass, log_dispersal)

rodents_traits_selected <- rodents_traits_selected |>
  rename("Species" = "phylacine_binomial")

# Standardize traits
rodents_traits_std <- rodents_traits_selected |>
  mutate(across(where(is.numeric), ~as.numeric(scale(.x))))


# calculate PCA of traits and extract the first axis
pca_rodents <- calc_pca_traits(rodents_traits_std)

# gradient from fast to slow species
pc1 <- pca_rodents$pca_traits$x[,1]

t_pc1 <- matrix(pc1, ncol = 1, dimnames = list(names(pc1), "PC1"))

# Calculate functional diversity, pca cwm
rodents_l_fd <- calc_fd(rodents_comm, rodents_traits_std, t_pc1, abundance.var = "Abund_Stnd")

# save data ----
write.csv(rodents_l_fd, "data/cleaned/Rodents/SmallRodents_fd_cwm.csv")
saveRDS(pca_rodents$pca_traits, "data/cleaned/Rodents/SmallRodents_traits_pca.rds")

ggsave(
  "output/figs/pca_traits/rodents_traits_pca.png",
  pca_rodents$pca_plot,
  width = 4,
  height = 4
)


# Winter game data --------------------------------------------------------

### load data ----
wintergame_comm <- read.csv("data/cleaned/WinterGame/wintergame_comm.csv")
wintergame_traits <-
  read.csv("data/cleaned/TraitsPhylogeny/WinterGame_traits_final.csv")

### prepare trait data ----

# select traits to analysis
wintergame_traits_selected <-
  wintergame_traits |>
  mutate(
    log_body_mass = log(adult_mass_g),
    log_dispersal = log(dispersal_km)
  ) |>
  # fast_slow strategy
  dplyr::select(phylacine_binomial , generation_length_d,
                litter_size_n, det_diet_breadth_n,
                log_body_mass, log_dispersal)


wintergame_traits_selected <- wintergame_traits_selected |>
  rename("Species" = "phylacine_binomial")

# Standardize traits
wintergame_traits_std <- wintergame_traits_selected |>
  mutate(across(where(is.numeric), ~as.numeric(scale(.x))))


# calculate PCA of traits and extract the first axis
pca_wintergame <- calc_pca_traits(wintergame_traits_std)

# changing the gradient from fast to slow species
pc1 <- pca_wintergame$pca_traits$x[,1]*(-1)

t_pc1 <- matrix(pc1, ncol = 1, dimnames = list(names(pc1), "PC1"))


# Calculate functional diversity, cwm and cwm's pca
wintergame_l_fd <- calc_fd(wintergame_comm, wintergame_traits_std, t_pc1,
                        abundance.var = "Abund_Stnd", parallel = T)

# save data ----
write.csv(wintergame_l_fd, "data/cleaned/WinterGame/wintergame_fd_cwm.csv")
saveRDS(pca_wintergame$pca_traits, "data/cleaned/WinterGame/wintergame_traits_pca.rds")

ggsave(
  "output/figs/pca_traits/wintergame_traits_pca.png",
  pca_wintergame$pca_plot,
  width = 4,
  height = 4
)


