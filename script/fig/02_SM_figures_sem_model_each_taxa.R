
# load packages -----------------------------------------------------------

library(tidyverse)
library(piecewiseSEM)
library(gt)
library(patchwork)
library(grid)

source("function/gt_dsep.R")
source("function/gt_model.R")
source("function/update_dag.R")
source("function/plot_sem.R")
source("function/add_icon.R")



# load data  --------------------------------------------------------------
# icons

icon_files <- list.files("data/icon", full.names =  T)

icon_img <- map(icon_files, function(x){
  png::readPNG(x) %>%
    rasterGrob(interpolate = TRUE)
})

names(icon_img) <- word(icon_files, 3, sep = "/|\\.")


# load direct acyclic graph (DAG)
SEM_dag <- readRDS("output/sem_dag.rds")


# Birds ----

birds_sem <- readRDS("output/SEM_results/model_output/birds_sem.rds")

# SEM plot
(birds_plot_gdd5 <- plot_sem_2(
  SEM_dag,
  summary(birds_sem$SEM_gdd5),
  title = "Birds (GDD)") +
    add_icon(icon_img$birds, rm_title = F)
)

birds_plot_fdd <-
  plot_sem_2(
    SEM_dag,
    summary(birds_sem$SEM_fdd),
    t_label = "FDD",
    var_names = c(
      "Async" = "log_sqrt_async_l",
      "Spop" = "log_pop_stab",
      "SR" = "avg_richness",
      "FDis" = "f_dis",
      "MPD" = "mpd",
      "CWPoL" = "cwm_pc1",
      "Scom" = "log_stability",
      "mT" = "avg_FDD",
      "sdT" = "sd_FDD"
    ),
    title = "Birds (FDD)") +
  add_icon(icon_img$birds, rm_title = F)

SM_birds_plot <- birds_plot_gdd5 / birds_plot_fdd

# Save plot
set.seed(52)
ggsave(
  "output/figs/SEM_plot/SM/01_birds.pdf",
  SM_birds_plot,
  width = 10,
  height = 12.5
)

# butterflies ----

bf_sem <- readRDS("output/SEM_results/model_output/butterflies_sem.rds")


# SEM plot
SEM_dag_bf <- update_dag(SEM_dag, "SR -> Spop")


bf_plot_gdd5 <- plot_sem_2(
  SEM_dag_bf,
  summary(bf_sem$SEM_gdd5),
  title = "Butterflies (GDD)"
  ) +
  add_icon(icon_img$butterflies, rm_title = F)


bf_plot_fdd <- plot_sem_2(
  SEM_dag_bf,
  summary(bf_sem$SEM_fdd),
  title = "Butterflies (FDD)",
  t_label = "FDD",
  var_names = c(
    "Async" = "log_sqrt_async_l",
    "Spop" = "log_pop_stab",
    "SR" = "avg_richness",
    "FDis" = "f_dis",
    "MPD" = "mpd",
    "CWPoL" = "cwm_pc1",
    "Scom" = "log_stability",
    "mT" = "avg_FDD",
    "sdT" = "sd_FDD"
  )
) +
  add_icon(icon_img$butterflies, rm_title = F)



SM_bf_plot <- bf_plot_gdd5 / bf_plot_fdd

# Save Plot
set.seed(52)
ggsave(
  "output/figs/SEM_plot/SM/02_butterflies.pdf",
  SM_bf_plot,
  width = 10,
  height = 12.5
)


# phyto ----
phyto_sem <- readRDS("output/SEM_results/model_output/phyto_sem.rds")


# SEM plot
phyto_plot_ggd5 <- plot_sem_2(
  SEM_dag,
  summary(phyto_sem$SEM_gdd5),
  title = "Phytoplankton (GDD)"
  ) +
  add_icon(icon_img$phytoplankton, rm_title = F)


phyto_plot_fdd <- plot_sem_2(
  SEM_dag,
  summary(phyto_sem$SEM_fdd),
  title = "Phytoplankton (FDD)",
  t_label = "FDD",
  var_names = c(
    "Async" = "log_sqrt_async_l",
    "Spop" = "log_pop_stab",
    "SR" = "avg_richness",
    "FDis" = "f_dis",
    "MPD" = "mpd",
    "CWPoL" = "cwm_pc1",
    "Scom" = "log_stability",
    "mT" = "avg_FDD",
    "sdT" = "sd_FDD"
  )
  )  +
  add_icon(icon_img$phytoplankton, rm_title = F)

SM_phyto_plot <- phyto_plot_ggd5 / phyto_plot_fdd

# Save Plot
set.seed(52)
ggsave(
  "output/figs/SEM_plot/SM/03_phytoplankton.pdf",
  SM_phyto_plot,
  width = 10,
  height = 12.5
)

# moths ----

moths_sem <- readRDS("output/SEM_results/model_output/moths_sem.rds")

moths_plot_fdd <- plot_sem_2(
  SEM_dag,
  summary(moths_sem$SEM_fdd),
  title = "Moths (FDD)",
  t_label = "FDD",
  var_names = c(
    "Async" = "log_sqrt_async_l",
    "Spop" = "log_pop_stab",
    "SR" = "avg_richness",
    "FDis" = "f_dis",
    "MPD" = "mpd",
    "CWPoL" = "cwm_pc1",
    "Scom" = "log_stability",
    "mT" = "avg_FDD",
    "sdT" = "sd_FDD"
  )
  ) +
  add_icon(icon_img$moths, rm_title = F)

moths_plot_gdd5 <- plot_sem_2(
  SEM_dag,
  summary(moths_sem$SEM_gdd5),
  title = "Moths (GDD)"
  ) +
  add_icon(icon_img$moths, rm_title = F)


SM_moths_plot <- moths_plot_gdd5 / moths_plot_fdd

# Save Plot
set.seed(52)
ggsave(
  "output/figs/SEM_plot/SM/04_moths.pdf",
  SM_moths_plot,
  width = 10,
  height = 12.5
)


# rodents ----
rodents_sem <- readRDS("output/SEM_results/model_output/rodents_sem.rds")

# SEM plot
SEM_dag_rodents <- update_dag(
  SEM_dag,
  "SR -> CWPoL
Spop <-> Async")

rodents_plot_gdd5 <- plot_sem_2(
  SEM_dag_rodents,
  summary(rodents_sem$SEM_gdd5),
  title = "Small Mammals (GDD)"
  ) +
  add_icon(icon_img$small_mammals, rm_title = F)

rodents_plot_fdd <- plot_sem_2(
  SEM_dag_rodents,
  summary(rodents_sem$SEM_fdd),
  title = "Small Mammals (FDD)",
  t_label = "FDD",
  var_names = c(
    "Async" = "log_sqrt_async_l",
    "Spop" = "log_pop_stab",
    "SR" = "avg_richness",
    "FDis" = "f_dis",
    "MPD" = "mpd",
    "CWPoL" = "cwm_pc1",
    "Scom" = "log_stability",
    "mT" = "avg_FDD",
    "sdT" = "sd_FDD"
  )
) +
  add_icon(icon_img$small_mammals, rm_title = F)


SM_rodents_plot <- rodents_plot_gdd5 / rodents_plot_fdd

# Save Plot
set.seed(52)
ggsave(
  "output/figs/SEM_plot/SM/05_rodents.pdf",
  SM_rodents_plot,
  width = 10,
  height = 12.5
)

# mammals ----
wgame_sem <- readRDS("output/SEM_results/model_output/wgame_sem.rds")


SEM_dag_mammals <- update_dag(
  SEM_dag,
  "
  Spop <-> Async
  SR -> Spop
  SR -> CWPoL
  ")

# SEM plot
wgame_plot_gdd5 <- plot_sem_2(
  SEM_dag_mammals,
  summary(wgame_sem$SEM_gdd5),
  title = "Large Mammals"
  ) +
  add_icon(icon_img$large_mammals, rm_title = F)


wgame_plot_fdd <- plot_sem_2(
  SEM_dag_mammals,
  summary(wgame_sem$SEM_fdd),
  title = "Large Mammals ",
  t_label = "FDD",
  var_names = c(
    "Async" = "log_sqrt_async_l",
    "Spop" = "log_pop_stab",
    "SR" = "avg_richness",
    "FDis" = "f_dis",
    "MPD" = "mpd",
    "CWPoL" = "cwm_pc1",
    "Scom" = "log_stability",
    "mT" = "avg_FDD",
    "sdT" = "sd_FDD"
  )
) +
  add_icon(icon_img$large_mammals, rm_title = F)


SM_wgame_plot <- wgame_plot_gdd5 / wgame_plot_fdd

# Save Plot
set.seed(52)
ggsave(
  "output/figs/SEM_plot/SM/06_wintergame.pdf",
  SM_wgame_plot,
  width = 10,
  height = 12.5
)

