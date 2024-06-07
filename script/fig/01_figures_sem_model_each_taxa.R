
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

# Model comparison
gt_model(birds_sem, "Birds") # GDD


# SEM plot
(birds_plot_gdd5 <- plot_sem(
  SEM_dag,
  summary(birds_sem$SEM_gdd5),
  title = "Birds")
)

birds_plot_fdd <-
  plot_sem(
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
    title = "Birds")

# model comparison
ggsave(
  "output/figs/SEM_plot/01_birds_GDD5.png",
  birds_plot_gdd5,
  width = 8,
  height = 5
)

ggsave(
  "output/figs/SEM_plot/01_birds_FDD.png",
  birds_plot_fdd,
  width = 8,
  height = 5
)


# butterflies ----

bf_sem <- readRDS("output/SEM_results/model_output/butterflies_sem.rds")

# Model comparison
gt_model(bf_sem, "Butterflies") # FDD

# SEM plot
SEM_dag_bf <- update_dag(SEM_dag, "SR -> Spop")


bf_plot_ggd5 <- plot_sem(
  SEM_dag_bf,
  summary(bf_sem$SEM_gdd5),
  title = "Butterflies"
  )


bf_plot_fdd <- plot_sem(
  SEM_dag_bf,
  summary(bf_sem$SEM_fdd),
  title = "Butterflies",
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
)


ggsave(
  "output/figs/SEM_plot/02_butterflies_GDD5.png",
  bf_plot_ggd5,
  width = 8,
  height = 5
)

ggsave(
  "output/figs/SEM_plot/02_butterflies_FDD.png",
  bf_plot_fdd,
  width = 8,
  height = 5
)


# phyto ----
phyto_sem <- readRDS("output/SEM_results/model_output/phyto_sem.rds")

# Model comparison
gt_model(phyto_sem, "Phyto") # GDD


# SEM plot
phyto_plot_ggd5 <- plot_sem(
  SEM_dag,
  summary(phyto_sem$SEM_gdd5),
  title = "Phytoplankton"
  )

phyto_plot_fdd <- plot_sem(
  SEM_dag,
  summary(phyto_sem$SEM_fdd),
  title = "Phytoplankton",
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
  )


ggsave(
  "output/figs/SEM_plot/03_phytoplankton_GDD5.png",
  phyto_plot_ggd5,
  width = 8,
  height = 5
)

ggsave(
  "output/figs/SEM_plot/03_phytoplankton_FDD.png",
  phyto_plot_fdd,
  width = 8,
  height = 5
)

# moths ----

moths_sem <- readRDS("output/SEM_results/model_output/moths_sem.rds")

# Model comparison
gt_model(moths_sem, "Moths") # GDD

moths_plot_fdd <- plot_sem(
  SEM_dag,
  summary(moths_sem$SEM_fdd),
  title = "Moths",
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
  )

moths_plot_gdd5 <- plot_sem(
  SEM_dag,
  summary(moths_sem$SEM_gdd5),
  title = "Moths"
  )


ggsave(
  "output/figs/SEM_plot/04_moths_GDD5.png",
  moths_plot_gdd5,
  width = 8,
  height = 5
)

ggsave(
  "output/figs/SEM_plot/04_moths_FDD.png",
  moths_plot_fdd,
  width = 8,
  height = 5
)


# rodents ----
rodents_sem <- readRDS("output/SEM_results/model_output/rodents_sem.rds")

# Model comparison
gt_model(rodents_sem, "Rodents") # GDD

# SEM plot
SEM_dag_rodents <- update_dag(SEM_dag,
                              "SR -> CWPoL")

rodents_plot_gdd5 <- plot_sem(
  SEM_dag_rodents,
  summary(rodents_sem$SEM_gdd5),
  title = "Small Mammals"
  )

rodents_plot_fdd <- plot_sem(
  SEM_dag_rodents,
  summary(rodents_sem$SEM_fdd),
  title = "Small Mammals",
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
)


ggsave(
  "output/figs/SEM_plot/05_rodents_GDD5.png",
  rodents_plot_gdd5,
  width = 8,
  height = 5
)

ggsave(
  "output/figs/SEM_plot/05_rodents_FDD.png",
  rodents_plot_fdd,
  width = 8,
  height = 5
)

# mammals ----
wgame_sem <- readRDS("output/SEM_results/model_output/wgame_sem.rds")

# Model comparison
gt_model(wgame_sem, "Mammals") # GDD

SEM_dag_mammals <- update_dag(
  SEM_dag,
  "
  Spop <-> Async
  SR -> Spop
  SR -> CWPoL
  ")

# SEM plot
wgame_plot_gdd5 <- plot_sem(
  SEM_dag_mammals,
  summary(wgame_sem$SEM_gdd5),
  title = "Large Mammals"
  )

wgame_plot_fdd <- plot_sem(
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
)


ggsave(
  "output/figs/SEM_plot/06_wintergame_GDD5.png",
  wgame_plot_gdd5,
  width = 8,
  height = 5
)

ggsave(
  "output/figs/SEM_plot/06_wintergame_FDD.png",
  wgame_plot_fdd,
  width = 8,
  height = 5
)

# Figure 3 manuscript ----

## best models ----


fig_3_results <-
  ((birds_plot_gdd5 + add_icon(icon_img$birds)) +
     (bf_plot_fdd+ add_icon(icon_img$butterflies)))/
  ((moths_plot_gdd5 + add_icon(icon_img$moths)) +
     (phyto_plot_ggd5 + add_icon(icon_img$phytoplankton))) /
  ((rodents_plot_gdd5 + add_icon(icon_img$small_mammals)) +
     (wgame_plot_gdd5 + add_icon(icon_img$large_mammals))) +
  plot_annotation(tag_levels = 'a', tag_suffix = ")")

ggsave(
  "output/figs/SEM_plot/00_fig_sem_all_taxa.png",
  fig_3_results,
  width = 15,
  height = 12.5
)

# GDD models ----
fig_3_results_gdd <-
  ((birds_plot_gdd5 + add_icon(icon_img$birds)) +
     (bf_plot_ggd5+ add_icon(icon_img$butterflies)))/
  ((moths_plot_gdd5 + add_icon(icon_img$moths)) +
     (phyto_plot_ggd5 + add_icon(icon_img$phytoplankton))) /
  ((rodents_plot_gdd5 + add_icon(icon_img$small_mammals)) +
     (wgame_plot_gdd5 + add_icon(icon_img$large_mammals))) +
  plot_annotation(tag_levels = 'a', tag_suffix = ")")

ggsave(
  "output/figs/SEM_plot/00_fig_sem_all_taxa_GDD.png",
  fig_3_results_gdd,
  width = 15,
  height = 12.5
)

## FDD models ----
fig_3_results_fdd <-
  ((birds_plot_gdd5 + add_icon(icon_img$birds)) +
     (bf_plot_fdd + add_icon(icon_img$butterflies)))/
  ((moths_plot_fdd + add_icon(icon_img$moths)) +
     (phyto_plot_fdd + add_icon(icon_img$phytoplankton))) /
  ((rodents_plot_fdd + add_icon(icon_img$small_mammals)) +
     (wgame_plot_fdd + add_icon(icon_img$large_mammals))) +
  plot_annotation(tag_levels = 'a', tag_suffix = ")")

ggsave(
  "output/figs/SEM_plot/00_fig_sem_all_taxa_FDD.png",
  fig_3_results_fdd,
  width = 15,
  height = 12.5
)

## all models together ----

((birds_plot_fdd + add_icon(icon_img$birds)) +
   (bf_plot_fdd + add_icon(icon_img$butterflies)))/
  ((moths_plot_fdd + add_icon(icon_img$moths)) +
     (phyto_plot_fdd + add_icon(icon_img$phytoplankton))) /
  ((rodents_plot_fdd + add_icon(icon_img$small_mammals)) +
     (wgame_plot_fdd + add_icon(icon_img$large_mammals)))

all_plots <-
(((birds_plot_gdd5 + add_icon(icon_img$birds)) +
    (bf_plot_ggd5+ add_icon(icon_img$butterflies)) +
    (moths_plot_gdd5 + add_icon(icon_img$moths)) +
    (phyto_plot_ggd5 + add_icon(icon_img$phytoplankton)) +
    (rodents_plot_gdd5 + add_icon(icon_img$small_mammals)) +
    (wgame_plot_gdd5 + add_icon(icon_img$large_mammals))
  ) +
 plot_layout(ncol = 1, nrow = 6))|
(((birds_plot_fdd + add_icon(icon_img$birds)) +
    (bf_plot_fdd + add_icon(icon_img$butterflies)) +
    (moths_plot_fdd + add_icon(icon_img$moths)) +
    (phyto_plot_fdd + add_icon(icon_img$phytoplankton)) +
    (rodents_plot_fdd + add_icon(icon_img$small_mammals)) +
    (wgame_plot_fdd + add_icon(icon_img$large_mammals))) +
  plot_layout(ncol = 1, nrow = 6))

ggsave(
  "output/figs/SEM_plot/00_fig_sem_all_plots_tgt.png",
  all_plots,
  width = 15,
  height = 25
)

