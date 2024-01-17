
# load pacakages ----------------------------------------------------------


library(tidyverse)


# load data ---------------------------------------------------------------


l_stab_files <- list.files(pattern = "_rich_stab_df.csv", recursive = T)
l_mpd_files <- list.files(pattern = "mpd_df.csv", recursive = T)
l_fd_files <- list.files(pattern = "fd_cwm", recursive = T)
l_env_files <- list.files(pattern = "env.csv", recursive = T)


stab_df <- map(l_stab_files, function(file){

  read.csv(file, row.names = 1) |>
    select(SiteID, stability, pop_stab, async_l, avg_richness) |>
    mutate(taxa = word(file, 3, sep = "/"), .before = 1) |>
    mutate(SiteID = as.character(SiteID))

}) |> list_rbind()

mpd_df <- map(l_mpd_files, function(file){

  read.csv(file) |>
    mutate(taxa = word(file, 3, sep = "/"), .before = 1) |>
    mutate(SiteID = as.character(SiteID))

}) |> list_rbind()

fd_df <- map(l_fd_files, function(file){

  read.csv(file) |>
    rename("SiteID" = "X") |>
    mutate(taxa = word(file, 3, sep = "/"),
           .before = 1) |>
    mutate(SiteID = as.character(SiteID))

}) |> list_rbind()

env_df <- map(l_env_files, function(file){

  read.csv(file) |>
    mutate(taxa = word(file, 3, sep = "/"), .before = 1) |>
    mutate(SiteID = as.character(SiteID))

}) |> list_rbind()

# join data ----

model_df <- left_join(stab_df, mpd_df, by = c("taxa", "SiteID")) |>
  left_join(fd_df, by = c("taxa", "SiteID")) |>
  left_join(env_df, by = c("taxa", "SiteID"))|>
  mutate(
    log_stability = log(stability),
    log_sqrt_async_l = 0.5*log((async_l)),
    log_pop_stab = log(pop_stab)
  )

# save data ----
write.csv(model_df, "data/cleaned/model_df_all_taxa.csv", row.names = F)
