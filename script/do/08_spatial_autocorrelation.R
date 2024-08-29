
# load packages -----------------------------------------------------------

library(piecewiseSEM)
library(tidyverse)
library(spdep)
library(sf)


# get files ---------------------------------------------------------------

# SEM results
SEM_files <- list.files("output/SEM_results/model_output/", full.names = T)

# Site Coordinantes
coords_files <- list.files("data/cleaned/coords/", full.names = T)

# dataset names
dataset_names <- SEM_files %>%
    word(4, sep = "/") %>%
    word(1, sep = "_")


# loop for Moran's I computation ------------------------------------------


## results dataframe ----

morans_df <- data.frame(
  dataset = character(),
  sem_model = character(),
  variable = character(),
  morans_I = numeric(),
  p_value = numeric()
)

# 1. dataset level ----
for (i in seq_along(dataset_names)){

  # site coordiantes
  xy_df <- read.csv(coords_files[i])
  xy_df$SiteID <- as.character(xy_df$SiteID)

  # data frame to sf
  data_sf <- st_as_sf(
    xy_df,
    coords = c("Longitude", "Latitude"),
    crs = "EPSG:3046"
  )

  # neighborhood distance: 50km
  ng_dist <- 50e3

  #make a neighborhood list:
  neigh <- dnearneigh(x = data_sf, d1 = 0, d2 = ng_dist)
  # distance weights
  wts <- nb2listw(neighbours = neigh, style = 'W', zero.policy = T)

  # load SEM model
  SEM_model <- readRDS(SEM_files[i])

  # 2. SEM model level ----
  for (k in seq_along(SEM_model)){

    # Dataset and model name
    dataset_name <- dataset_names[i]
    model_name <- names(SEM_model)[k]


    # get model residuals
    model_residuals <- residuals(SEM_model[[k]]) %>%
      as.data.frame() %>%
      mutate(SiteID = SEM_model[[k]]$data$SiteID)

    # join coords and residuals
    data_model_sf <- left_join(data_sf, model_residuals, by = "SiteID")

    variables <- grep("residuals", names(bird_ac_sf), value = T)

    # 3. Variable level ----
    for (var in variables){
      # variable to calculate the Moran's I
      var_residual <- data_model_sf[[var]]

      # compute Moran's I
      mor.mc <- moran.mc(x = var_residual, listw = wts,
                         nsim = 999, zero.policy = T)

      new_row <- data.frame(
        variable = var,
        morans_I = mor.mc$statistic,
        p_value = mor.mc$p.value
      )

      new_row <- data.frame(
        dataset = dataset_name,
        sem_model = model_name,
        variable = var,
        morans_I = mor.mc$statistic,
        p_value = mor.mc$p.value
      )

      morans_df <- morans_df %>% add_row(new_row)
    }

  }
}


# Save Moran's I results --------------------------------------------------


morans_df %>%
  # add new columm
  mutate(spat_autocor = ifelse(p_value <= 0.05, T, F)) %>%
  #save in csv
  write.csv("output/SEM_results/spatial_autocorrelation/morans_i_residuals.csv")
