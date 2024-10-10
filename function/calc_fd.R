# Calculate Functional Dispersion and community weighted mean

calc_fd <- function(
    comm_data,
    trait_data_std,
    trait_cwm,
    parallel = FALSE,
    n_cores = NULL,
    time.var = "Year",
    species.var = "Species",
    abundance.var = "Abund_Stnd",
    replicate.var = "SiteID"
    ){

  require(tidyverse)
  require(FD)
  require(janitor)
  require(future)
  require(furrr)

  if(parallel == TRUE){
    if(is.null(n_cores)) n_cores <- (availableCores()*.75) |> round(0)

    plan(multisession, workers = n_cores)
  }
  if(parallel == FALSE){
    plan(sequential)
  }

  sites <- unique(comm_data[, replicate.var]) |> sort()

  fd_df <- future_map(sites, function(site){
    st <- comm_data %>% filter((!!as.symbol(replicate.var)) == site)

    # community matrix (sites vs species)
    comm_df <- st |>
      arrange(species.var) |>
      pivot_wider(
        id_cols = all_of(time.var),
        names_from = all_of(species.var),
        values_from = all_of(abundance.var),
        values_fill = 0
      )|>
      #keep only the species present in trait data (and in the same order)
      select(all_of(time.var), any_of(trait_data_std$Species))

    comm_mtx <- comm_df[,-1] |> as.matrix()
    rownames(comm_mtx) <- comm_df[[1]]


    # trait data frame
    spe_year <- st[, species.var] |> unique() |> sort()
    traits <- trait_data_std |>
      filter(Species %in% spe_year)

    rownames(traits) <- traits$Species


    ## Euclidean distance is been used, make sure to use dummy variables for factors

    FDis <- fdisp(dist(traits[,-1]), comm_mtx)$FDis |> mean()

    sp_in_comm <- rownames(trait_cwm) %in% colnames(comm_mtx)
    t_cwm <- trait_cwm[sp_in_comm,]
    cwm <- functcomp(as.matrix(t_cwm), comm_mtx) |> colMeans()

    matrix(
      c(FDis = FDis, cwm = cwm),
      ncol = 2,
      dimnames = list(site, c("FDis", "CWM_PC1"))
    ) |> as.data.frame()

  }) |>  list_rbind()

  #fd_df[is.na(fd_df)] <- 0

  if(parallel == TRUE) plan(sequential)


 clean_names(fd_df)

}

