# Calculate mean pairwise phylogenetic distance


calc_mpd <- function(
    comm_data,
    phy_data,
    time.var = "Year",
    species.var = "Species",
    abundance.var = "Abundance",
    replicate.var = "SiteID"

    ){

  sites <- unique(comm_data[, replicate.var]) |> sort()

  # start loop to calculate MPD for each site
  mdp_df <-  future_map(sites, function(site){

    st <- comm_data %>% filter((!!as.symbol(replicate.var)) == site)

    # community matrix (sites vs species)
    comm_df <- st |>
      # mutate(Species = str_replace_all(Species, " ", "_")) |>
      arrange(Species) |>
      pivot_wider(
        id_cols = all_of(time.var),
        names_from = all_of(species.var),
        values_from = all_of(abundance.var),
        values_fill = 0
      )

    comm_mtx <- comm_df[,-1] |> as.matrix()
    rownames(comm_mtx) <- comm_df[[1]]

    if(inherits(phy_data, "phylo")){

      data_year <- match.phylo.comm(phy_data, comm_mtx)

      mpd_site <- mpd(
        data_year$comm,
        cophenetic(data_year$phy),
        abundance.weighted = TRUE
      ) |> mean(na.rm = T)

      mpd_phy_df <- data.frame(mpd = mpd_site)
    }

    if(inherits(phy_data, "multiPhylo")){
      # compute MPD for each phylogeny
      # future_map makes calculation in parallel
      mpd_phy_df <- future_map(phy_data, function(phy){
        # match species in the phylogeny and in the community
        data_year <- match.phylo.comm(phy, comm_mtx)

        #calculate Standardized Efect size for MPD (SES-MPD)
        mpd_site <- mpd(
          data_year$comm,
          cophenetic(data_year$phy),
          abundance.weighted = TRUE
        ) |> mean(na.rm = T)

        data.frame(mpd = mpd_site)

      }) |>
        list_rbind()
    }

    # average value for each site across 100 phylogenies
    data.frame(
      SiteID = site,
      mpd = colMeans(mpd_phy_df)
    )

  }) |>
    list_rbind()

  mdp_df

}


