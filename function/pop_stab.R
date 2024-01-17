#' Weighted Average of Population Stability for one community
#'
#' Calculates Weighted Average of Population Stability based on the equation 4
#' of Thibaut and Connolly (2013)
#'
#' @param comm_mtx matrix of abundances for one community. Years in rows, species in columns.
#'
#' @return Weighted Average of Population Stability for the community
#' @export
#'
#' @references Thibaut and Connolly (2013). Understanding diversity–stability
#'   relationships: towards a unified model of portfolio effects.
#'   Ecology Letters. doi.org/10.1111/ele.12019
#'
#' @examples
pop_stab_one_comm <- function(comm_mtx){

  mu_sp <- colMeans(comm_mtx)
  sd_sp <- apply(comm_mtx, 2, sd, na.rm = T)
  mu_tot <- mean(rowSums(comm_mtx,na.rm=T))

  stab.sp <- 1/sum((mu_sp/mu_tot) * (sd_sp/mu_sp))

  c("pop_stability" = stab.sp)
}


#' Weighted Average of Population Stability for multiple communities
#'
#' @param comm_data A data frame containing time, species and abundance columns and an column of communities
#' @param time.var The name of the time column
#' @param species.var The name of the species column
#' @param abundance.var The name of the abundance column
#' @param replicate.var The name of the optional replicate column, identifying the communities
#'
#' @return
#' @export
#'
#' @examples
pop_stab <- function(
    comm_data,
    time.var = "Year",
    species.var = "Species",
    abundance.var = "Abundance",
    replicate.var = "SiteID"){

  require(codyn)
  require(tidyr)

  l_comm_data <- split(comm_data, comm_data[,replicate.var])

  map_dbl(l_comm_data, function(site_data){
    comm_df <- site_data |>
      pivot_wider(
        id_cols = all_of(time.var),
        names_from = all_of(species.var),
        values_from = all_of(abundance.var),
        values_fill = 0
      )

    comm_mtx <- as.matrix(comm_df[,-1])
    rownames(comm_mtx) <- comm_df[[1]]


    pop_stab_one_comm(comm_mtx)
  })

}
