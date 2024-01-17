#' Compute metrics of stability, synchrony and species richness of communities
#'
#' @param comm_data a data frame with community dynamics.
#'
#' @return a lists with tree values.
#'
#'  stab: the stability metric
#'  sync_l: the Loureau's metric of synchrony
#'  sync_g: the Gross's metric of synchrony
#'
#' @export
#'
#' @examples
#'
comm_dyn_metrics <- function(
    comm_data,
    time.var = "Year",
    species.var = "Species",
    abundance.var = "Abundance",
    replicate.var = "SiteID")
  {

  require(codyn)
  source("function/pop_stab.R")


  stab <- community_stability(
    comm_data,
    time.var = time.var,
    abundance.var = abundance.var,
    replicate.var = replicate.var
  )

  pop_stab_vec <- pop_stab(
    comm_data,
    time.var = time.var,
    species.var = species.var,
    abundance.var = abundance.var,
    replicate.var = replicate.var
  )

  sync_l <- synchrony(
    comm_data,
    time.var = time.var,
    species.var = species.var,
    abundance.var = abundance.var,
    replicate.var = replicate.var,
    metric = "Loreau"

  )

  sync_g <- synchrony(
    comm_data,
    time.var = time.var,
    species.var = species.var,
    abundance.var = abundance.var,
    replicate.var = replicate.var,
    metric = "Gross"
  )



  ## species richness ----
  richness <-
    comm_data |>
    count(.data[[replicate.var]], .data[[time.var]], name = "richness") |>
    arrange(.data[[replicate.var]], .data[[time.var]]) |>
    group_by(.data[[replicate.var]]) |>
    summarise(
      avg_richness = mean(richness))

  data.frame(
    SiteID = stab[, replicate.var],
    stability = stab$stability,
    pop_stab = pop_stab_vec,
    async_l = 1/sync_l$synchrony,
    async_g = 1-sync_g$synchrony,
    avg_richness = richness$avg_richness

  )
}


