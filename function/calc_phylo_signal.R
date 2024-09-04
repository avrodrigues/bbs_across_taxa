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


library(dplyr)
library(furrr)
library(progressr)
library(picante)
library(phytools)
source("function/delta_stat_code.R")

calc_phylo_signal <- function(
    tree,
    trait_df,
    n_rep = 100,
    file = NULL,
    parallel = FALSE,
    workers = NULL,
    progress = TRUE
) {

  # Check if species names match between tree and trait data
  match_data <- match.phylo.data(tree, trait_df)

  phy <- match_data$phy
  trait_data <- match_data$data %>%
    mutate(
      across(
        where(is.character), ~ ifelse(grepl("^[0-9.]+$", .), as.numeric(.), .)
      )
    )

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

  # Define computation function for each trait
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
      random_delta <- future_map_dbl(1:n_rep, ~ {
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

  # Set up parallelization if enabled
  if (parallel) {

    if(is.null(workers)){
      nworkers <- (availableCores() * .75) %>% round()
    } else {
      nworkers <- workers
    }

    # Use multisession for parallel processing
    plan(multisession, workers = nworkers)
  } else {
    # Fallback to sequential processing
    plan(sequential)
  }

  # Run the computation with optional progress bar
  if (progress) {
    handlers("progress")
    with_progress({
      p <- progressor(along = trait_names)
      phy_sig_df <- future_map_dfr(trait_names, ~ {
        p()  # Update progress bar
        compute_phylo_signal(.x)
      }) #%>% future::value()  # Ensure all tasks are completed before proceeding
    })
  } else {
    phy_sig_df <- future_map_dfr(trait_names, compute_phylo_signal)
  }

  if(nrow(phy_sig_df) == 0){
    stop("No results returned from parallel computation.")
  }

  # Save the result to a file if provided
  if (!is.null(file)) {
    write.csv(phy_sig_df, file, row.names = FALSE)
  }

  return(phy_sig_df)
}
