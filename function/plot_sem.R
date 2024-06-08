


plot_sem <- function(
    sem_dag,
    sem_summary,
    title,
    t_label = "GDD",
    var_names = c(
      "Async" = "log_sqrt_async_l",
      "Spop" = "log_pop_stab",
      "SR" = "avg_richness",
      "FDis" = "f_dis",
      "MPD" = "mpd",
      "CWPoL" = "cwm_pc1",
      "Scom" = "log_stability",
      "mT" = "avg_GDD5",
      "sdT" = "sd_GDD5"
    ),
    model_parts = c("Species Richeness",
                    "Environment",
                    "Traits",
                    "Ecossystem Function"),
    model_parts_col = c("#3d291a", "#a9344f", "#83a6c4", "#578a5b"),
    var_groups = c(
      "Async" = "Ecossystem Function",
      "Scom" = "Ecossystem Function",
      "SR" = "Species Richeness",
      "Spop" = "Ecossystem Function",
      "mT" = "Environment",
      "sdT" = "Environment",
      "CWPoL" = "Traits",
      "FDis" = "Traits",
      "MPD" = "Traits"
    )
){

  require(tidyverse)
  require(ggdag)


  names(model_parts_col) <- model_parts


  rename_var <- names(var_names)
  names(rename_var) <- var_names

  fc <- sem_summary$Cstat
  sem_coefs <- sem_summary$coefficients
  r2_df <- sem_summary$R2 |>
    rename("to" = "Response") |>
    mutate(
      to = str_replace_all(to, rename_var) |>
        str_replace_all( "~~", "")
    ) |>
    select(to, R.squared)

  tidy_dag <- tidy_dagitty(sem_dag) %>%
    mutate(
      model_part = factor(str_replace_all(name, var_groups),
                          levels = model_parts)
    )

  # reorder MPD <-> FDis and Async <-> Spop to be able to join tidy_dag and
  # sem_coefs_tidy


  l_pattern <- c("FDis MPD", "Async Spop")


  for (i in seq_along(l_pattern)) {

    cor_row <- paste(tidy_dag$data$name, tidy_dag$data$to) %>%
      str_detect(pattern = l_pattern[i]) %>%
      which()

    word_to <- word(l_pattern[i], 1, sep = " ")
    word_name <- word(l_pattern[i], 2, sep = " ")

    word_name_row <- which(tidy_dag$data$name == word_name)[1]
    word_to_row <- which(tidy_dag$data$to == word_to)[1]


    tidy_dag$data[cor_row,1:3] <- tidy_dag$data[word_name_row,1:3]
    tidy_dag$data[cor_row,5:7] <- tidy_dag$data[word_to_row,5:7]

  }


  sem_coefs_tidy <- sem_coefs |>
    select(Response, Predictor, DF, P.Value, Std.Estimate) |>
    rename("to" = "Response", "name" = "Predictor") |>
    mutate(
      name = str_replace_all(name, rename_var) |>
        str_replace_all( "~~|log\\(|[[:punct:]]", ""),
      to = str_replace_all(to, rename_var) |>
        str_replace_all( "~~|log\\(|[[:punct:]]", ""),
      significance = ifelse(P.Value <= 0.05, "grey30", "grey80"),
      dir_relation = as.integer(ifelse(Std.Estimate < 0, 2, 1)),
      edge_width = ifelse(abs(Std.Estimate) < 0.15, 0.15, abs(Std.Estimate))
    )

  tidy_dag$data <-
    left_join(tidy_dag$data, sem_coefs_tidy, by = c("name", "to")) |>
    left_join(r2_df, by = "to")|>
    mutate(
      x_r2 = xend + 0.025,
      y_r2 = yend + 0.025,
      r2_label = paste0(R.squared*100, "%")
    ) %>%
    mutate(
      significance = case_when(
        significance == "grey30" ~ ifelse(direction == "<->", "#E49B0F", significance),
        .default = significance
      )
    )




  ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend, color = model_part)) +
    scale_color_manual(values = model_parts_col, name = "Group") +
    geom_dag_point(size = 22) +
    geom_dag_edges(
      aes(edge_colour = significance,
          edge_width = edge_width*4,
          edge_linetype = dir_relation),
      curvature = 0.3,
      arrow_directed = grid::arrow(length = grid::unit(7, "pt"),
                                   type = "closed")
    ) +
    geom_dag_text(col = "white",
                  size = 4.5,
                  fontface = "bold") +
    geom_text(aes(x = .55, y = -.03),
              label = t_label, fontface = "bold",
              size = 4.5) +
    theme_dag() +
    ylim(-0.05, 0.5) +
    coord_fixed() +
    labs(
      title = title
    ) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white", color = NA),
          title = element_text(size = 18)
    )


}

plot_sem_2 <- function(
    sem_dag,
    sem_summary,
    title,
    t_label = "GDD",
    var_names = c(
      "Async" = "log_sqrt_async_l",
      "Spop" = "log_pop_stab",
      "SR" = "avg_richness",
      "FDis" = "f_dis",
      "MPD" = "mpd",
      "CWPoL" = "cwm_pc1",
      "Scom" = "log_stability",
      "mT" = "avg_GDD5",
      "sdT" = "sd_GDD5"
    ),
    model_parts = c("Species Richeness",
                    "Environment",
                    "Traits",
                    "Ecossystem Function"),
    model_parts_col = c("#3d291a", "#a9344f", "#83a6c4", "#578a5b"),
    var_groups = c(
      "Async" = "Ecossystem Function",
      "Scom" = "Ecossystem Function",
      "SR" = "Species Richeness",
      "Spop" = "Ecossystem Function",
      "mT" = "Environment",
      "sdT" = "Environment",
      "CWPoL" = "Traits",
      "FDis" = "Traits",
      "MPD" = "Traits"
    )
){

  require(tidyverse)
  require(ggdag)


  names(model_parts_col) <- model_parts


  rename_var <- names(var_names)
  names(rename_var) <- var_names

  fc <- sem_summary$Cstat
  chi <- sem_summary$ChiSq
  sem_coefs <- sem_summary$coefficients
  aicc <- sem_summary$AIC$AICc

  r2_df <- sem_summary$R2 |>
    rename("to" = "Response") |>
    mutate(
      to = str_replace_all(to, rename_var) |>
        str_replace_all( "~~", "")
    ) |>
    select(to, R.squared)

  tidy_dag <- tidy_dagitty(sem_dag) %>%
    mutate(
      model_part = factor(str_replace_all(name, var_groups),
                          levels = model_parts)
      # col_arrow = ifelse(direction == "<->", "#905000", 'grey30')
    )


  # reorder MPD <-> FDis to be able to join tidy_dag and sem_coefs_tidy

  # cor_row <- paste(tidy_dag$data$name, tidy_dag$data$to) %>%
  #   str_detect(pattern = "FDis MPD") %>%
  #   which()
  #
  # mpd_row <- which(tidy_dag$data$name == "MPD")[1]
  # FDis_row <- which(tidy_dag$data$to == "FDis")[1]
  #
  #
  # tidy_dag$data[cor_row,1:3] <- tidy_dag$data[mpd_row,1:3]
  # tidy_dag$data[cor_row,5:7] <- tidy_dag$data[FDis_row,5:7]

  l_pattern <- c("FDis MPD", "Async Spop")


  for (i in seq_along(l_pattern)) {

    cor_row <- paste(tidy_dag$data$name, tidy_dag$data$to) %>%
      str_detect(pattern = l_pattern[i]) %>%
      which()

    word_to <- word(l_pattern[i], 1, sep = " ")
    word_name <- word(l_pattern[i], 2, sep = " ")

    word_name_row <- which(tidy_dag$data$name == word_name)[1]
    word_to_row <- which(tidy_dag$data$to == word_to)[1]


    tidy_dag$data[cor_row,1:3] <- tidy_dag$data[word_name_row,1:3]
    tidy_dag$data[cor_row,5:7] <- tidy_dag$data[word_to_row,5:7]

  }

  sem_coefs_tidy <- sem_coefs |>
    select(Response, Predictor, DF, P.Value, Std.Estimate) |>
    rename("to" = "Response", "name" = "Predictor") |>
    mutate(
      name = str_replace_all(name, rename_var) |>
        str_replace_all( "~~|log\\(|[[:punct:]]", ""),
      to = str_replace_all(to, rename_var) |>
        str_replace_all( "~~|log\\(|[[:punct:]]", ""),
      significance = ifelse(P.Value <= 0.05, "grey30", "grey80"),
      dir_relation = as.integer(ifelse(Std.Estimate < 0, 2, 1)),
      edge_width = ifelse(abs(Std.Estimate) < 0.15, 0.15, abs(Std.Estimate)),
      coefs = ifelse(significance == "grey30", Std.Estimate, NA)
    )

  tidy_dag$data <- left_join(tidy_dag$data, sem_coefs_tidy, by = c("name", "to")) |>
    mutate(
      x_coefs = case_when(
        direction == "->" ~  x+((xend - x)*0.30),
        direction == "<->" ~ x-((yend - y)*0.125)
      ),
      y_coefs = case_when(
        direction == "->" ~  y+((yend - y)*0.30),
        direction == "<->" ~ y+((yend - y)*0.40)
      )
    ) |>
    left_join(r2_df, by = "to")|>
    mutate(
      x_r2 = xend + 0.025,
      y_r2 = yend + 0.025,
      r2_label = paste0(R.squared*100, "%")
    ) #|>
  #filter(direction %in% c("->",NA))



  ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend, color = model_part)) +
    scale_color_manual(values = model_parts_col, name = "Group") +
    geom_dag_point(size = 22) +
    geom_dag_edges(
      aes(edge_colour = significance,
          edge_width = edge_width*4,
          edge_linetype = dir_relation),
      curvature = 0.3,
      arrow_directed = grid::arrow(length = grid::unit(7, "pt"),
                                   type = "closed")
    ) +
    geom_dag_text(col = "white",
                  size = 4.5,
                  fontface = "bold") +
    ggrepel::geom_label_repel(
      aes(x = x_coefs, y = y_coefs, label = round(coefs, 2)),
      show.legend = F,
      color = "grey30",
      fill = "white",
      label.padding = unit(.25, "lines"),
      label.r = unit(.2, "lines"),
      size = 3.5,
      fontface = "bold"
    ) +
    geom_label(
      aes(x = x_r2, y = y_r2, label = r2_label),
      show.legend = F,
      color = "grey90",
      fill = "grey20",
      label.padding = unit(.25, "lines"),
      label.r = unit(.2, "lines"),
      size = 4,
      fontface = "bold.italic"

    ) +
    geom_text(aes(x = .55, y = -.03),
              label = t_label, fontface = "bold",
              size = 4.5) +
    theme_dag() +
    ylim(-0.05, 0.5) +
    coord_fixed() +
    labs(
      title = title,
      subtitle = glue::glue(
        "Fischer's C = {fc$Fisher.C}; df = {fc$df}; p-value = {fc$P.Value}
        Chi-square = {chi$Chisq}; df = {chi$df}; p-value = {chi$P.Value}
        AICc = {aicc}")
    ) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(size = 22, face = "bold"),
          plot.subtitle = element_text(size = 16)
    )

}
