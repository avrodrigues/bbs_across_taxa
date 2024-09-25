library(tidyverse)


list_tree_dirs <- list.dirs("output/phylo_signal")

sm_lg_mammals_dir <- grep(pattern = "rod|game", list_tree_dirs, value = T)

names(sm_lg_mammals_dir) <- word(sm_lg_mammals_dir, 3, sep = "/") %>%
  word(1, sep = "_")

list_df <- purrr::map(sm_lg_mammals_dir, function(x){

  files_path <- list.files(x, full.names = T)

  purrr::map_df(files_path, read.csv)

})


results_df <- purrr::map(
  list_df, function(x){
    x %>%
      group_by(trait_name) %>%
      summarise(
        stat_name = unique(stat_name),
        avg_stat_value = mean(stat_value),
        sd_stat_value = sd(stat_value),
        signal = sum(signal)
      )


  }
)

walk(1:length(results_df), function(i){

  res_save <- results_df[[i]]
  name_save <- names(results_df)[i]

  file_name <- glue::glue(
    "output/phylo_signal/summary_table_{name_save}.csv"
  )

  write.csv(res_save, file_name, row.names = F)
})
