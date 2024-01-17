# load packages ----
library(tidyverse)


model_files <- list.files(pattern = "_sem.rds", recursive = T)

model_results <- map(model_files, readRDS)
names(model_results) <- c("Birds", "Butterflies", "Moths", "Phytoplankton", "Small Rodents", "Mammals")


calc_gof_metrics <- function(sem_list, taxon){
  require(janitor)
  require(piecewiseSEM)
  require(tidyverse)


  sem_fc <- map(sem_list, fisherC) |> list_rbind() |>
    mutate(
      `Fisher's C (df, P)` = glue::glue("{Fisher.C} ({df}, {P.Value})")
    ) |>
    select(4)

  sem_ll_chisq <- map(sem_list, LLchisq) |> list_rbind() |>
    mutate(
      `χ2 (df, P)` = glue::glue("{Chisq} ({df}, {P.Value})")
    ) |>
    select(4)

  sem_aic <- map(sem_list, AIC_psem) |> list_rbind() |>
    select(AICc) |>
    mutate(
      ΔAICc = AICc-min(AICc)
    )

  mod_df <- cbind(sem_fc, sem_ll_chisq, sem_aic) |>
     mutate(
      .before = 1,
      Taxa = taxon,
      Model = names(sem_list) |> word(2, sep = "_") |> toupper() |> str_sub(1, 3)) |>
    arrange(ΔAICc)

  mod_df
}


gof_df <- map(1:6, function(i){
  calc_gof_metrics(model_results[[i]], names(model_results)[i])
}) |> list_rbind()

gof_df |>
  gt(rowname_col = "row", groupname_col = "Taxa") |>
  sub_missing()  |>
  gtsave("output/SEM_results/table_2.docx")

