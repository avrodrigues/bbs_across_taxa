gt_model <- function(sem_list, taxon){
  require(gt)
  require(janitor)
  require(piecewiseSEM)
  require(tidyverse)

  sem_fc <- map(sem_list, fisherC) |> list_rbind()
  sem_aic <- map(sem_list, AIC_psem) |> list_rbind() |>
    select(-AIC)
  sem_ll_chisq <- map(sem_list, LLchisq) |> list_rbind()

  mod_df <- data.frame(model = names(sem_list),
                       sem_fc, sem_ll_chisq, sem_aic) |>
    janitor::clean_names() |>
    rename("aic_c" = "ai_cc",
           "df_c" = "df",
           "p_val_c" = "p_value",
           "df_chi" = "df",
           "p_val_chi" = "p_value_1") |>
    arrange(aic_c)

  gt(mod_df) |>
    tab_header(
      title = "Model comparison GGD vs. FDD",
      subtitle = taxon
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = "p_val_c",
        rows = p_val_c > .05
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = "p_val_chi",
        rows = p_val_chi > .05
      )
    )

}
