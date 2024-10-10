# plot a gt table with the results of the d-separation test


gt_dsep <- function(dsep, taxon){
  require(gt)
  require(janitor)

  dsep |>
  janitor::clean_names() |>
    select(independ_claim, p_value) |>
    mutate(p_value = round(p_value, 3)) |>
    gt() |>
    tab_header(
      title = "Test of directed separation",
      subtitle = taxon
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = p_value,
        rows = p_value <= 0.05
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = independ_claim,
        rows = p_value <= 0.05
      )
    )

}
