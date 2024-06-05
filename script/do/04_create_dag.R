library(dagitty)
library(ggdag)
library(ggplot2)
library(tidyverse)

SEM_dag <-
  dagitty(
  'dag {
  Scom [pos="1, 0.3"]
  Spop [pos="0.8,0.4"]
  Async [pos="0.8,0.2"]
  MPD [pos="0.3,0.45"]
  CWPoL [pos="0.3,0.15"]
  FDis [pos="0.3,0.3"]
  SR [pos="0.1,0.3"]
  mT [pos="0.45,0.0"]
  sdT [pos="0.65,0.0"]
  Async -> Scom
  Spop -> Scom
  Spop <-> Async
  FDis -> Async
  FDis <-> CWPoL
  MPD <-> CWPoL
  MPD -> Async
  MPD <-> FDis
  CWPoL -> Async
  CWPoL -> Spop
  SR -> FDis
  SR -> MPD
  mT -> Async
  mT -> FDis
  mT -> MPD
  mT -> CWPoL
  mT -> Spop
  MPD -> Spop
  FDis -> Spop
  sdT -> Async
  sdT -> FDis
  sdT -> MPD
  sdT -> CWPoL
  sdT -> Spop

}'
  )

saveRDS(SEM_dag, "output/sem_dag.rds")

model_parts <- c("Species Richeness", "Environment", "Traits", "Ecossystem Function")
var_groups <- c(
  "Async" = "Ecossystem Function",
  "Spop" = "Ecossystem Function",
  "CWPoL" = "Traits",
  "MPD" = "Traits",
  "SR" = "Species Richeness",
  "Scom" = "Ecossystem Function",
  "mT" = "Environment",
  "sdT" = "Environment",
  "FDis" = "Traits"
)

model_parts_col <- c("#3d291a", "#a9344f", "#83a6c4", "#578a5b")
names(model_parts_col) <- model_parts


(sem_general_model <-
    tidy_dagitty(SEM_dag) %>%
    mutate(
      model_part = factor(str_replace_all(name, var_groups), levels = model_parts),
      col_arrow = ifelse(direction == "<->", "#E49B0F", 'grey30')
    )  |>
    arrange(desc(col_arrow)) |>
    #filter(direction %in% c("->",NA)) |>
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color = model_part), show.legend = F) +
  scale_color_manual(values = model_parts_col, name = "Group") +
  geom_dag_point(size = 20) +
  geom_dag_edges(aes(edge_colour = col_arrow),
    curvature = 0.5,
    edge_width = 1,
    edge_linetype = 1
    ) +
  geom_dag_text(col = "white",
                size = 4,
                fontface = "bold") +
  coord_equal() +
  theme_void() +
  #theme_dag() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA))
)

ggsave(
  "output/figs/00_SEM_general_model.png",
  sem_general_model,
  width = 8,
  height = 4
)


ggsave(
  "output/figs/00_SEM_general_model.pdf",
  sem_general_model,
  width = 8,
  height = 4
)

