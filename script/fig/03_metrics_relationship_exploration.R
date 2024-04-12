library(tidyverse)
library(GGally)
library(patchwork)

sem_data_df <- read.csv("data/cleaned/model_df_all_taxa.csv")

sem_data_df <- sem_data_df %>%
  mutate(
    taxa = str_replace_all(
      taxa,
      c("WinterGame" = "Large Mammal", "Rodents" = "Small mammal"))
  )

names(sem_data_df)


cols_taxa <- c(
  "Birds" = "#d3a838",
  "Butterflies" = "#363870",
  "Moths" = "#589eab",
  "Phytoplankton" = "#3d291a",
  "Rodents" = "#bb4455",
  "WinterGame" = "#f9c0c2"
  )

sem_data_df %>%
  filter(taxa == unique(sem_data_df$taxa)[6]) %>%
  select(mpd, f_dis, cwm_pc1) %>%
  cor(method = "spearman")



cor_plot <- function(data, sel_taxa){
  data %>%
    filter(taxa == sel_taxa) %>%
    select(mpd, f_dis, cwm_pc1) %>%
    ggpairs(
      columns = 1:3,
      upper = list(
        continuous = wrap(
          "cor",
          method = "spearman",
          title = "Spearman's\nCorrelation",
          #size = 5,
          color = "grey10"
        )
      ),
      diag = list(
        continuous = wrap(
          'densityDiag',
          fill = "grey50",
          alpha = 0.2
        )
      ),
      columnLabels = c("MPD", "FDis", "CWPoL"),

    ) +
    theme_bw() +
    theme(
      #text = element_text(size = 15),
      panel.grid = element_blank()
    ) +
    labs(title = sel_taxa)


}



taxa_names <- unique(sem_data_df$taxa)



l_plots <- map(taxa_names, function(taxa){

  ggsave(
    paste0(
      "output/figs/cor_plots/cor_plot_",
      tolower(taxa),
      ".png"),
    cor_plot(sem_data_df, taxa),
    width = 5, height = 5
  )
})





