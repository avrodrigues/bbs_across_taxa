
# load packages -----------------------------------------------------------


library(tidyverse)
library(GGally)
library(patchwork)


# load data ---------------------------------------------------------------


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

# Relationship among Functiona traits metris -----------------------

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



# relatioships btwn FD  and Env -------------------------------------------


names(sem_data_df)

resp <- c("mpd", "f_dis", "cwm_pc1")
env <- c("avg_FDD", "sd_FDD", "avg_GDD5", "sd_GDD5")


resp_long_df <-
sem_data_df %>%
  select(taxa, SiteID, all_of(resp)) %>%
  pivot_longer(
    cols = all_of(resp),
    names_to = "resp_var",
    values_to = "resp_value"
  )

env_long_df <-
  sem_data_df %>%
  select(taxa, SiteID, all_of(env)) %>%
  pivot_longer(
    cols = all_of(env),
    names_to = "env_var",
    values_to = "env_value"
  )

resp_env <-
 left_join(resp_long_df, env_long_df, by = c("taxa", "SiteID"),
 relationship = "many-to-many") %>%
  mutate(
    resp_var = as.factor(resp_var),
    env_var = as.factor(env_var)
  )


taxa_names <- unique(sem_data_df$taxa)

fd_env_plots <- map(1:6, function(i){
  resp_env %>%
    filter(taxa == taxa_names[i]) %>%
    ggplot(aes(x = env_value, y = resp_value, color = taxa)) +
    geom_point() +
    labs(title = taxa_names[i]) +
    facet_grid(rows = vars(resp_var), cols =  vars(env_var), scales = "free") +
    theme_bw()

})

fd_env_plots[[6]]


