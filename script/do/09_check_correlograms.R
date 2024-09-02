
# load library ------------------------------------------------------------

library(tidyverse)
library(ncf)


# load data ---------------------------------------------------------------

morans_df <- readRDS("output/SEM_results/spatial_autocorrelation/morans_i_residuals.rds")


# filter the residuals that present spatial auto correlation
morans_true_df <- morans_df %>%
  filter(spat_autocor)


# elements selected from correlog object
correlog_vars <- c("n", "mean.of.class", "correlation", "p" )

# Significance colors
sig_color <- c("grey50", "red")


data_cor <- morans_true_df %>%
  mutate(
    # create the correlogram plot with ggplot
    corr_plot = pmap(list(correlogram, dataset, sem_model, variable, morans_I, p_value),
                     \(cl, dataset, sem_model, variable, morans_I, p_value){

      correlog_df <- cl[correlog_vars] %>% as_tibble() %>%
        mutate(sig = p <= 0.05)

      model <- str_replace_all(sem_model, "SEM_|5", "")


      ggplot(correlog_df, aes(x = mean.of.class/1000, y = correlation)) +
        geom_line() +
        geom_point(aes(colour = sig)) +
        scale_colour_manual(values = sig_color, name = "Significant") +
        ylim(-1,1) +
        labs(
          title = paste0(dataset, " (", model, ")"),
          subtitle = paste0(
            variable, "\nMoran's I: ", round(morans_I, 3),
            "\nP-value:", round(p_value, 4)
          ),
          x = "Mean of Class (Km)", y = "Moran's I") +
        theme_bw() +
        theme(
          legend.position = "bottom"
        )
    })
  )


walk(seq_along(data_cor$corr_plot), \(i){
  ggsave(
    paste0("output/SEM_results/spatial_autocorrelation/correlog_", sprintf("%02d", i), ".png"),
    data_cor$corr_plot[[i]]
  )
})

