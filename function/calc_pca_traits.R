# calculate and plot PCA on trait values

calc_pca_traits <- function(traits_std){

  t_to_pca <- traits_std[,-1]
  row.names(t_to_pca) <- traits_std[,1]

  pca_traits <- prcomp(t_to_pca)

  pca_plot <- autoplot(pca_traits,
           loadings = TRUE, loadings.colour = '#83a6c4',
           loadings.label = TRUE, loadings.label.size = 4,
           label.position = "identity", scale = 0) +
    theme_bw()

  list(pca_traits = pca_traits, pca_plot = pca_plot)
}
