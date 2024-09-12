# Plot indirect effects in SEMs

plot_indirect_effects <- function(df, variable_colors, title){
  df$path <- as.factor(df$path)
  df$var_group <- as.factor(df$var_group)

  df %>%
    mutate(path = fct_reorder(path, as.integer(var_group))) -> df2

  ggplot(df2, aes(x=path, y=indir_effect, fill=var_group)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_manual(values=variable_colors) +
    theme_minimal() +
    theme(legend.position="none")+
    xlab("") +
    ylab("") +
    ggtitle(title)

}
