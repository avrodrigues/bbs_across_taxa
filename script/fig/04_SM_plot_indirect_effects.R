# Visualise indirect effects in SEMs

library(tidyverse)
library(ggplot2)
library(egg)

source("./function/group_variables_plotting.R")
setwd("C:/LocalData/tuulriss/OneDrive - University of Helsinki/REC/BBS/bbs_across_taxa")

var_groups <- c("Avg_richness","Fun_traits", "Temp_vars")
var_groups_col = c("#3d291a", "#83a6c4", "#a9344f")
names(var_groups_col) <- var_groups

# Birds ----
birds_gdd <- readRDS("./output/SEM_results/indir_effects/birds_indir_gdd.rds")
birds_gdd <- group_variables_for_plotting(birds_gdd)

birds_gdd %>%
  mutate(path = fct_reorder(path, as.integer(var_group))) -> birds_gdd2

birds_gdd %>% arrange(var_group) -> birds_gdd2

ggplot(birds_gdd2, aes(x=path, y=indir_effect, fill=var_group)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values=var_groups_col) +
  scale_y_discrete(limits=var_groups)+
  theme_minimal()




