# Visualise indirect effects in SEMs

library(tidyverse)
library(ggplot2)
library(egg)

setwd("C:/LocalData/tuulriss/OneDrive - University of Helsinki/REC/BBS/bbs_across_taxa")

source("./function/group_variables_plotting.R")

var_groups <- c("Environment","Fun_traits","Richness")
var_groups_col = c("#a9344f", "#83a6c4" ,"#3d291a")
names(var_groups_col) <- var_groups

# Birds ----
birds_gdd <- readRDS("./output/SEM_results/indir_effects/birds_indir_gdd.rds")
birds_gdd <- group_variables_for_plotting(birds_gdd)

birds_gdd$path <- as.factor(birds_gdd$path)
birds_gdd$var_group <- as.factor(birds_gdd$var_group)


birds_gdd %>%
  mutate(path = fct_reorder(path, as.integer(var_group))) -> birds_gdd2

ggplot(birds_gdd2, aes(x=path, y=indir_effect, fill=var_group)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values=var_groups_col) +
  theme_minimal()




