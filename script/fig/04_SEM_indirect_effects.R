# Calculate and visualise direct and indirect effects in SEMs

#install_version("piecewiseSEM", version="2.3.0")

library(tidyverse)
library(piecewiseSEM)
library(remotes)

# Birds ----

birds_sem <- readRDS("output/SEM_results/model_output/birds_sem.rds")
