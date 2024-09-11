# Group variables to plot SEM indirect effects

group_variables_for_plotting <- function(df){
  df$first_var <- ifelse(!is.na(df$var1), df$var1, df$var2)
  df$var_group <- NA

  first_vars <- c("avg_richness", "f_dis", "mpd", "cwm_pc1", "avg_GDD5", "sd_GDD5")

  for(i in first_vars){
    print(i)
    if(i == "avg_richness"){
      df[df$first_var==i,"var_group"] <- "Avg_richness"
    } else if(i == "f_dis" | i == "mpd" | i=="cwm_pc1"){
      df[df$first_var==i,"var_group"] <- "Fun_traits"
    } else if(i=="avg_GDD5" | i =="sd_GDD5"){
      df[df$first_var==i,"var_group"] <- "Temp_vars"
    }
  }
  return(df)
}
