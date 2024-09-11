# Function to calculate indirect effects based on SEM coefficients and paths between variables which are
# extracted using semEff() function
# indirect effect = multiply the direct effects of the variables along a specific path

indir_effects <- function(semEff_paths, SEM_coefs){
  # Create a df for indirect effects using the paths from semEff
  df_indir <- as.data.frame(rownames(semEff_paths))
  names(df_indir) <- "path"
  df_indir$path_nro <- paste0("path",seq(1:nrow(df_indir)))

  # Replace "." between variable names with ":" to enable string splitting
  df_indir$path <- gsub(".", ":", df_indir$path, fixed=TRUE)

  # Split variables in each path to their own columns and add also response variable i.e. log_stability
  df_indir$var1 <- str_split_i(df_indir$path, ":", 3)
  df_indir$var2 <- str_split_i(df_indir$path, ":", 2)
  df_indir$var3 <- str_split_i(df_indir$path, ":", 1)
  df_indir$resp_var <- "log_stability"

  # Columns to stroe the effects between variables
  df_indir$var1xvar2 <- NA
  df_indir$var2xvar3 <- NA
  df_indir$var3xresp_var <- NA
  df_indir$indir_effect <- NA

  # Remove unnecessary columns from the model coefficient df
  SEM_coefs %>% select(Response, Predictor, Std.Estimate) -> SEM_coeffs
  paths <- df_indir$path_nro

  # Extract variable effects from SEM coefficients and calculate indirect effects
  for(i in paths){
    print(i)
    path <- df_indir[df_indir$path_nro==i,]
    var1 <- path$var1
    var2 <- path$var2
    var3 <- path$var3
    resp <- path$resp_var

    # Extract coefficients
    if(!is.na(var1)) {
      SEM_coeffs %>% filter(Response==var2 & Predictor==var1) -> coef1
      coef1 <- coef1$Std.Estimate
    } else {
      coef1 <- NA
    }

    SEM_coeffs %>% filter(Response==var3 & Predictor==var2) -> coef2
    coef2 <- coef2$Std.Estimate
    SEM_coeffs %>% filter(Response==resp & Predictor==var3) -> coef3
    coef3 <- coef3$Std.Estimate

    # Store coefficients
    df_indir[df_indir$path_nro==i,"var1xvar2"] <- coef1
    df_indir[df_indir$path_nro==i,"var2xvar3"] <- coef2
    df_indir[df_indir$path_nro==i,"var3xresp_var"] <- coef3

    # Calculate indirect effects for each path
    if(!is.na(coef1)) {
      df_indir[df_indir$path_nro==i,"indir_effect"] <- coef1 * coef2 * coef3
    } else {
      df_indir[df_indir$path_nro==i,"indir_effect"] <- coef2 * coef3
    }

  }
  return(df_indir)
}
