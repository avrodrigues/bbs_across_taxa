fit_bbs_sem <- function(model_data){
  require(piecewiseSEM)

  SEM_gdd5 <- psem(
    lm(log_stability ~ log_pop_stab + log_sqrt_async_l, data = model_data),
    lm(log_pop_stab ~  mpd + f_dis + cwm_pc1 + avg_GDD5 + sd_GDD5, data = model_data),
    lm(cwm_pc1 ~ avg_GDD5 + sd_GDD5, data = model_data),
    lm(log_sqrt_async_l ~  mpd + f_dis + cwm_pc1 + avg_GDD5 + sd_GDD5, data = model_data),
    lm(mpd ~ avg_richness + avg_GDD5 + sd_GDD5, data = model_data),
    lm(f_dis ~ avg_richness + avg_GDD5 + sd_GDD5, data = model_data),
    f_dis %~~% mpd,
    mpd %~~% cwm_pc1,
    f_dis %~~% cwm_pc1,
    log_sqrt_async_l %~~% log_pop_stab,
    data = model_data
  )

  SEM_fdd<- psem(
    lm(log_stability ~ log_pop_stab + log_sqrt_async_l, data = model_data),
    lm(log_pop_stab ~ mpd + f_dis + cwm_pc1 + avg_FDD + sd_FDD, data = model_data),
    lm(cwm_pc1 ~ avg_FDD + sd_FDD, data = model_data),
    lm(log_sqrt_async_l ~ mpd + f_dis + cwm_pc1 + avg_FDD + sd_FDD, data = model_data),
    lm(mpd ~ avg_richness + avg_FDD + sd_FDD, data = model_data),
    lm(f_dis ~ avg_richness + avg_FDD + sd_FDD, data = model_data),
    f_dis %~~% mpd,
    mpd %~~% cwm_pc1,
    f_dis %~~% cwm_pc1,
    log_sqrt_async_l %~~% log_pop_stab,
    data = model_data
  )

  # summary results
  list(SEM_gdd5 = SEM_gdd5,
       SEM_fdd = SEM_fdd)
}
