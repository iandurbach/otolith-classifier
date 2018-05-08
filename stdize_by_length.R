stdize_by_length = function(x,y){
  
  coast = x$coast
  x = x$length
  
  # standardizing for fish length with lm (extracts residuals)
  
  lm_resids = matrix(NA,nrow(y),ncol(y))
  lm_sig_int = c()
  for(i in 1:ncol(y)){
    # check if interaction effect between coast and length
    mod = lm(y[,i] ~ coast * x) 
    lm_sig_int[i] = ifelse(anova(mod)$`Pr(>F)`[3] < 0.05, 1, 0)
    # find residuals
    lm_mod = lm(y[,i] ~ x + coast)
    coast_coeffs <- str_detect(names(lm_mod$coefficients), "coast")
    add_back <- data.frame(coast = str_replace_all(names(lm_mod$coefficients)[coast_coeffs],"coast",""),
                           coeffs = lm_mod$coefficients[coast_coeffs])
    lm_coastpreds_df <- tibble(id = 1:nrow(y), coast = coast) %>% 
      left_join(add_back) %>% replace_na(list(coeffs = 0))
    lm_resids[,i] = lm_mod$residuals + lm_coastpreds_df$coeffs
  }
  
  # standardizing for fish length with gam (extracts residuals)
  
  gam_resids = matrix(NA,nrow(y),ncol(y))
  gam_sig_int = c()
  gam.df <- data.frame(y = y[,i], x = x, coast = coast)
  for(i in 1:ncol(y)){
    # check if interaction effect between coast and length
    mod1 <- gam(y ~ s(x) + coast, data = gam.df)
    mod2 <- gam(y ~ s(x) + coast + ti(x, by = coast), data = gam.df)
    gam_sig_int[i] = ifelse(sum(summary(mod2)$s.pv[-1] < 0.05) > 0, 1, 0)
    # find residuals
    gam_mod = gam(y ~ s(x) + coast, data = gam.df)
    coast_coeffs <- str_detect(names(gam_mod$coefficients), "coast")
    add_back <- data.frame(coast = str_replace_all(names(gam_mod$coefficients)[coast_coeffs],"coast",""),
                           coeffs = gam_mod$coefficients[coast_coeffs])
    gam_coastpreds_df <- tibble(id = 1:nrow(y), coast = coast) %>% 
      left_join(add_back) %>% replace_na(list(coeffs = 0))
    gam_resids[,i] = gam_mod$residuals + gam_coastpreds_df$coeffs
  }
  
  return(list(lm_resids = lm_resids, lm_sig_int = lm_sig_int, 
              gam_resids = gam_resids, gam_sig_int = gam_sig_int))
  
}