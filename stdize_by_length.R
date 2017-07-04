stdize_by_length = function(x,y,coast){
  
  coast = ifelse(x$coast == "West", 1, 0)
  x = x$length
  
  # standardizing for fish length with lm (extracts residuals)
  
  lm_resids = matrix(NA,nrow(y),ncol(y))
  lm_sig_int = c()
  for(i in 1:ncol(y)){
    # check if interaction effect between coast and length
    mod = lm(y[,i] ~ coast * x) 
    lm_sig_int[i] = ifelse(summary(mod)$coefficients[4,4] < 0.05, 1, 0)
    # find residuals
    lm_mod = lm(y[,i] ~ coast + x)
    lm_resids[,i] = lm_mod$residuals + lm_mod$coefficients[2] * coast
  }
  
  # standardizing for fish length with gam (extracts residuals)
  
  gam_resids = matrix(NA,nrow(y),ncol(y))
  gam_sig_int = c()
  for(i in 1:ncol(y)){
    # check if interaction effect between coast and length
    mod1 = gam(y[,i] ~ s(x) + coast)
    mod2 = gam(y[,i] ~ s(x) + coast + ti(x,by=coast))
    gam_sig_int[i] = ifelse(summary(mod2)$s.pv[2] < 0.05, 1, 0)
    # find residuals
    gam_mod = gam(y[,i] ~ coast + s(x))
    gam_resids[,i] = gam_mod$residuals + gam_mod$coefficients[2] * coast
  }
  
  return(list(lm_resids = lm_resids, lm_sig_int = lm_sig_int, 
              gam_resids = gam_resids, gam_sig_int = gam_sig_int))
  
}