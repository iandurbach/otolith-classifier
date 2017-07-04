make_otdata <- function(X_co){
  
  # create nefcs and pcs
  X = create_nefc_data(X_co)
  
  # standardize NEFCs for fish length (lm and gam)
  X$nef_std = stdize_by_length(x=X$otdata_original,y=X$nef_pred_original)
  
  # standardize NEFC PCs for fish length (lm and gam)
  X$nefpc_std = stdize_by_length(x=X$otdata_original,y=X$nef_pc_original)
  
  # remove any variables that have a relnship with length that depends on coast
  x <- X$nef_std$lm_resids[,X$nef_std$lm_sig_int == 0]
  
  # set up dependent variable
  y <- X$coast_original
  classes <- c("S","W")
  y <- factor(y,levels=c(0,1),labels=classes)
  
  # set up predictors
  x <- as.data.frame(x)
  
  # dataset
  ot <- cbind(y,x)
  
  return(ot)
  
}