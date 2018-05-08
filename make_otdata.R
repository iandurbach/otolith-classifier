make_otdata <- function(X_co){
  
  # create nefcs and pcs
  X = create_nefc_data(X_co)
  
  # standardize NEFCs for fish length (lm and gam)
  X$nef_std = stdize_by_length(x=X$otdata_original,y=X$nef_pred_original)
  
  # standardize NEFC PCs for fish length (lm and gam)
  X$nefpc_std = stdize_by_length(x=X$otdata_original,y=X$nef_pc_original)
  
  # remove any NEFCs that have a relnship with length that depends on coast
  x_nefc <- X$nef_std$lm_resids[,X$nef_std$lm_sig_int == 0]
  
  # remove any NEFCs that have a relnship with length that depends on coast
  x_pc <- X$nefpc_std$lm_resids[,X$nefpc_std$lm_sig_int == 0]

    # set up dependent variable
  y <- X$coast_original
  #classes <- c("S","W")
  #y <- factor(y,levels=c(0,1),labels=classes)
  
  # set up predictors
  xnefc <- as.data.frame(x_nefc)
  xpc <- as.data.frame(x_pc)
  
  # datasets
  ot_nefc <- cbind(y,x_nefc)
  ot_pc <- cbind(y,x_pc)
  
  return(list(ot_nefc = ot_nefc, ot_pc = ot_pc))
  
}