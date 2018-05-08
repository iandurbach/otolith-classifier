# converts coordinates into NEFC and PCs of NEFCs (run aftr create_data.r)

create_nefc_data = function(combined_list){
  #converting the otolith data matrix into a data frame
  otdata <- as.data.frame(combined_list[[1]],stringsAsFactors=F)
  head(otdata)
  
  #define a new list equal to only the coordinate part of combined_data
  coords <- combined_list[[2]]
  n_ots = length(coords)
  
  #converting the variables in the data frame to the appropriate types
  otdata[,"length"] <- as.numeric(as.character(otdata[,"length"]))
  
  #defining a binary response variable for coast membership
  # coast <- as.factor(as.numeric(1*(otdata$coast=="West")))
  coast <- otdata$coast
  
  #creating the predictor matrix of the NEFDs
  
  #data preparation for modelling using the principal components of the NEFDs
  
  nef <- nef_set(coords=coords)
  length(nef)
  
  for (i in 1:10)
  {
    A <- paste("A", i, sep = "")
    assign(A,give_vector(harm=1,coeff=i,nef=nef,n=n_ots))
    
    B <- paste("B", i, sep = "")
    assign(B,give_vector(harm=2,coeff=i,nef=nef,n=n_ots))
    
    C <- paste("C", i, sep = "")
    assign(C,give_vector(harm=3,coeff=i,nef=nef,n=n_ots))
    
    D <- paste("D", i, sep = "")
    assign(D,give_vector(harm=4,coeff=i,nef=nef,n=n_ots))
  }
  
  nef_pred <- cbind(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,
                    C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10)
  
  #defining a data matrix for modelling using NEFCs
  data <- as.data.frame(cbind(coast,nef_pred))
  data[,1] <- as.factor(as.numeric(1*(otdata$coast=="West")))
  #corrects for the data frame coercing the coast factor variable to be outside the range (0,1)
  head(data)
  
  #Finding the PCs of the NEFCs
  
  nef_mean <- colMeans(nef_pred)
  
  nef_center <- matrix(NA,nrow=nrow(data),ncol=40)
  
  for (j in 1:40)
  {
    for (i in 1:nrow(data))
    {
      nef_center[i,j]<-nef_pred[i,j]-nef_mean[j]
    }
  }
  colMeans(nef_center)
  
  nef_pc<-princomp(nef_center)
  
  #Scree plots for PCs of NEFCs
  par(mfrow=c(1,1))
  plot(nef_pc)
  plot(nef_pc$sdev,type="b",pch=16,ylab="Prop. variance explained",xlab="PC Number")
  abline(v=10,col="red",lty=2)
  nef_pc$sdev
  #an elbow in the scree plot could be said to occur at PC10
  
  nef_pc$loadings
  
  #examining the matrix of the loadings on the 40 principal components
  head(nef_pc$scores)
  
  #creating a matrix of the first 10 principal components for use in modelling
  nef_pc<-nef_pc$scores[,1:10]
  
  #saving the original data and predictor matrices
  otdata_original <- otdata
  nef_pred_original <- nef_pred
  nef_pc_original <- nef_pc
  coast_original <- coast
  
  return(list(otdata_original = otdata_original, nef_pred_original = nef_pred_original,
                  nef_pc_original = nef_pc_original, coast_original = coast_original))
  
}