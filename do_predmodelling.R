do_predmodelling <- function(ot, trainSample = 0.7, trainGroupSizes = NULL){
  
  # if trainGroupSizes is NULL then take fixed proportion of training samples per group, if 
  # not NULL then first take the fixed prop and then downsample specified numbers from these
  if(is.null(trainGroupSizes)){
    trainIndex <- createDataPartition(ot$y, p = trainSample, list = FALSE)
    testIndex <- -trainIndex
  }else{
    # add row_ids as can get jumbled up later
    nested_ot <- ot %>% 
      mutate(row_id = 1:nrow(ot))
    
    # extract trainSample % of the data (NB: rest will be test data)
    pretrainIndex <- createDataPartition(nested_ot$y, p = trainSample, list = FALSE)
    testIndex <- -pretrainIndex
    
    # downsample specified numbers per group (rows in pretrainIndex but not in downsampled data are
    # essentially excluded - not in test or training, see above)
    # this bit from https://jennybc.github.io/purrr-tutorial/ls12_different-sized-samples.html
    nested_ot <- nested_ot %>% 
      filter(row_id %in% pretrainIndex) %>%
      group_by(y) %>%   
      nest() %>%           
      arrange(y) %>%
      mutate(n = trainGroupSizes)
    
    sampled_ot <- nested_ot %>%
      mutate(samp = map2(data, n, sample_n))
    
    trainIndex <- sampled_ot %>% 
      select(y, samp) %>%
      unnest() %>%
      select(row_id) %>% 
      unlist() %>% as.vector()  
  }
  
  
  # partition data into training and test
  ot_tr <- ot[trainIndex, ]
  ot_te <- ot[testIndex, ]
  
  # scale the data by mean and sd of the training set
  ms <- apply(ot_tr[,-1],2,mean)
  sds <- apply(ot_tr[,-1],2,sd)
  ot_tr[,-1] <- t(apply(ot_tr[,-1],1,function(z){(z-ms)/sds}))
  ot_te[,-1] <- t(apply(ot_te[,-1],1,function(z){(z-ms)/sds}))
  
  # apply ML methods
  
  # LDA
  ctrl <- trainControl(method = "none", savePred=T, classProb=T)
  lda <- train(y ~ ., data = ot_tr, method = "lda", trControl = ctrl)
  
  predclass <- predict(lda, ot_te,type="raw")
  predprob <- predict(lda, ot_te,type="prob")
  obsclass <- ot$y[testIndex]
  
  lda_res <- confusionMatrix(data = predclass, reference = obsclass)

  # RF
  ctrl <- trainControl(method = "cv", number = 10, savePred=T, classProb=T)
  rf <- train(y ~ ., data = ot_tr, method = "rf", tuneLength = 5, trControl = ctrl)
  rf$finalModel$confusion
  
  predclass <- predict(rf, ot_te,type="raw")
  predprob <- predict(rf, ot_te,type="prob")
  obsclass <- ot$y[testIndex]
  
  rf_res <- confusionMatrix(data = predclass, reference = obsclass)

  # SVM
  ctrl <- trainControl(method = "cv", number = 10, savePred=T, classProb=T)
  svm <- train(y ~ .,data = ot_tr, method = "svmRadial", tuneLength = 5, trControl = ctrl)
  
  predclass <- predict(svm, ot_te[,-1],type="raw")
  predprob <- predict(svm, ot_te[,-1],type="prob")
  obsclass <- ot$y[testIndex]
  
  svm_res <- confusionMatrix(data = predclass, reference = obsclass)

  return(list(lda_res = lda_res, rf_res = rf_res, svm_res = svm_res))
}