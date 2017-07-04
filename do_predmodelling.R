do_predmodelling <- function(ot){
  
  # partition data into training and test
  trainIndex <- createDataPartition(ot$y, p = .7, list = FALSE)
  ot_tr <- ot[trainIndex, ]
  ot_te <- ot[-trainIndex, ]
  
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
  lda_test <- data.frame(obs = ot$y[-trainIndex], 
                         pred = predclass, 
                         S = predprob[,1], W = predprob[,2])
  
  lda_res1 <- confusionMatrix(data = lda_test$pred, reference = lda_test$obs)
  lda_res2 <- twoClassSummary(lda_test, lev = c("S","W"))
  lda_res3 <- roc(lda_test$obs, lda_test$S)
  
  
  # RF
  ctrl <- trainControl(method = "cv", number = 10, savePred=T, classProb=T)
  rf <- train(y ~ ., data = ot_tr, method = "rf", tuneLength = 5, trControl = ctrl)
  rf$finalModel$confusion
  
  predclass <- predict(rf, ot_te,type="raw")
  predprob <- predict(rf, ot_te,type="prob")
  rf_test <- data.frame(obs = ot_te$y, 
                        pred = predclass, 
                        S = predprob[,1], W = predprob[,2])
  
  rf_res1 <- confusionMatrix(data = rf_test$pred, reference = rf_test$obs)
  rf_res2 <- twoClassSummary(rf_test, lev = c("S","W"))
  rf_res3 <- roc(rf_test$obs, rf_test$S)
  
  # SVM
  ctrl <- trainControl(method = "cv", number = 10, savePred=T, classProb=T)
  svm <- train(y ~ .,data = ot_tr, method = "svmRadial", tuneLength = 5, trControl = ctrl)
  
  predclass <- predict(svm, ot_te[,-1],type="raw")
  predprob <- predict(svm, ot_te[,-1],type="prob")
  svm_test <- data.frame(obs = ot_te$y, 
                         pred = predclass, 
                         S = predprob[,1], W = predprob[,2])
  
  svm_res1 <- confusionMatrix(data = svm_test$pred, reference = svm_test$obs)
  svm_res2 <- twoClassSummary(svm_test, lev = c("S","W"))
  svm_res3 <- roc(svm_test$obs, svm_test$S)
  
  return(list(lda_res1 = lda_res1, lda_res2 = lda_res2, lda_res3 = lda_res3,
              rf_res1 = rf_res1, rf_res2 = rf_res2, rf_res3 = rf_res3,
              svm_res1 = svm_res1, svm_res2 = svm_res2, svm_res3 = svm_res3))
}