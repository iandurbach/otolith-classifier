load("results/classifier_results_4class.RData")

allres = c()
allres4 = c()
for(i in 1:100){
  srf2 <- c(sardres_2class[[i]]$rf_res$overall,sardres_2class[[i]]$rf_res$byClass[1:2])
  srf4 <- c(sardres_4class[[i]]$rf_res$overall,sardres_4class[[i]]$rf_res$byClass[,1], sardres_4class[[i]]$rf_res$byClass[,2])
  arf <- c(anchres[[i]]$rf_res1$overall,anchres[[i]]$rf_res1$byClass[1:2])
  rrf <- c(redeyeres[[i]]$rf_res1$overall,redeyeres[[i]]$rf_res1$byClass[1:2])

  ssvm2 <- c(sardres_2class[[i]]$svm_res$overall,sardres_2class[[i]]$svm_res$byClass[1:2])
  ssvm4 <- c(sardres_4class[[i]]$svm_res$overall,sardres_4class[[i]]$svm_res$byClass[,1], sardres_4class[[i]]$svm_res$byClass[,2])
  asvm <- c(anchres[[i]]$svm_res1$overall,anchres[[i]]$svm_res1$byClass[1:2])
  rsvm <- c(redeyeres[[i]]$svm_res1$overall,redeyeres[[i]]$svm_res1$byClass[1:2])
  
  slda2 <- c(sardres_2class[[i]]$lda_res$overall,sardres_2class[[i]]$lda_res$byClass[1:2])
  slda4 <- c(sardres_4class[[i]]$lda_res$overall,sardres_4class[[i]]$lda_res$byClass[,1], sardres_4class[[i]]$lda_res$byClass[,2])
  alda <- c(anchres[[i]]$lda_res1$overall,anchres[[i]]$lda_res1$byClass[1:2])
  rlda <- c(redeyeres[[i]]$lda_res1$overall,redeyeres[[i]]$lda_res1$byClass[1:2])
  
  allres <- rbind(allres,srf2,arf,rrf,ssvm2,asvm,rsvm,slda2,alda,rlda)
  allres4 <- rbind(allres4,srf4,ssvm4,slda4)
}

# add Species and Method to allres
allres <- as.data.frame(allres)
allres$Species <- rep(c("Sardine","Anchovy","Redeye"), times = 300)
allres$Method <- rep(rep(c("RF","SVM","LDA"), each = 3), times = 100)
rownames(allres) <- c()

allres$Species <- factor(allres$Species, levels = c("Sardine","Anchovy","Redeye"))
allres$Method <- factor(allres$Method, levels = c("RF","SVM","LDA"))

# add Species and Method to allres4
allres4 <- as.data.frame(allres4)
allres4$Species <- rep("Sardine", times = 300)
allres4$Method <- rep(rep(c("RF","SVM","LDA"), each = 1), times = 100)
rownames(allres4) <- c()
colnames(allres4)[8:15] <- c("sensKZN", "sensNAM", "sensS", "sensW", "specKZN", "specNAM", "specS", "specW")

allres4$Species <- factor(allres4$Species)
allres4$Method <- factor(allres4$Method, levels = c("RF","SVM","LDA"))

# 
rf_confusion <- matrix(0, 4, 4)
lda_confusion <- matrix(0, 4, 4)
svm_confusion <- matrix(0, 4, 4)
for(i in 1:100){
  rf_confusion <- rf_confusion + sardres_4class[[i]]$rf_res$table
  svm_confusion <- svm_confusion + sardres_4class[[i]]$svm_res$table
  lda_confusion <- lda_confusion + sardres_4class[[i]]$lda_res$table
}
rf_confusion <- rf_confusion / 100
svm_confusion <- svm_confusion / 100
lda_confusion <- lda_confusion / 100

save(allres, allres4, file = "results/summary_accuracies.RData")

