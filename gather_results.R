load("results/classifier_results.RData")

allres = c()
for(i in 1:100){
  srf <- c(sardres[[i]]$rf_res1$overall,sardres[[i]]$rf_res2)
  arf <- c(anchres[[i]]$rf_res1$overall,anchres[[i]]$rf_res2)
  rrf <- c(redeyeres[[i]]$rf_res1$overall,redeyeres[[i]]$rf_res2)

  ssvm <- c(sardres[[i]]$svm_res1$overall,sardres[[i]]$svm_res2)
  asvm <- c(anchres[[i]]$svm_res1$overall,anchres[[i]]$svm_res2)
  rsvm <- c(redeyeres[[i]]$svm_res1$overall,redeyeres[[i]]$svm_res2)
  
  slda <- c(sardres[[i]]$lda_res1$overall,sardres[[i]]$lda_res2)
  alda <- c(anchres[[i]]$lda_res1$overall,anchres[[i]]$lda_res2)
  rlda <- c(redeyeres[[i]]$lda_res1$overall,redeyeres[[i]]$lda_res2)
  
  allres <- rbind(allres,srf,arf,rrf,ssvm,asvm,rsvm,slda,alda,rlda)
}
allres <- as.data.frame(allres)
allres$Species <- rep(c("Sardine","Anchovy","Redeye"), times = 300)
allres$Method <- rep(rep(c("RF","SVM","LDA"), each = 3), times = 100)
rownames(allres) <- c()

allres$Species <- factor(allres$Species, levels = c("Sardine","Anchovy","Redeye"))
allres$Method <- factor(allres$Method, levels = c("RF","SVM","LDA"))

save(allres, file = "results/summary_accuracies.RData")

