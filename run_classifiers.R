library(caret)
library(pROC)

source("do_predmodelling.R")

### anchovy
load("proc_data/anch_nefc.RData")
# shuffle rows
ot <- ot[sample(1:nrow(ot)),]
# run 100 train/test splits and do predictive modelling on each
anchres = list()
for(i in 1:100){
  anchres[[i]] <- do_predmodelling(ot = ot)
}
rm(ot)

### sardine
load("proc_data/sard_nefc.RData")
# shuffle rows
ot <- ot[sample(1:nrow(ot)),]
# run 100 train/test splits and do predictive modelling on each
sardres = list()
for(i in 1:100){
  sardres[[i]] <- do_predmodelling(ot = ot)
}
rm(ot)

### redeye
load("proc_data/redeye_nefc.RData")
# shuffle rows
ot <- ot[sample(1:nrow(ot)),]
# run 100 train/test splits and do predictive modelling on each
redeyeres = list()
for(i in 1:100){
  redeyeres[[i]] <- do_predmodelling(ot = ot)
}
rm(ot)

save(anchres,sardres,redeyeres,file="results/classifier_results.RData")