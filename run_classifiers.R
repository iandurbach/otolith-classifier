library(caret)
library(tidyverse)

source("do_predmodelling.R")

# !!! make sure to select if you want to use NEFCs or PCs as predictors !!!
usePC <- F

### anchovy
load("proc_data/anch_nefc.RData")
# choose either NEFCs or PCs
if(usePC == T){ot <- ot$ot_pc}else{ot <- ot$ot_nefc}
# shuffle rows
ot <- ot[sample(1:nrow(ot)),]
# make into a data frame with first col a char vector and rest numeric
ot <- data.frame(ot, stringsAsFactors = F) %>% mutate_at(-1, as.numeric)
# run 100 train/test splits and do predictive modelling on each
anchres = list()
for(i in 1:100){
  anchres[[i]] <- do_predmodelling(ot = ot)
}
rm(ot)

### sardine
load("proc_data/sard_4class_nefc.RData")
# choose either NEFCs or PCs
if(usePC == T){ot <- ot$ot_pc}else{ot <- ot$ot_nefc}
# shuffle rows
ot <- ot[sample(1:nrow(ot)),]
# make into a data frame with first col a char vector and rest numeric
ot <- data.frame(ot, stringsAsFactors = F) %>% mutate_at(-1, as.numeric)
# run 100 train/test splits and do predictive modelling on each
sardres = list()
for(i in 1:100){
  sardres[[i]] <- do_predmodelling(ot = ot)
}
rm(ot)

### redeye
load("proc_data/redeye_nefc.RData")
# choose either NEFCs or PCs
if(usePC == T){ot <- ot$ot_pc}else{ot <- ot$ot_nefc}
# shuffle rows
ot <- ot[sample(1:nrow(ot)),]
# make into a data frame with first col a char vector and rest numeric
ot <- data.frame(ot, stringsAsFactors = F) %>% mutate_at(-1, as.numeric)
# run 100 train/test splits and do predictive modelling on each
redeyeres = list()
for(i in 1:100){
  redeyeres[[i]] <- do_predmodelling(ot = ot)
}
rm(ot)

# save output
restype <- ifelse(usePC == T, "pc", "nefc")
save(anchres,sardres,redeyeres,
     file=paste("results/classifier_results_",restype,".RData",sep=""))