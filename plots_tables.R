library(ggplot2)
library(dplyr)

load("results/summary_accuracies.RData")

ggplot(data = allres, aes(x = Accuracy)) + geom_histogram(bins = 20) +
  geom_vline(aes(xintercept = AccuracyNull), color = "red") +
  facet_grid(Method ~ Species)

ggplot(data = allres, aes(x = Kappa)) + geom_histogram(bins = 20) +
  geom_vline(xintercept = 0, color = "red") +
  facet_grid(Method ~ Species)

allres %>% group_by(Species,Method) %>% summarize(meanAcc = mean(Accuracy),
                                                  nullAcc = mean(AccuracyNull),
                                                  meanKappa = mean(Kappa),
                                                  meanS = mean(Sens),
                                                  meanW = mean(Spec),
                                                  prSig = sum(Accuracy < AccuracyNull)/n())
