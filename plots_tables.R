library(ggplot2)
library(dplyr)

load("results/summary_accuracies.RData")

p1 <- ggplot(data = allres, aes(x = Accuracy)) + geom_histogram(bins = 20) +
  geom_rect(aes(xmin=159683.438, xmax=159684.186, ymin=0, ymax=Inf))
  geom_vline(aes(xintercept = AccuracyNull), color = "red") +
  facet_grid(Method ~ Species) + 
  theme_bw(base_size=24) + ylab("Count") +
  theme(axis.text = element_text(size = 20))
ggsave("results/nefc_accuracy.png", p1, width=12, height=7, dpi=200)

p2 <- ggplot(data = allres, aes(x = Kappa)) + geom_histogram(bins = 20) +
  geom_vline(xintercept = 0, color = "red") +
  facet_grid(Method ~ Species) + 
  theme_bw(base_size=24) + ylab("Count") +
  theme(axis.text = element_text(size = 20))
ggsave("results/nefc_kappa.png", p2, width=12, height=7, dpi=200)

t1 <- allres %>% group_by(Species,Method) %>% summarize(meanAcc = mean(Accuracy),
                                                  nullAcc = mean(AccuracyNull),
                                                  meanKappa = mean(Kappa),
                                                  meanS = mean(Sens),
                                                  meanW = mean(Spec),
                                                  prSig = sum(Accuracy > AccuracyNull)/n())
write.csv(t1,"results/nefc_res.csv")
