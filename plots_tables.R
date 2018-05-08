library(tidyverse)

load("results/summary_accuracies.RData")

p1 <- ggplot(data = allres, aes(x = Accuracy)) + geom_histogram(bins = 20) +
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

allres4_long <- allres4 %>% as.tibble() %>%
  select(Accuracy, Kappa, AccuracyNull, Method, Species) %>% 
  gather(Accuracy, Kappa, key = "Measure", value = "Value") %>%
  mutate(AccuracyNull = ifelse(Measure == "Kappa", 0, AccuracyNull))

p3 <- ggplot(data = allres4_long, aes(x = Value)) + geom_histogram(bins = 20) +
  geom_vline(aes(xintercept = AccuracyNull), color = "red") +
  facet_grid(Method ~ Measure, scales = "free") + 
  theme_bw(base_size=24) + ylab("Count") +
  theme(axis.text = element_text(size = 20))
ggsave("results/nefc_4class.png", p3, width=12, height=7, dpi=200)

t1 <- allres %>% group_by(Species,Method) %>% summarize(meanAcc = mean(Accuracy),
                                                  nullAcc = mean(AccuracyNull),
                                                  meanKappa = mean(Kappa),
                                                  meanS = mean(Sensitivity),
                                                  meanW = mean(Specificity),
                                                  prSig = sum(Accuracy > AccuracyNull)/n())
write.csv2(t1,"results/nefc_res_2class.csv")

t2 <- allres4 %>% group_by(Method) %>% summarize(meanAcc = mean(Accuracy),
                                                 nullAcc = mean(AccuracyNull),
                                                 meanKappa = mean(Kappa),
                                                 sensKZN = mean(sensKZN),
                                                 sensNAM = mean(sensNAM),
                                                 sensS = mean(sensS),
                                                 sensW = mean(sensW),
                                                 specKZN = mean(specKZN),
                                                 specNAM = mean(specNAM),
                                                 specS = mean(specS),
                                                 specW = mean(specW),
                                                 prSig = sum(Accuracy > AccuracyNull)/n())
write.csv2(t2,"results/nefc_res_4class.csv")
