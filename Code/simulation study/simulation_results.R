library(tidyverse)
library(mclust)
library(mvtnorm)


## Scenario 1: #########################################################################################
set.seed(300)

# create parameters
mean1 = c(0,0)
sigma1 = cbind(c(1,0),c(0,1))
n1 = 250

mean2 = c(5,5)
sigma2 = cbind(c(1.5,0),c(0,1.5))
n2 = 250

# use function method_comparison
data_both <- method_comparison(mean1 = mean1, sigma1 = sigma1, n1 = n1, 
                               mean2 = mean2, sigma2 = sigma2, n2 = n2)

# Combine two data sets:
GMM_data_cut <- data_both$GMM_results %>%
  select(c(x1_est, y1_est, x2_est, y2_est, cluster_type))
data_est <- rbind(data_both$kmeans_results%>%select(-confusion_matrix), GMM_data_cut)
data_est$cluster_type <- factor(data_est$cluster_type, levels = c("kmeans", "GMM"))

# compare parameter estimates of k-means and GMM
ggplot(data_est)+
  geom_histogram(aes(x = x1_est))+
  geom_vline(xintercept = 0)+
  facet_wrap(~cluster_type)+
  xlab("Parameter Mean 1 Estimate")

# look at avg confusion matrix and the misclassification proportion
data_both_CF <- avg_conf_mtx(data_both)
data_both_CF$GMM_CF_avg
data_both_CF$GMM_prop[1]
data_both_CF$kmeans_CF_avg
data_both_CF$kmeans_prop[1]

# look at models selected by mclust
table(data_both$GMM_results$model_name)

## Scenario 2: #########################################################################################

set.seed(300)

# create parameters
mean5 = c(0,0)
sigma5 = cbind(c(2,0),c(0,0.1))
n5 = 250

mean6 = c(1,1)
sigma6 = cbind(c(1,0),c(0,3))
n6 = 250

# use function method_comparison
dif_GMM_bet <- method_comparison(mean1 = mean5, sigma1 = sigma5, n1 = n5, 
                                 mean2 = mean6, sigma2 = sigma6, n2 = n6)

# Combine two data sets:
GMM_data_cut2 <- dif_GMM_bet$GMM_results %>%
  select(c(x1_est, y1_est, x2_est, y2_est, cluster_type))
data_est_GMM <- rbind(GMM_data_cut2,
                      dif_GMM_bet$kmeans_results%>%select(-confusion_matrix))
data_est_GMM$cluster_type <- factor(data_est_GMM$cluster_type, levels = c("kmeans", "GMM"))

# compare parameter estimates of k-means and GMM
ggplot(data_est_GMM)+
  geom_histogram(aes(x = x1_est))+
  geom_vline(xintercept = 0)+
  facet_wrap(~cluster_type)+
  xlab("Parameter Mean 1 Estimate")

# look at avg confusion matrix and the misclassification proportion
data_GMM_bet_CF <- avg_conf_mtx(dif_GMM_bet)
data_GMM_bet_CF$GMM_CF_avg
data_GMM_bet_CF$GMM_prop[1]
data_GMM_bet_CF$kmeans_CF_avg
data_GMM_bet_CF$kmeans_prop[1]

# look at models selected by mclust
table(dif_GMM_bet$GMM_results$model_name)
