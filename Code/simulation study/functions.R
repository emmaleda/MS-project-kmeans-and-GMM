library(tidyverse)
library(mclust)

## Create functions for large simulation

# simulate_clusters simulates one data set and returns two variables and true grouping
simulate_clusters <- function(mean1, sigma1, n1, 
                              mean2, sigma2, n2){
  sim1 <- rmvnorm(n = n1, mean = mean1, sigma = sigma1)
  sim2 <- rmvnorm(n = n2, mean = mean2, sigma = sigma2)
  
  one_cluster <- rbind(sim1,sim2)
  cluster <- data.frame(x = one_cluster[,1], 
                        y = one_cluster[,2],
                        group = c(rep("a", n1), rep("b", n2)))
  return(cluster)
}

# fifty_cluters uses simulate_clusters to generate 50 data sets
fifty_clusters <- function(mean1, sigma1, n1, mean2, sigma2, n2){
  clusters <- purrr::map(1:50, ~simulate_clusters(mean1 = mean1, sigma1 = sigma1, n1 = n1, 
                                                  mean2 = mean2, sigma2 = sigma2, n2 = n2))
  clusters
}

## GMM_fit_bivariate takes one data set and returns the parameters, confusion matrix,
## and model name in a tibble format
GMM_fit_bivariate <- function(data){
  data_nogrp <- data %>%
    select(-group)
  mod <- Mclust(data_nogrp, G = 2)
  
  confusion_matrix <- table(mod$classification, data$group)
  model_name <- mod$parameters$variance$modelName[1]  
  
  summary_info <- tibble(
    x1_est  = mod$parameters$mean[1,1],
    y1_est  = mod$parameters$mean[2,1],
    x2_est  = mod$parameters$mean[1,2],
    y2_est  = mod$parameters$mean[2,2],
    var_x1  = mod$parameters$variance$sigma[,,1][1,1],
    cov_xy1 = mod$parameters$variance$sigma[,,1][2,1],
    var_y1  = mod$parameters$variance$sigma[,,1][2,2],
    var_x2  = mod$parameters$variance$sigma[,,2][1,1],
    cov_xy2 = mod$parameters$variance$sigma[,,2][2,1],
    var_y2  = mod$parameters$variance$sigma[,,2][2,2],
    confusion_matrix = list(confusion_matrix), 
    model_name = model_name,
    cluster_type = "GMM"
  )
  
  # depending on the parameters, sometimes mclust isn't consistant with which
  # cluster it returns first, so the if statement ensures that the cluster will
  # be uniform across all 50 simulations
  if(confusion_matrix[1,1] < 125){
    summary_info <- tibble(
      x1_est = summary_info$x2_est,
      y1_est = summary_info$y2_est,
      x2_est = summary_info$x1_est,
      y2_est = summary_info$y1_est,
      var_x1  = summary_info$var_x2,
      cov_xy1 = summary_info$cov_xy2,
      var_y1  = summary_info$var_y2,
      var_x2  = summary_info$var_x1,
      cov_xy2 = summary_info$cov_xy1,
      var_y2  = summary_info$var_y1,
      confusion_matrix = list(apply(confusion_matrix, 2, rev)),
      model_name = model_name,
      cluster_type = "kmeans"
    )
  }
  
  return(summary_info) 
}

## kmeans_fit_bivariate takes one data set and returns the parameters and confustion matrix in a tibble format
kmeans_fit_bivariate <- function(data){
  data_nogrp <- data %>% select(-group)
  
  fit <- kmeans(data_nogrp, centers = 2)
  
  confusion_matrix <- table(fit$cluster, data$group)
  
  summary_info <- tibble(
    x1_est = fit$centers[1,1],
    y1_est = fit$centers[1,2],
    x2_est = fit$centers[2,1],
    y2_est = fit$centers[2,2],
    confusion_matrix = list(table(fit$cluster, data$group)),
    cluster_type = "kmeans"
  )
  
  # depending on the parameters, sometimes mclust isn't consistant with which
  # cluster it returns first, so the if statement ensures that the cluster will
  # be uniform across all 50 simulations
  if(confusion_matrix[1,1] < 125){
    summary_info <- tibble(
      x1_est = summary_info$x2_est,
      y1_est = summary_info$y2_est,
      x2_est = summary_info$x1_est,
      y2_est = summary_info$y1_est,
      confusion_matrix = list(apply(confusion_matrix, 2, rev)),
      cluster_type = "kmeans"
    )
  }
  return(summary_info)
}

## GMM_fit_fifty takes GMM_fit_bivariate and allows it to take multiple data sets
GMM_fit_fifty <- function(data){
  GMM_data_est <- purrr::map_dfr(data, GMM_fit_bivariate) 
  return(GMM_data_est)
}

## kmeans_fit_fifty takes kmeans_fit_bivariate and allows it to take multiple data sets
kmeans_fit_fifty <- function(data){
  kmeans_data_est <- purrr::map_dfr(data, kmeans_fit_bivariate) 
  return(kmeans_data_est)
}

## method_comparison combines all the earlier functions; provide to it the parameters and 
## it will return a tibble with 50 sets of parameters for each method as well as the original data
method_comparison <- function(mean1, sigma1, n1, 
                              mean2, sigma2, n2){
  comp_data <- fifty_clusters(mean1 = mean1, sigma1 = sigma1, n1 = n1, 
                              mean2 = mean2, sigma2 = sigma2, n2 = n2)
  GMM_results    <- GMM_fit_fifty(comp_data)
  kmeans_results <- kmeans_fit_fifty(comp_data)
  return(tibble(GMM_results    = GMM_results,
                kmeans_results = kmeans_results,
                comp_data = comp_data))
}

## avg_conf_mtx takes data created by the method_comparison function and calculates the average of
## all 50 confusion matrices for both k-means and GMM; it also find the misclassification percentage
avg_conf_mtx <- function(data){
  GMM_cols <-    t(matrix(unlist(data$GMM_results$confusion_matrix), nrow  = 4))
  kmeans_cols <- t(matrix(unlist(data$kmeans_results$confusion_matrix), nrow  = 4))
  
  GMM_avg    <- matrix(colMeans(GMM_cols), nrow = 2)
  kmeans_avg <- matrix(colMeans(kmeans_cols), nrow = 2)
  
  GMM_prop <- (GMM_avg[1,2] + GMM_avg[2,1])/sum(GMM_avg)
  kmeans_prop <- (kmeans_avg[1,2] + kmeans_avg[2,1])/sum(kmeans_avg)
  
  avgs <- tibble(
    GMM_CF_avg = GMM_avg,
    GMM_prop = GMM_prop,
    kmeans_CF_avg = kmeans_avg, 
    kmeans_prop = kmeans_prop
  )
  return(avgs)
}