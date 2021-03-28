library(tidyverse)
library(latex2exp)

## this is GMM by hand

# parameters & seed
set.seed(15753)
x1 <- rnorm(25)
y1 <- rnorm(25, 1, 1)

x2 <- rnorm(25, 3, 1)
y2 <- rnorm(25, 3, 2)

bivariate_gauss <- data.frame(x = c(x1, x2), y = c(y1, y2),
                              group = c(rep("a", 25), rep("b", 25)))

# view true groupings:
ggplot()+
  geom_point(aes(x = bivariate_gauss$x, y = bivariate_gauss$y, color = bivariate_gauss$group))+
  scale_color_manual(values = c("#3FACF5", "#0F2A44"), name = "group") +
  ggtitle("True Cluster Grouping")+
  xlab("x")+
  ylab("y")+
  geom_point(data = bivariate_gauss[bivariate_gauss$group == "a",],  
             aes(x = mean(x), y = mean(y)), color = "red")+
  geom_point(data = bivariate_gauss[bivariate_gauss$group == "b",],  
             aes(x = mean(x), y = mean(y)), color = "red")

# apply k-means to the data
bifit <- kmeans(x = bivariate_gauss %>% select(-group),
                centers = 2)

# add column for graphing purposes later
data <- bivariate_gauss %>%
  mutate(cluster = ifelse(bifit$cluster == 2, 0.0001, 0.9999))

# summarize the data and create parameter names
(summary_data <- data %>%
    group_by(cluster) %>%
    summarize(mu_x = mean(x), variance_x = var(x), std_x = sd(x), 
              mu_y = mean(y), variance_y = var(y), std_y = sd(y), 
              cov_xy = cov(x,y), size = n()) %>%
    mutate(pi = size/sum(size)))

# create starting pdfs with the summarized data
pdf1 <- dmvnorm(x = data %>% select(x,y), mean = c(summary_data$mu_x[1], summary_data$mu_y[1]), 
                sigma = cbind(c(summary_data$variance_x[1], summary_data$cov_xy[1]),
                              c(summary_data$cov_xy[1], summary_data$variance_y[1])))* summary_data$pi[1]

pdf2 <- dmvnorm(x = data %>% select(x,y), mean = c(summary_data$mu_x[2], summary_data$mu_y[2]), 
                sigma = cbind(c(summary_data$variance_x[2], summary_data$cov_xy[2]),
                              c(summary_data$cov_xy[2], summary_data$variance_y[2])))* summary_data$pi[2]

# create the posterior pdfs
normalizer <- pdf1 + pdf2

post1 <- pdf1 / normalizer
post2 <- pdf2 / normalizer

# find the new parameters, given the new posteriors 
clust1_n <- sum(post1)
clust2_n <- sum(post2)

mu_x1 <- 1/clust1_n * sum(post1 * data$x)
mu_x2 <- 1/clust2_n * sum(post2 * data$x)
mu_y1 <- 1/clust1_n * sum(post1 * data$y)
mu_y2 <- 1/clust2_n * sum(post2 * data$y)

var_x1 <- sum(post1 * (data$x - mu_x1)^2) * 1/clust1_n
var_x2 <- sum(post2 * (data$x - mu_x2)^2) * 1/clust2_n
var_y1 <- sum(post1 * (data$y - mu_y1)^2) * 1/clust1_n
var_y2 <- sum(post2 * (data$y - mu_y2)^2) * 1/clust2_n

cov_xy1 <- sum(post1 * (data$x - mu_x1)*(data$y - mu_y1)) * 1/clust1_n
cov_xy2 <- sum(post2 * (data$x - mu_x2)*(data$y - mu_y2)) * 1/clust2_n


pi1 <- clust1_n/ length(data$x)
pi2 <- clust2_n/ length(data$x)

# sum the pdfs and find the log likelihood
pdf_sums <- pdf1 + pdf2
pdf_sums_ln <- log(pdf_sums)

# put previous steps into functions:
e_step <- function(data, mean_matrix, var_matrix, pi_vec){
  pdf1 <- dmvnorm(x = data, mean  = mean_matrix[,1], sigma = var_matrix[1:2,1:2])*pi_vec[1]
  pdf2 <- dmvnorm(x = data, mean  = mean_matrix[,2], sigma = var_matrix[3:4,1:2])*pi_vec[2]
  
  
  sum_pdfs <- pdf1 + pdf2
  
  post1 <- pdf1 / sum_pdfs
  post2 <- pdf2 / sum_pdfs
  
  pdf_sums_ln <- log(sum_pdfs)
  ln_sum <- sum(pdf_sums_ln)
  
  list("loglik" = ln_sum,
       "posterior_df" = cbind(post1,post2))
}


m_step <- function(data, posterior_df){
  clust1_n <- sum(posterior_df[, 1])
  clust2_n <- sum(posterior_df[, 2])
  
  mu_x1 <- 1/clust1_n * sum(posterior_df[, 1] * data$x)
  mu_x2 <- 1/clust2_n * sum(posterior_df[, 2] * data$x)
  mu_y1 <- 1/clust1_n * sum(posterior_df[, 1] * data$y)
  mu_y2 <- 1/clust2_n * sum(posterior_df[, 2] * data$y)
  
  var_x1 <- sum(posterior_df[, 1] * (data$x - mu_x1)^2) * 1/clust1_n
  var_x2 <- sum(posterior_df[, 2] * (data$x - mu_x2)^2) * 1/clust2_n
  var_y1 <- sum(posterior_df[, 1] * (data$y - mu_y1)^2) * 1/clust1_n
  var_y2 <- sum(posterior_df[, 2] * (data$y - mu_y2)^2) * 1/clust2_n
  
  cov_xy1 <- sum(posterior_df[, 1] * (data$x - mu_x1)*(data$y - mu_y1)) * 1/clust1_n
  cov_xy2 <- sum(posterior_df[, 2] * (data$x - mu_x2)*(data$y - mu_y2)) * 1/clust2_n
  
  pi1 <- clust1_n/ length(data$x)
  pi2 <- clust2_n/ length(data$x)
  
  list("mu"     = cbind(c(mu_x1, mu_y1),c(mu_x2, mu_y2)),
       "cov1"   = cbind(c(var_x1, cov_xy1),c(cov_xy1, var_y1)),
       "cov2"   = cbind(c(var_x2, cov_xy2),c(cov_xy2, var_y2)),
       "pi"     = c(pi1, pi2))
}

# find the resulting clusters with EM
for (i in 1:100) {
  if (i == 1) {
    # Initialization
    e.step <- e_step(data = data %>% select(x,y),
                     mean_matrix = rbind(summary_data[["mu_x"]], summary_data[["mu_y"]]),
                     var_matrix = rbind(c(summary_data[["variance_x"]][1], summary_data[["cov_xy"]][1]),
                                        c(summary_data[["cov_xy"]][1], summary_data[["variance_y"]][1]),
                                        c(summary_data[["variance_x"]][2], summary_data[["cov_xy"]][2]),
                                        c(summary_data[["cov_xy"]][2], summary_data[["variance_y"]][2])),
                     pi_vec = summary_data[["pi"]])
    m.step <- m_step(data = data %>% select(x,y), e.step[["posterior_df"]])
    cur.loglik <- e.step[["loglik"]]
    loglik.vector <- e.step[["loglik"]]
  } else {
    # Repeat E and M steps till convergence
    e.step <- e_step(data = data %>% select(x,y),
                     mean_matrix = m.step[["mu"]],
                     var_matrix = rbind(m.step[["cov1"]], m.step[["cov2"]]),
                     pi_vec = m.step[["pi"]])
    m.step <- m_step(data = data %>% select(x,y), e.step[["posterior_df"]])
    loglik.vector <- c(loglik.vector, e.step[["loglik"]])
    
    loglik.diff <- abs((cur.loglik - e.step[["loglik"]]))
    if(loglik.diff < 1e-6) {
      break
    } else {
      cur.loglik <- e.step[["loglik"]]
    }
    if (i == 4){
      pdf_i4 <- e.step$posterior_df[,1]
      m.step_i4 <- m.step
    }
    if (i == 12){
      pdf_i12 <- e.step$posterior_df[,1]
      m.step_i12 <- m.step
    }
  }
}

# collect the necessary data into one data frame
graph_data <- data.frame(x     = bivariate_gauss$x,
                         y     = bivariate_gauss$y,
                         group = bivariate_gauss$group, 
                         kmeans = data$cluster,
                         it_4  = pdf_i4,
                         it_12 = pdf_i12,
                         it_f  = e.step$posterior_df[,1])

# more data adjusting to create graph
graph_data_long <- pivot_longer(graph_data, cols = c(kmeans, it_4, it_12, it_f), 
                                names_to = "which_step", values_to = "prob")
graph_data_long <- graph_data_long %>%
  mutate(which_step = factor(which_step, levels = c("kmeans", "it_4", "it_12","it_f")))

graph_names <- c(
  `kmeans` = "k-means",
  `it_4` = "GMM Step 4",
  `it_12` = "GMM Step 12",
  `it_f` = "GMM last Step"
)

# finally, graph that appears in the report:
ggplot(graph_data_long)+
  geom_point(aes(x = x, y = y, color = prob))+
  scale_color_gradient2(name = TeX('$P(x_i \\in k_1)$'), 
                        low = "red", mid = "yellow", high = "blue",
                        midpoint = 0.5)+
  xlab("x")+
  ylab("y")+
  facet_wrap(~which_step, labeller = as_labeller(graph_names))

# ggsave("three_steps.png", height = 4)


