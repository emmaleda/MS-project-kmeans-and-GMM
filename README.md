# MS project: *k*-means and Gaussian Mixture Models

Emma Grossman

## Clustering

The *k*-means algorithm and Gaussian Mixture Models estimated with the Expectation Maximization algorithm are both clustering techniques used to identify relationships between variables. This repository for my MS project documents the theory behind the two methods, then explores the commonly used functions in R that implement them. Examples of *k*-means and Gaussian Mixture Models are explored in a case study using the `penguins` data set from the `palmerpenguins` package. Then, data were simulated to highlight scenarios in which both methods performed well, as well as scenarios where only one performed well. 

These two clustering methods are notable because the *k*-means algorithm is more widely known and commonly used, but Gaussian Mixture Models tends to perform better on data structures with groupings that are less distinct or have different variances.

## Organization

Required packages are `tidyverse`, `stats`, `palmerpenguins`, `GGally`, `mclust`, and `mvtnorm`.

The `palmerpenguins` case study can be found in [Code/case study: palmerpenguins/penguins.R](https://github.com/emmaleda/MS-project-kmeans-and-GMM/blob/b999c095a2afb011dba9b06a92c1d64c0d31c414/Code/case%20study:%20palmerpenguins/penguins.R). The code to view the functions that compare *k*-means and GMM for the simulation study is in [Code/simulation study/functions.R](https://github.com/emmaleda/MS-project-kmeans-and-GMM/blob/c98bf081d97f34f4a1fedfd81a40af5bbe2d0ad3/Code/simulation%20study/functions.R) and this file must be run before the simulation_results.R file. To run the simulation, head to [Code/simulation study/simulation_results.R](https://github.com/emmaleda/MS-project-kmeans-and-GMM/blob/b999c095a2afb011dba9b06a92c1d64c0d31c414/Code/simulation%20study/simulation_results.R). To create the graph that demonstrates how the EM algorithm creates soft labels, go to [Code/step by step GMM/graphcode.R](https://github.com/emmaleda/MS-project-kmeans-and-GMM/blob/b999c095a2afb011dba9b06a92c1d64c0d31c414/Code/step%20by%20step%20GMM/graphcode.R).

The final results and graphs can be viewed in the presentation and report.

## Presentation & Report

------
