# MS project: *k*-means and Gaussian Mixture Models

Emma Grossman

## Clustering

The *k*-means algorithm and Gaussian Mixture Models estimated with the Expectation Maximization algorithm are both clustering techniques used to identify relationships between variables.This paper documents the theory behind the two methods, then explores the commonly used functions in R that implement them. Examples of *k*-means and Gaussian Mixture Models are explored in a case study using the `penguins` data set from the `palmerpenguins` package. Then, data were simulated to highlight scenarios in which both methods performed well, as well as scenarios where only one performed well. Lastly, a discussion on future work on this topic concludes this paper.

These two clustering methods are notable because the *k*-means algorithm is more widely known and commonly used, but Gaussian Mixture Models tends to perform better on data structures with groupins that are less distinct or have different variances.

## Organization

Required packages are `tidyverse`, `stats`, `palmerpenguins`, `GGally`, `mclust`, and `mvtnorm`.

The `palmerpenguins` case study can be found in [Code/case study: palmerpenguins/penguins.R](Code/case study: palmerpenguins/penguins.R). The code to view the functions that compare *k*-means and GMM for the simulation study is in [Code/simulation study/functions.R](Code/simulation study/functions.R) and this file must be run first. To run the simulation, head to [Code/simulation study/simulation_results.R](Code/simulation study/simulation_results.R). To create the graph that demonstrates how the EM algorithm creates soft labels, go to [Code/step by step GMM/graphcode.R](Code/step by step GMM/graphcode.R).

The final results and graphs can be viewed in the presentation and report.

## Presentation & Report

------
