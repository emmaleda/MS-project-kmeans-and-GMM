library(palmerpenguins)
library(tidyverse)
library(GGally)
library(mclust)

data(penguins)

subset_penguins_Sp <- penguins %>%
  select(c(-island, -sex, -year)) # remove categorical variables

subset_penguins <- subset_penguins_Sp %>%
  select(c(-species)) # create a subset that doesn't include the species, which we are trying to classify

subset_penguins_Sp <- na.omit(subset_penguins_Sp) # remove NA values
subset_penguins <- na.omit(subset_penguins) # remove NA values

ggpairs(subset_penguins_Sp, ggplot2::aes(color = species)) # scatterplot matrix by species

penguins_z <- as.data.frame(lapply(subset_penguins, scale))  # standardize the data, which k-means requires

penguins_sub_Sp_na <- na.omit(subset_penguins_Sp) # remove NA values
penguins_z_na <- na.omit(penguins_z) # remove NA values

## k-means
set.seed(11206)
penguin_clusters <- kmeans(penguins_z_na, 3, nstart  = 10)
table1 <- table(penguins_sub_Sp_na$species, penguin_clusters$cluster)
table1

mod <- Mclust(subset_penguins, G = 3)
table2 <- table(subset_penguins_Sp$species, mod$classification)
table2
