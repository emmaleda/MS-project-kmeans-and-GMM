library(palmerpenguins)
library(tidyverse)
library(GGally)
library(mclust)
library(plyr)
library(dplyr)

data(penguins)

subset_penguins_Sp <- penguins %>%
  select(c(-island, -sex, -year)) # remove categorical variables

subset_penguins <- subset_penguins_Sp %>%
  select(c(-species)) # create a subset that doesn't include the species, which we are trying to classify

subset_penguins_Sp <- na.omit(subset_penguins_Sp) # remove NA values
subset_penguins <- na.omit(subset_penguins) # remove NA values

par(las = 1)  # scatterplot matrix by species
my_cols <- c("#8BBDE0", "#E2A4E0", "#B6E08B")
pairs(subset_penguins_Sp[,2:5], col = my_cols[subset_penguins_Sp$species], 
      pch = 19, upper.panel = NULL, 
      labels = c("bill length", "bill depth", "flipper length", "body mass"))

penguins_z <- as.data.frame(lapply(subset_penguins, scale))  # standardize the data, which k-means requires

penguins_sub_Sp_na <- na.omit(subset_penguins_Sp) # remove NA values
penguins_z_na <- na.omit(penguins_z) # remove NA values


## k-means
set.seed(11206)
penguin_clusters <- kmeans(penguins_z_na, 3, nstart  = 10)
table1 <- table(penguins_sub_Sp_na$species, penguin_clusters$cluster)
table1

## GMM
mod <- Mclust(subset_penguins, G = 3)
table2 <- table(subset_penguins_Sp$species, mod$classification)
table2

## create comparison graph from presentation
penguin_graph <- data.frame(bill_length = subset_penguins$bill_length_mm,
                            bill_depth  = subset_penguins$bill_depth_mm,
                            species     = subset_penguins_Sp$species,
                            kmeans      = as.factor(penguin_clusters$cluster),
                            GMM         = as.factor(mod$classification))


penguin_graph$kmeans <- revalue(penguin_graph$kmeans, 
                                c("1"="Gentoo","2"="Adelie", "3"="Chinstrap"))
penguin_graph$GMM <- revalue(penguin_graph$GMM, 
                             c("1"="Adelie","2"="Gentoo", "3"="Chinstrap"))
penguin_graph_long <- pivot_longer(penguin_graph, cols = c(species, kmeans, GMM), 
                                   names_to = "type", values_to = "Cluster")
penguin_graph_long$type <- factor(penguin_graph_long$type, 
                                  levels = c("species","kmeans", "GMM"))

ggplot(penguin_graph_long)+
  geom_point(aes(x = bill_length, y = bill_depth, color = Cluster) )+
  facet_wrap(~type)+
  scale_color_manual(values = c("#8BBDE0", "#E2A4E0", "#B6E08B"))+
  ggtitle("True Clusters (species) and Generated Clusters from the Algorithms")+
  xlab("Bill Length (mm)")+
  ylab("Bill Depth (mm)")
