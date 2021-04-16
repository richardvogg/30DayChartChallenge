library(tidymodels)
library(ggplot2)
library(dplyr)
library(rpart.plot)

df <- read.csv("C:/Richard/R and Python/Datasets/diabetes.csv") %>%
  mutate(Outcome = factor(ifelse(Outcome == 1, "diabetes", "no diabetes")))

ctrl <- list(cp = 0.01, minbucket = 10, maxdepth = 4)

tree <- rpart(factor(Outcome)~., data=df, method = "class",
              control = ctrl)


rpart.plot(tree,
           box.palette = c("orangered2","grey","cyan2"),
           branch.col = c("goldenrod"),
           branch.lwd = 3,
           tweak = 1.2,
           varlen = 15,
           branch = 0.1,
           clip.right.labs = FALSE,
           type = 3,
           cex.main = 1.5,
           leaf.round = 9,
           fallen.leaves = TRUE,
           main = "Factors for diabetes (dataset with 768 females with Pima Indian heritage from Kaggle)")


# larger tree

ctrl <- list(cp = 0.01, minbucket = 10, maxdepth = 5)

tree <- rpart(factor(Outcome)~., data=df, method = "class",
              control = ctrl)


rpart.plot(tree,
           box.palette = c("orangered2","grey","cyan2"),
           branch.col = c("goldenrod"),
           branch.lwd = 3,
           tweak = 1.2,
           varlen = 15,
           branch = 0.1,
           clip.right.labs = FALSE,
           type = 3,
           cex.main = 1.5,
           leaf.round = 9,
           fallen.leaves = TRUE,
           main = "Factors for diabetes (dataset with 768 females with Pima Indian heritage from Kaggle)")
