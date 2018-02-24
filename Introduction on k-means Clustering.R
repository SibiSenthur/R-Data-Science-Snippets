## Cluster Analysis
#1. Rows are the observations; Columns are Variables;
#2. Any Missing Value in the Data must be removed or estimated;
#3. The data must be standardized to make variables comparable; Standardization consists of transforming the variables such that they contain zero mean and Standard Deviation of one.

##I am using the built-in R dataset "USArrests", which contains statistics in arrests per 100,000 residents for assualt, murder, and rape in each of the 50 US States.

## 1. Data Preparation
data("USArrests")
View(USArrests)
df<-USArrests

#1. To remove any missing values, that may be present in the data;
df<-na.omit(df)

#2. As we don't want the clustering algorithm to depend to an arbitrary variable unit, we start by scaling/standardizing the data using the R function scale():
df<-scale(df)
head(df, n=3)

## Required 'R' Packages for Clustering Algorithms;
  # Cluster for computing Cluster Algorithms;
  # FactorExtra for ggplot2 based Visualizations;

## 2. Compte K-Means;
set.seed(123)
km.res<-kmeans(df,4,nstart=25)

## 3. Visualization;
install.packages("factoextra")
library(factoextra)

fviz_cluster(km.res, data = df,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)

