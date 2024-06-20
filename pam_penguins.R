# Importing required libraries
library(cluster)
library(factoextra)

# Importing data
penguins <- read.csv('penguins.csv')
head(penguins)

# Sex as factor
penguins$sex <- factor(penguins$sex)
str(penguins)

# Data cleaning
boxplot(penguins)
penguins <- na.omit(penguins)
clean_penguins <- penguins[penguins$flipper_length_mm < 4000 & penguins$flipper_length_mm > 0, ]
boxplot(clean_penguins)

# Dataframe without factor columns
clean_penguins_numeric <- clean_penguins[, sapply(clean_penguins, is.numeric)]

# Checking the optimal number of clusters with wss
fviz_nbclust(clean_penguins_numeric, pam, method = "wss")

# Checking the optimal number of clusters with silhouette
fviz_nbclust(clean_penguins_numeric, pam, method = "silhouette")

# PAM algorithm
pam.out<-pam(clean_penguins, 2, metric="euclidean", stand=TRUE)
pam.out
plot(clean_penguins, col =(pam.out$cluster+1), main="PAM (K=2)", pch=19)
points(pam.out$medoids, pch=as.character(pam.out$cluster[pam.out$id.med]))

# TODO Significant plots to show

# plot(clean_penguins$flipper_length_mm, clean_penguins$bill_depth_mm, 
#      col =(pam.out$cluster+1), 
#      main="PAM (K=2) - Flipper Length vs Culmen Depth", 
#      xlab="Flipper Length (mm)", 
#      ylab="Culmen Depth (mm)", 
#      pch=19)
# 
# points(pam.out$medoids[, c("flipper_length_mm", "culmen_depth_mm")], 
#        pch=as.character(pam.out$cluster[pam.out$id.med]), 
#        col="red")
