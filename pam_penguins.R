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
penguins$sex <- replace(penguins$sex, penguins$sex == ".", NA)
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

# 1: Culmen Depth vs Culmen Length
plot(clean_penguins$culmen_depth_mm, clean_penguins$culmen_length_mm,
    col =(pam.out$cluster+1), 
    main="Culmen Depth vs Culmen Length", 
    xlab="Culmen Depth (mm)", 
    ylab="Culmen Length (mm)", 
    pch=19)
 
points(pam.out$medoids[, c("culmen_depth_mm", "culmen_length_mm")], 
        pch=as.character(pam.out$cluster[pam.out$id.med]), 
        col="black")

# 2: Flipper Length vs Culmen Length
plot(clean_penguins$flipper_length_mm, clean_penguins$culmen_length_mm,
     col =(pam.out$cluster+1), 
     main="Flipper Length vs Culmen Length", 
     xlab="Flipper Length (mm)", 
     ylab="Culmen Length (mm)", 
     pch=19)

points(pam.out$medoids[, c("flipper_length_mm", "culmen_length_mm")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 3: Body Mass vs Culmen Length
plot(clean_penguins$body_mass_g, clean_penguins$culmen_length_mm,
     col =(pam.out$cluster+1), 
     main="Body Mass vs Culmen Length", 
     xlab="Body Mass (g)", 
     ylab="Culmen Length (mm)", 
     pch=19)

points(pam.out$medoids[, c("body_mass_g", "culmen_length_mm")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 4: Culmen Length vs Culmen Depth
plot(clean_penguins$culmen_length_mm, clean_penguins$culmen_depth_mm,
     col =(pam.out$cluster+1), 
     main="Culmen Length vs Culmen Depth", 
     xlab="Culmen Length", 
     ylab="Culmen Depth (mm)", 
     pch=19)

points(pam.out$medoids[, c("culmen_length_mm", "culmen_depth_mm")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 5: Flipper Length vs Culmen Depth
plot(clean_penguins$flipper_length_mm, clean_penguins$culmen_depth_mm,
     col =(pam.out$cluster+1), 
     main="Flipper Length vs Culmen Depth", 
     xlab="Flipper Length", 
     ylab="Culmen Depth (mm)", 
     pch=19)

points(pam.out$medoids[, c("flipper_length_mm", "culmen_depth_mm")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 6: Body Mass vs Culmen Depth
plot(clean_penguins$body_mass_g, clean_penguins$culmen_depth_mm,
     col =(pam.out$cluster+1), 
     main="Body Mass vs Culmen Depth", 
     xlab="Body Mass (g)", 
     ylab="Culmen Depth (mm)", 
     pch=19)

points(pam.out$medoids[, c("body_mass_g", "culmen_depth_mm")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 7: Culmen Length vs Flipper Length
plot(clean_penguins$culmen_length_mm, clean_penguins$flipper_length_mm,
     col =(pam.out$cluster+1), 
     main="Culmen Length vs Flipper Length", 
     xlab="Culmen Length (mm)", 
     ylab="Flipper Length (mm)", 
     pch=19)

points(pam.out$medoids[, c("culmen_length_mm", "flipper_length_mm")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 8: Culmen Depth vs Flipper Length
plot(clean_penguins$culmen_depth_mm, clean_penguins$flipper_length_mm,
     col =(pam.out$cluster+1), 
     main="Culmen Depth vs Flipper Length", 
     xlab="Culmen Depth (mm)", 
     ylab="Flipper Length (mm)", 
     pch=19)

points(pam.out$medoids[, c("culmen_depth_mm", "flipper_length_mm")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 9: Body Mass vs Flipper Length
plot(clean_penguins$body_mass_g, clean_penguins$flipper_length_mm,
     col =(pam.out$cluster+1), 
     main="Body Mass vs Flipper Length", 
     xlab="Body Mass (g)", 
     ylab="Flipper Length (mm)", 
     pch=19)

points(pam.out$medoids[, c("body_mass_g", "flipper_length_mm")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 10: Culmen Length vs Body Mass
plot(clean_penguins$culmen_length_mm, clean_penguins$body_mass_g,
     col =(pam.out$cluster+1), 
     main="Culmen Length vs Body Mass", 
     xlab="Culmen Length (mm)", 
     ylab="Body Mass (g)", 
     pch=19)

points(pam.out$medoids[, c("culmen_length_mm", "body_mass_g")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 11: Culmen Depth vs Body Mass
plot(clean_penguins$culmen_depth_mm, clean_penguins$body_mass_g,
     col =(pam.out$cluster+1), 
     main="Culmen Depth vs Body Mass", 
     xlab="Culmen Depth (mm)", 
     ylab="Body Mass (g)", 
     pch=19)

points(pam.out$medoids[, c("culmen_depth_mm", "body_mass_g")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")

# 12: Flipper Length vs Body Mass
plot(clean_penguins$flipper_length_mm, clean_penguins$body_mass_g,
     col =(pam.out$cluster+1), 
     main="Flipper Length vs Body Mass", 
     xlab="Flipper Length (mm)", 
     ylab="Body Mass (g)", 
     pch=19)

points(pam.out$medoids[, c("flipper_length_mm", "body_mass_g")], 
       pch=as.character(pam.out$cluster[pam.out$id.med]), 
       col="black")
