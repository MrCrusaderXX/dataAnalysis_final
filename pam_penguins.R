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



#Clustering con DBSCAN
# Carico i pacchetti necessari
library(ggplot2)
library(dplyr)
library(dbscan)

#Seleziono le variabili numeriche
penguins_num <- penguins %>% select(culmen_length_mm, culmen_depth_mm, flipper_length_mm, body_mass_g)

# Standardizzo i dati
penguins_scaled <- scale(penguins_num)

# Determino il valore di eps usando il diagramma k-dist
kNNdistplot(penguins_scaled, k = 4)
abline(h=0.8, col = "red", lty=8)

# Applico DBSCAN con eps e minPts scelti
dbscan_result <- dbscan(penguins_scaled, eps = 0.8, minPts = 5)
dbscan_result

# abbiamo 2 cluster ed un terzo con 4 punti rumorosi
# questi punti non appartengono a nessun cluster denso e possono essere considerati anomalie

# Aggiungo i cluster al dataset originale
penguins$cluster <- factor(dbscan_result$cluster)

# Posso ottenere il numero di punti per ogni cluster anche in questo modo
table(penguins$cluster)

# Analizzo le caratteristiche dei cluster
# facendo una tabella con le medie delle variabili per ciascun cluster
penguins %>% group_by(cluster) %>% summarise(
  culmen_length_mm = mean(culmen_length_mm, na.rm = TRUE),
  culmen_depth_mm = mean(culmen_depth_mm, na.rm = TRUE),
  flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE),
  body_mass_g = mean(body_mass_g, na.rm = TRUE)
)
# i pinguini con una massa corporea maggiore tendono ad avere il becco piu lungo e meno spesso
# quelli meno pesanti ad avere il becco piu corto e spesso
# la media dell'ala del cluster "rumoroso" e' nettamente maggiore dei altri due claster per colpa di un outlier

# Visualizzo i cluster
fviz_cluster(dbscan_result, data = penguins_scaled, geom = "point", ellipse = FALSE, show.clust.cent = FALSE) + theme_minimal()
# la visualizzazione non e' ottima per colpa dei punti rumorosi

# Filtro i punti rumorosi
penguins_scaled_filtered <- penguins_scaled[penguins$cluster != 0, ]

# Applico nuovamente DBSCAN sul dataframe filtrato (senza i punti rumorosi)
dbscan_result_filtered <- dbscan(penguins_scaled_filtered, eps = 0.8, minPts = 5)
dbscan_result_filtered

# Visualizzo i cluster senza i punti rumorosi
fviz_cluster(dbscan_result_filtered, data = penguins_scaled_filtered, geom = "point", ellipse = FALSE, show.clust.cent = FALSE) + theme_minimal()
# la prima dimensione ha una variabilita' dei dati maggiore rispetto alla seconda


# Scatter plot per vedere i cluster identificati
ggplot(penguins, aes(x = culmen_length_mm, y = culmen_depth_mm, color = cluster)) + geom_point() + theme_minimal()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = cluster)) + geom_point() + theme_minimal()
#notiamo 2 un outlier ovvero:
#una lunghezza negativa ed una sproporzionata(5k mm)

# filtrando:
ggplot(penguins_filtered, aes(x = flipper_length_mm, y = body_mass_g, color = cluster)) + geom_point() + theme_minimal()
# all'aumentare del peso corporeo aumenta anche la lunghezza delle ali
