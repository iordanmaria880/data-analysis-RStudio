tema <- Date_AD
View(tema)

# Eliminarea outlierilor:
# Iterăm prin coloanele setului de date (excluzând prima coloană)
for (col in colnames(tema)[-1]) {  # Excludem prima coloană folosind [-1]
  # Identificăm outlierii pentru fiecare coloană
  outliers <- boxplot(tema[[col]], plot = F)$out
  
  # Excludem liniile care conțin outlieri
  tema <- tema[-which(tema[[col]] %in% outliers), ]
}

View(tema)

# Calcularea indicatorilor statistici 

summary(tema)

install.packages("psych")
library(psych)

describe(tema[-1])

# Matricea de corelatie si matricea de covarianta
cor(tema[-1])
cov(tema[-1])

#Pentru a observa mai bine rezultatele, vom standardiza datele
tema_std = scale(tema[-1], scale = T)
View(tema_std)

# Recalculam corelatia si covarianta

matrice_corelatie <- cor(tema_std)
matrice_covarianta <- cov(tema_std)
View(matrice_corelatie)
View(matrice_covarianta)

# Reprezentarea grafica a matricei de corelatie

install.packages("corrplot")
library(corrplot)

windows()
corrplot(matrice_corelatie, method = "circle", type = "upper", col ="pink", title = "Matricea de corelatie" )

#Analiza cluster prin metode ierarhice

tema3 = tema
cor(tema3[-1])

#Elimin pe X8 si X9

date_3 = cbind(tema3[,2:8], tema3[,11])

# Standardizarea datelor

date_3_std = scale(date_3, scale=TRUE)
rownames(date_3_std)=tema3$Companie
View(date_3_std)

#Distanta dintre forme/observatii (similaritatea/disimilaritatea)
#Matricea distantelor (de proximitate) prin metoda euclidiana

dist_euclidiana <- dist(as.matrix(date_3_std),method="euclidian")
dist_manhattan <- dist(as.matrix(date_3_std),method="manhattan")

dist_euclidiana
dist_manhattan

#Calcularea distantei prin met. euclidiana, utilizand alta metoda de calcul:
#Distanta dintre firma SPOT si firma RDDT:

dist_euclidiana[1]

date_3_std[1:2,]
dist_spot_rddt=sqrt((date_3_std[1,1]-date_3_std[2,1])^2+
                       (date_3_std[1,2]-date_3_std[2,2])^2+
                       (date_3_std[1,3]-date_3_std[2,3])^2+
                       (date_3_std[1,4]-date_3_std[2,4])^2+
                       (date_3_std[1,5]-date_3_std[2,5])^2+
                       (date_3_std[1,6]-date_3_std[2,6])^2+
                       (date_3_std[1,7]-date_3_std[2,7])^2+
                       (date_3_std[1,7]-date_3_std[2,8])^2)
dist_spot_rddt

#Algoritmul aglomerativ de clusterizare ierarhica - utilizand doua metode de calcul a distantei dintre clustere

#Metoda single (distanta dintre cele mai apropiate puncte)
clust_single=hclust(dist_euclidiana,method="single")
cbind(clust_single$merge,clust_single$height)
windows()
plot(clust_single, main = "Clusterizare ierarhică - Metoda Single")

#Metoda complete (distanța dintre cele mai îndepărtate puncte)
clust_complete=hclust(dist_euclidiana,method="complete")
cbind(clust_complete$merge,clust_complete$height)
windows()
plot(clust_complete, main = "Clusterizare ierarhică - Metoda Complete")

#In continuare, vom folosi rezultatele clusterizarii prin metoda Complete Linkage

#Criterii de alegere a nr de clustere

#Criteriul general al clasificarii
#Dendograma-ne uitam de sus in jos,gasim distanta cea mai mare intre 2 etape succesive si realizam o taietura paralela cu cea orizontala

windows()
plot(clust_complete,labels=rownames(date_3_std))
rect.hclust(clust_complete,k=5,border="red")

#Grafic elbow
install.packages("factoextra")
library(factoextra)

fviz_nbclust(date_3_std, hcut, method = "wss")+
  geom_vline(xintercept = 6, linetype = 2)+
  labs(substitute = "Elbow method - STD")

#Calculul unor indici
install.packages("NbClust")
library(NbClust)

res <- NbClust(date_3_std, distance = "euclidean", min.nc = 3, max.nc = 10, method = "complete", index ="all")
res

#Graficul siluetei

library(cluster)

si_std <- silhouette(cutree(clust_complete, k = 6), dist_euclidiana)
plot(si_std, cex.names = 0.5)
si_std

#Daca s(i) -> 0 => obs. i este intre 2 clase
#Daca s(i) -> 1 => obs. i este bine incadrata in clasa
#Daca s(i) < 0 => obs. i este eronat incadrata

#Calcularea indicatorilor statistici per cluster

cutree_result <- cutree(clust_complete, k = 6)
cluster1 <- date_3_std[cutree_result == 1, ]
cluster2 <- date_3_std[cutree_result == 2, ]
cluster3 <- date_3_std[cutree_result == 3, ]
cluster4 <- date_3_std[cutree_result == 4, ]
cluster5 <- date_3_std[cutree_result == 5, ]
cluster6 <- date_3_std[cutree_result == 6, ]

summary(cluster1)
summary(cluster2)
summary(cluster3)
summary(cluster4)
summary(cluster5)
summary(cluster6)

describe(cluster1)
describe(cluster2)
describe(cluster3)
describe(cluster4)
describe(cluster5)
describe(cluster6)

#Algoritmul K-means

k_means = kmeans(date_3_std, 6)
k_means

#Calculul indicatorilor

tss = k_means$totss #variabilitatea totala/suma patratelor variatiilor
wss = k_means$tot.withinss #variabilitatea intraclasa/suma patratelor variatiilor intraclasa
bss = k_means$betweenss #variabilitratea interclasa/suma patratelor variatiilor interclasa
calit_part = bss/tss * 100 #calitatea partitiei

variab = cbind(tss, wss, bss, calit_part) 
variab

k_means$withinss #variabilitatea intraclasa pt fiecare clasa in parte
#suma acestor variabilitati da variabilitatea totala intraclasa (wss)
sum(k_means$withinss)

#Reprezentare grafica a claselor

clasa = k_means$cluster

c = cbind(clasa, round(date_3_std,6))
c

m = data.frame(c)
m

library(factoextra)

windows()
fviz_cluster(list(data = date_3_std, cluster = clasa))


#Calcularea indicatorilor statistici per cluster

cluster_1 <- subset(m, clasa == 1)
cluster_2 <- subset(m, clasa == 2)
cluster_3 <- subset(m, clasa == 3)
cluster_4 <- subset(m, clasa == 4)
cluster_5 <- subset(m, clasa == 5)
cluster_6 <- subset(m, clasa == 6)

summary(cluster_1)
summary(cluster_2)
summary(cluster_3)
summary(cluster_4)
summary(cluster_5)
summary(cluster_6)

library(psych)

describe(cluster_1)
describe(cluster_2)
describe(cluster_3)
describe(cluster_4)
describe(cluster_5)
describe(cluster_6)
