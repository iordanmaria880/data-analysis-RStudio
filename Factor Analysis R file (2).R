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

# Reprezentari grafice

options(scipen=999)

windows()
par(mfrow=c(2,5)) #afisarea graficelor in aceeasi fereastra

# Distributia variabilei X1 - histograma + densitate de probabilitate
hist(tema$X1, col = "turquoise", main = "Histograma X1", freq = F)
lines(density(tema$X1), col="blue", lwd=3)

# Distributia variabilei X2 - histograma  + densitate de probabilitate
hist(tema$X2, col = "pink", main = "Histograma X2", freq = F)
lines(density(tema$X2), col="magenta", lwd=3)

# Distributia variabilei X3 - histograma  + densitate de probabilitate
hist(tema$X3, col = "green", main = "Histograma X3", freq = F)
lines(density(tema$X3), col="black", lwd=3)

# Distributia variabilei X4 - histograma  + densitate de probabilitate
hist(tema$X4, col = "aquamarine", main = "Histograma X4", freq = F)
lines(density(tema$X4), col="blue", lwd=3)

# Distributia variabilei X5 - histograma  + densitate de probabilitate
hist(tema$X5, col = "aquamarine", main = "Histograma X5", freq = F)
lines(density(tema$X5), col="magenta", lwd=3)

# Distributia variabilei X6 - histograma  + densitate de probabilitate
hist(tema$X6, col = "grey", main = "Histograma X6", freq = F)
lines(density(tema$X6), col="green", lwd=3)

# Distributia variabilei X7 - histograma  + densitate de probabilitate
hist(tema$X7, col = "pink", main = "Histograma X7", freq = F)
lines(density(tema$X7), col="red", lwd=3)

# Distributia variabilei X8 - histograma  + densitate de probabilitate
hist(tema$X8, col = "yellow", main = "Histograma X8", freq = F)
lines(density(tema$X8), col="black", lwd=3)

# Distributia variabilei X9 - histograma  + densitate de probabilitate
hist(tema$X9, col = "purple", main = "Histograma X9", freq = F)
lines(density(tema$X9), col="green", lwd=3)

# Distributia variabilei X10 - histograma  + densitate de probabilitate
hist(tema$X10, col = "red", main = "Histograma X10", freq = F)
lines(density(tema$X10), col="green", lwd=3)

windows()
# Dependenta dintre 2 variabile -> regresia
plot(tema$X1, tema$X2, col="black")
abline(lm(tema$X2~tema$X1), col="purple")

# Scatter plot
install.packages("ggplot2")
library(ggplot2)

ggplot(tema, aes(x=X1, y=X2))+
  geom_point()+
  geom_text(label=tema$Companie, color="magenta", 
            nudge_x = 0.25, nudge_y = 0.25,
            check_overlap = T)  

par(mfrow=c(2,5))
# Boxplot
boxplot(tema$X1, col="green", main="Boxplot X1")
boxplot(tema$X2, col="purple", main="Boxplot X2")
boxplot(tema$X3, col="blue", main="Boxplot X3")
boxplot(tema$X4, col="pink", main="Boxplot X4")
boxplot(tema$X5, col="magenta", main="Boxplot X5")
boxplot(tema$X6, col="lavender", main="Boxplot X6")
boxplot(tema$X7, col="grey", main="Boxplot X7")
boxplot(tema$X8, col="yellow", main="Boxplot X8")
boxplot(tema$X9, col="red", main="Boxplot X9")
boxplot(tema$X10, col="aquamarine", main="Boxplot X10")

# ANALIZA FACTORIALA

tema_AF = tema
View(tema_AF)

cor(tema_AF[-1])

# Elimin X8 si X9 

date_AF = cbind(tema_AF[,2:8], tema_AF[,11])

# Standardizarea datelor

date_AF_std = scale(date_AF, scale=TRUE)
View(date_AF_std)

# Testul KMO

install.packages("psych")
library(psych)
KMO(date_AF_std)
#pragul minim pentru a merge mai departe cu analiza factoriala este 0.5-0.6
#ideal este ca valoarea sa fie cat mai aproape de 1, in cazul nostru este 0.71, asadar continuam analiza factoriala

# Testul de sfericitate Bartlett

# H0-variabilele sunt ortogonale(matricea de corelatie este matricea identitate)
# H1- exista cel putin 1 factor comun care sa-mi explice corelatia dintre variabile
# Ne uitam la p-value-> daca e < de 0.05 => respingem ipoteza nula si acceptam ipoteza alternativa

R=cor(date_AF_std)
cortest.bartlett(R,n=81,diag=TRUE)

#respingem H0, acceptam H1: exista cel putin un factor comun

#Criterii de alegere al factorilor

#Criteriul Kaiser-retin in analiza comp principale care au suma >=1
#Criteriul procentului de acoperire-retin un nr de componente principala care impreuna contin un procent informational 75-80%
#Criteriul lui Screeplot

#Alegerea numarului de factori

acp = princomp(date_AF_std, cor=T)
acp

sdev = acp$sdev
valp = sdev*sdev
procent_info = (valp/sum(valp))*100
procent_cumulat=cumsum(procent_info)

X = data.frame(sdev, valp, procent_info, procent_cumulat)
X
View(X)

# Criteriul lui Screeplot
windows()
screeplot = prcomp(date_AF_std)
plot(screeplot, type = "l", main = "Screeplot", col = "purple")
#Pe axa orizontala avem componentele principale, iar pe axa verticala avem varianta/valorile proprii
#Acest screeplot il folosim ca un prim criteriu pt alegerea unui nr potrivit de comp principale
#Prima data ducem o dreapta paralela cu axa verticala din punctul de cotitura (5) -repr punctul in care panta incepe sa se apropie de 0 - si primul intreg din stanga dreptei va repr nr de componente pe care le pastram
#4 factori

# Criteriul lui Kaiser - vom retine in analiza componentele principale care au valorile proprii mai mari sau egale cu 1
#Observam ca valp sunt >= 1 pt primele 2 componente principale
#2 factori

# Criteriului procentului de acoperire - procent cumulat
#ar trebui sa avem un procent de acoperire in jur de 70-75-80%
#ne oprim la comp 2 deoarece are val de 80.28%
#2 comp principale 

# Pastram in final in analiza 2 componente principale/2 factori

# Estimarea modelului factorial

# 1. Metoda axelor principale

install.packages("GPArotation")
library(GPArotation)
library(psych)
factor1 = fa(date_AF_std, nfactors = 2, rotate = "none", fm = "pa")
print(factor1$loadings, cutoff = 0.4)

install.packages("corrplot")
library(corrplot)
corrplot(factor1$loadings)

factor1

# 2. Metoda verosimilitatii maxime

factor2 = fa(date_AF_std, nfactors = 2, rotate = "none", fm = "ml")

factor2 = fa(date_AF_std, nfactors = 2, rotate = "varimax", fm = "ml")

print(factor2$loadings, cutoff = 0.4)

corrplot(factor2$loadings)

  factor2

# Diagramele modelelor
windows()
fa.diagram(factor1)
fa.diagram(factor2)

# Generarea scorurilor factoriale
scoruri_ml <- factor2$scores 
# Adaugam scorurile la setul de date original
date_noi <- cbind(date_AF_std, scoruri_ml)
# Redenumim coloanele
colnames(date_noi)[(ncol(date_noi)-1):ncol(date_noi)] <- c("Performanta financiara", "Performanță operațională și venituri" )
# Salvarea noului set de date ca fisier csv
write.csv(date_noi, "date_ML.csv", row.names=FALSE)

View(date_noi)

# Asociem numele companiilor cu scorurile pentru a le afisa etichetele pe grafic
companii <- tema_AF$Companie
rownames(factor2$scores) <- companii

# Reprezentari grafice

windows()

# Scatterplot-ul observatiilor/Score plot

plot(factor2$scores[, 1], factor2$scores[, 2], 
     col = "#696969", pch = 16,
     main = "Score Plot pentru Observații", 
     xlab = "Factor 1", ylab = "Factor 2")
text(factor2$scores[,1], factor2$scores[,2], labels=rownames(factor2$scores), col="purple", cex=0.8, pos=3)

# Loading plot/Graficul incarcaturilor fiecarei variabile pe fiecare factor

plot(factor2$loadings[,1], factor2$loadings[,2], 
     xlab = "Factor 1", ylab = "Factor 2", 
     main = "Loading Plot pentru Factorii Analizei Factoriale", 
     pch = 16, col = "pink")
text(factor2$loadings[,1], factor2$loadings[,2], labels = rownames(factor2$loadings), col="black", cex=0.8, pos=3)

# Biplot

plot(factor2$scores[,1], factor2$scores[,2], 
     xlab = "Factor 1", ylab = "Factor 2", 
     main = "Biplot Analiza Factorială", 
     pch = 16, col = "blue")
text(factor2$scores[,1], factor2$scores[,2], labels=rownames(factor2$scores), col="blue", cex=0.8, pos=3)
arrows(0, 0, factor2$loadings[,1], factor2$loadings[,2], 
       col = "red", length = 0.1, angle = 20)
text(factor2$loadings[,1], factor2$loadings[,2], labels = rownames(factor2$loadings), 
     col = "red", cex = 0.8, pos = 3) 

# ANALIZA CORESPONDENTELOR

install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)
library(ggplot2)
install.packages("corrplot")
library(corrplot)

date = date_AC
date_ac = date[,-1]
date_ac=as.table(as.matrix(date_ac))
rownames(date_ac)=date$Regiuni
View(date_ac)

# Reprezentarea grafica 

install.packages("gplots")
library(gplots)
windows()
balloonplot(t(date_ac),main="Matricea de contingenta",xlab="",ylab="",
            label=T,show.margins=F)

# Testul de independenta chi^2
# Ipoteze:
# H0: nu exista asocieri intre variabile (liniile si coloanele matricii sunt independente)
# H1: exista asocieri intre variabile

x2=chisq.test(date_ac)
x2
# p-value = 0.00000000000000022 < 0.05 => respingem H0, acceptam H1 
# => Testul Chi-patrat sugereaza ca exista o asociere semnificativă între variabile, deoarece p-value este extrem de mic, putem spune ca este foarte puțin probabil ca observațiile sa fi aparut intamplator si ca exista o legatura semnificativa intre variabilele testate

# X-squared = 2259.4
# Valoarea mare sugereaza ca exista o asociere semnificativa intre variabilele implicate

# df reprezinta gradele de libertate ale testului, si se calculeaza ca (m-1)*(n-1) = 7*4 = 28

#Rezultate AC

rez=CA(date_ac,graph=F)
rez

#Valorile proprii

eig=get_eigenvalue(rez)
eig

rez$eig

#Inertia totala

s=sum(eig[,1])
s 
# Inertia totala de 0.03690236 sugereaza ca întreaga variabilitate a datelor este de aproximativ 0.0369 (valoarea totală a tuturor valorilor proprii)

# Scree-plot
windows()
fviz_screeplot(rez)
# Conform graficului, putem observa ca prima dimensiune explica mai mult de 60% din variatia totala, adica explica majoritatea variabilitatii in date, fiind urmata apoi de o cadere brusca, ce indica faptul ca urmatoarele componente nu adauga foarte multa variabilitate suplimentara

summary(rez,nb.dec=2) 

# Indicatorii pt dimensiunea linie

rez$row 

#Coordonatele 

rez$row$coord

#Contributiile
rez$row$contrib

#Calitatea reprezentarii
rez$row$cos2

#Inertia
rez$row$inertia

#Matricea factor pt randuri ????
cor(date_ac, rez$row$coord)

#Indicatorii pentru dimensiunea coloana

rez$col 

#Coordonatele
rez$col$coord

#Contributiile
rez$col$contrib

#Calitatea reprezentarii
rez$col$cos2

#Inertia
rez$col$inertia

#Matricea factor pentru coloane 
mf_coloane <- rez$col$coord
print(mf_coloane)

#Matricea factor pentru linii
mf_linii <- rez$row$coord
print(mf_linii)

# Reprezentari grafice

windows()

#Biplot

fviz_ca_biplot(rez)

fviz_ca_biplot(rez,map="row principal",arrow=c(T,T),repel=T)

windows()

# Contributia coloanelor la cele patru dimenisiuni
fviz_contrib(rez,choice="col",axes=1:4)

# Contributia randurilor la cele patru dimensiuni
corrplot(rez$row$contrib,is.corr=F)
fviz_contrib(rez,choice="row",axes=1:4)

# Calitatea repr pentru coloane
fviz_cos2(rez, choice = "col", axes = 1:2)

# Calitatea repr pentru linii
fviz_cos2(rez, choice = "row", axes = 1:2)

#Proiectia coloanelor pe primele 2 dim
fviz_ca_col(rez, axes=1:2)
#Proiectia randurilor pe primele 2 dim
fviz_ca_row(rez, axes=1:2)
