tema1 <- Date_AD
View(tema1)

#Calcularea indicatorilor statistici 

summary(tema1)

install.packages("psych")
library(psych)

describe(tema1[-1])

#Matricea de corelatie si de covarianta

cor(tema1[-1])
cov(tema1[-1])

#Eliminarea outlierilor:
# Iterăm prin coloanele setului de date (excluzând prima coloană)
for (col in colnames(tema1)[-1]) {  # Excludem prima coloană folosind [-1]
  # Identificăm outlierii pentru fiecare coloană
  outliers <- boxplot(tema1[[col]], plot = F)$out
  
  # Excludem liniile care conțin outlieri
  tema1 <- tema1[-which(tema1[[col]] %in% outliers), ]
}

View(tema1)

#Pentru a observa mai bine rezultatele, vom standardiza datele
tema1_std = scale(tema1[-1], scale = T)
View(tema1_std)

#Recalculam corelatia si covarianta
matrice_corelatie <- cor(tema1_std)
matrice_covarianta <- cov(tema1_std)

#Reprezentarea grafica a matricei de corelatie

install.packages("corrplot")
library(corrplot)
?corrplot
windows()
corrplot(matrice_corelatie, method = "circle", type = "upper", col ="pink", title = "Matricea de corelatie" )

#Reprezentari grafice

options(scipen=999)

windows()
par(mfrow=c(1,2)) #afisarea graficelor in aceeasi fereastra

#Distributia variabilei X1 - histograma 
hist(tema1$X1, col = "turquoise", main = "Histograma X1")
#Densitatea de probabilitate
# Adaugam freq = F
hist(tema1$X1, col = "turquoise", main = "Histograma X1", freq = F)
lines(density(tema1$X1), col="blue", lwd=3)

#Distributia variabilei X2 - histograma 
hist(tema1$X2, col = "pink", main = "Histograma X2")
#Densitatea de probabilitate
# Adaugam freq = F
hist(tema1$X2, col = "pink", main = "Histograma X2", freq = F)
lines(density(tema1$X2), col="magenta", lwd=3)

#Distributia variabilei X6 - histograma 
hist(tema1$X6, col = "blue", main = "Histograma X6")
#Densitatea de probabilitate
# Adaugam freq = F
hist(tema1$X6, col = "blue", main = "Histograma X6", freq = F)
lines(density(tema1$X6), col="green", lwd=3)

#Dependenta dintre 2 variabile -> regresia
plot(tema1$X1, tema1$X2, col="black")
abline(lm(tema1$X2~tema1$X1), col="purple")

#scatter plot
install.packages("ggplot2")
library(ggplot2)

ggplot(tema1, aes(x=X1, y=X2))+
  geom_point()+
  geom_text(label=tema1$Companie, color="magenta", 
            nudge_x = 0.25, nudge_y = 0.25,
            check_overlap = T)  

par(mfrow=c(1,3))
#Boxplot
boxplot(tema1$X1, col="green", main="Boxplot X1")
boxplot(tema1$X2, col="purple", main="Boxplot X2")
boxplot(tema1$X6, col="lavender", main="Boxplot X6")

#Metoda ACP

View(tema1)
cor(tema1[-1])

#Observam ca X8 si X9 au corelatii negative cu majoritatea variabilelor, iar X6 are relatii de corelatie slaba cu celelalte variabile, asadar le voi elimina pe acestea din analiza pentru a simplifica modelul

date_acp <- cbind(tema1[,2:6], tema1[,8], tema1[,11])
View(date_acp)

date_std = scale(date_acp, scale = T)
View(date_std)

analizacp = princomp(date_std, cor=T)
analizacp

sdev = analizacp$sdev
valp = sdev*sdev
procent_info = (valp/sum(valp))*100
procent_cumulat=cumsum(procent_info)

ACP = data.frame(sdev, valp, procent_info, procent_cumulat)
ACP
View(ACP)

#Cele 3 criterii de alegere a nr de componente principale

#Criteriul 1 - Screeplot
windows()
screeplot = prcomp(date_std)
plot(screeplot, type = "l", main = "Screeplot", col = "magenta")
#Pe axa orizontala avem componentele principale, iar pe axa verticala avem varianta/valorile proprii
#Acest screeplot il folosim ca un prim criteriu pt alegerea unui nr potrivit de comp principale
#Prima data ducem o dreapta paralela cu axa verticala din punctul de cotitura (2) -repr punctul in care panta incepe sa se apropie de 0 - si primul intreg din stanga dreptei va repr nr de componente pe care le pastram
#O singura componenta principala

#Criteriul lui Kaiser - vom retine in analiza componentele principale care au valorile proprii mai mari sau egale cu 1
#Observam ca valp este >= 1 doar pentru prima componenta
#o componenta principala

#Criteriului procentului de acoperire - procent cumulat
#Cu toate ca ar trebui sa avem un procent de acoperire in jur de 70-75-80%, procentul cumulat al primei componente este de 90.18% (procentul cumulat minim), aratand ca o singura componenta explica suficient de mult varianta datelor, acoperind aproximativ 90% din varianta totala, inseamna ca aceasta captureaza o parte semnificativa din structura datelor 
#Pierderea informationala: 100 - 90.18 = 9.82%

#Pastram in analiza o componenta principala

a = analizacp$loadings
a
#Pe coloane se regasesc vectorii proprii
print(analizacp$loadings, cutoff = 0.0001)

#Forma generala a componentei principale

# W1 = 0.326 * X1 + 0.382 * X2 + 0.394 * X3 + 0.385 * X4 + 0.389 * X5 + 0.389 * X7 + 0.377 * X10

#Scorurile principale -inlocuim datele std in forma generala

scoruri_principale = matrix(analizacp$scores[,1], ncol = 1)
scoruri_principale

rownames(scoruri_principale) = tema1$Companie
scoruri_principale

#matricea factor - matricea de corelatie dintre variabilele originale si componentele principale
matrice_factor = cor(date_std, scoruri_principale)
matrice_factor
library(corrplot)
windows()
corrplot(matrice_factor, method = "number")
#Putem observa faptul ca componenta principala se coreleaza puternic cu toate celelalte variabile, de aceea aceasta componenta explica varianta datelor intr-o mare masura
#Avand in vedere corelatiile puternice cu variabilele originale, poate fi denumita PERFORMANTA FINANCIARA, deoarece capteaza aspecte esentiale legate de profitabilitatea si lichiditatea companiilor, fiind influentata de veniturile si fluxurile de numerar ce reflecta sanatatea financiara.

#Cercul corelatiilor
install.packages("FactoMineR")
library(FactoMineR)

windows()
cp = PCA(date_std)

#Reprezentarea scorurilor principale pentru diadele de componente principale

windows()

scoruri_principale2= data.frame(scoruri_principale)

plot(scoruri_principale2[,1], main = "Plot componente W1", col="green", xlab="W1")

text(scoruri_principale2[,1], labels=rownames(scoruri_principale2), col="blue", cex=1.2)

#Biplot

install.packages("factoextra")
library(factoextra)

fviz_pca_biplot(analizacp, repel=TRUE,
                col.var="#2E9FDF", #Variables color
                col.ind="#696969" #Individuals color
)

#Contributia si calitatea reprezentarii pentru variabile si indivizi
cp
summary(cp)

#Contributiile variabilelor

fviz_pca_var(analizacp, col.var="contrib",
             gradient.cols=c("red", "yellow", "green"),
             repel=TRUE) 

#Calitatea reprezentarii variabilelor

fviz_pca_var(analizacp, col.var="cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel=TRUE)


#Calitatea reprezentarii observatiilor

fviz_pca_ind(analizacp, col.ind="cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel=TRUE) 

#Contributiile observatiilor

fviz_pca_ind(analizacp, col.ind="contrib",
             gradient.cols=c("red", "yellow", "green"),
             repel=TRUE) 

