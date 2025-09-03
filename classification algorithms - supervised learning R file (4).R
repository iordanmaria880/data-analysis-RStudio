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

#Generarea variabilei de clasificare utilizand analiza cluster

tema3 = tema
cor(tema3[-1])

#Elimin pe X8 si X9

date_3 = cbind(tema3[,2:8], tema3[,11])

# Standardizarea datelor

date_3_std = scale(date_3, scale=TRUE)
rownames(date_3_std)=tema3$Companie
View(date_3_std)

#Generarea variabilei de clasificare

k_means = kmeans(date_3_std, 2)
k_means

#Adaugarea clasificarii in setul de date

clasa = k_means$cluster

dataset = cbind(clasa, round(date_3_std,3))
dataset

df = data.frame(dataset)

#Impartirea setului de date in date de antrenare si date de testare

nr=round(nrow(df)*.70) #70% date-set de antrenare,30%-set testare
a <- sample(seq_len(nrow(df)),size=nr)

antrenare <- df[a,] #setul de antrenare
testare <- df[-a,] #setul de testare
round(antrenare,3)
round(testare,3)

df_a=data.frame(antrenare)
df_a$clasa[df_a$clasa==1]<-"clasa1"
df_a$clasa[df_a$clasa==2]<-"clasa2"
cbind(round(df_a[,2:9],3),df_a[,1])

#Variabila dependenta este clasa

#Clasificatorul NAIV BAYESIAN

install.packages("e1071")
library(e1071)
model <- naiveBayes(as.factor(df_a[,1])~.,data=df_a[,-1])
summary(model)

model$apriori

model$tables

model$levels

#Realizarea de predictii pe setul de testare

#Class=probabilitatile aposteriorice de apartenenta la clasa
#Raw=probabilitatile aposteriorice de apartenenta la grupe
#Frecventa incrucisata=matricea de confuzie
#Gradul de clasificare corecta(acuratetea )=suma pe diagonala principala/suma totala*

pred_test <- predict(model,testare[,-1],type="class")
pred_test

pred_test2 <- predict(model,testare[,-1],type="raw")
pred_test2

#Matricea de confuzie

conf <- table(pred_test,testare[,1],dnn=c("Prediction","Actual"))

#Acuratetea modelului
acuratete <- sum(diag(conf)) / sum(conf)
acuratete

#Metoda KNN

install.packages("caret")
install.packages("MLmetrics")
library(MLmetrics)
library(caret)
library(e1071)

#Vom utiliza aceeasi distributie

df_a=data.frame(antrenare)
df_t=data.frame(testare)
df_a$clasa <- as.factor(df_a$clasa)
df_t$clasa <- as.factor(df_t$clasa)

levels(antrenare$clasa) <- make.names(levels(factor(antrenare$clasa)))
levels(testare$clasa) <- make.names(levels(factor(testare$clasa)))

#Setarea parametrilor pt validarea incrucisata repetata
repeats = 3
numbers = 10

set.seed(1234)

#Definirea controlului pentru validarea încrucișată repetată

x=trainControl(method="repeatedcv",number=numbers,repeats = repeats,
               classProbs = TRUE,summaryFunction = twoClassSummary)

#Definirea unor valori pt k
k_val = c(3,7,10)

#Vector pt acuratete
ac_val = c()

#Antrenarea modelului pt fiecare valoare a lui k

for (k in k_val) {
  model_knn <- train(clasa ~ .,data=antrenare,method="knn",
                     preProcess=c("center","scale"),trControl=x,metric="ROC",tuneLength=tunel)
  
  pred_test <- predict(model_knn,df_t[,-1])
  
  matrice_conf <- table(Predicted = pred_test, Actual = df_t$clasa)
  
  acuratete = sum(diag(matrice_conf)) / sum(matrice_conf)
  ac_val = c(ac_val, acuratete)
}

ac_val

#Vom alege modelul cu k=7 deoarece are cea mai mare valoare pentru acuratete

tunel = 7

model_knn_7 <- train(clasa ~ .,data=antrenare,method="knn",
                preProcess=c("center","scale"),trControl=x,metric="ROC",tuneLength=tunel)
model_knn_7

#Vizualizarea modelului
windows()
plot(model_knn_7)

#Realizarea de predictii pe setul de testare

#Predictii pentru clase
pred_clase <- predict(model_knn_7, testare)
pred_clase

#Predictii probabilistice
pred_prob <- predict(model_knn_7, testare, type ="prob")
pred_prob
head(pred_prob)

#Matricea de confuzie
matrice_confuzie <- table(pred_clase,testare[,1],dnn=c("Prediction","Actual"))
print(matrice_confuzie)

#Acuratetea modelului
acuratete <- sum(diag(matrice_confuzie))/sum(matrice_confuzie)
print(acuratete)

# Evaluarea modelului cu ROC și AUC

install.packages("ROCR")
library(ROCR)

pred_val <- prediction(pred_prob[,2],testare$clasa)
pred_val

perf_val <- performance(pred_val,"auc")
perf_val

perf_val <- performance(pred_val,"tpr","fpr")
plot(perf_val,col="green",lwd=1.5, main = "Curba ROC - Model KNN")

auc <- performance(pred_val,measure="auc")
auc <- auc@y.values[[1]]
auc

#Metoda arbore de decizie/Random forest

#Vom utiliza aceeasi distributie

df_a=data.frame(antrenare)
df_t=data.frame(testare)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

arbore_decizie <- rpart(clasa~.,data=df_a,method="class")
summary(arbore_decizie)
printcp(arbore_decizie)

windows()
plotcp(arbore_decizie)

arbore_decizie <- rpart(clasa~.,data=df_a,method="class",control=rpart.control(cp=0.18))
summary(arbore_decizie)
prp(arbore_decizie,type=4,extra=106,box.palette="BuPu",under=T,fallen.leaves =F )

#Realizarea de predictii pe setul de testare

#Predictii pentru clase
pred_clase_arbore <- predict(arbore_decizie, df_t, type="class")
pred_clase_arbore

#Predictii probabilistice
pred_prob_arbore <- predict(arbore_decizie, df_t, type ="prob")
pred_prob_arbore
head(pred_prob_arbore)

#Matricea de confuzie
matrice_confuzie_arbore <- table(pred_clase_arbore,df_t$clasa,dnn=c("Prediction","Actual"))
print(matrice_confuzie_arbore)

#Acuratetea modelului
acuratete_arbore <- sum(diag(matrice_confuzie_arbore))/sum(matrice_confuzie_arbore)
print(acuratete_arbore)

#Construim curba Roc pt arbore
install.packages("ROCR")
library(ROCR)
yhat2 <- predict(arbore_decizie,type="prob")[,2]
pr2 <- prediction(yhat2,df_a$clasa)
performanta <- performance(pr2,"tpr","fpr")
plot(performanta,colorize=T)
auc <- performance(pr2,"auc")
auc@y.values[[1]]

#Reprezentarea grafica a importantei variabilelor

importanta_var <- arbore_decizie$variable.importance

barplot(
  importanta_var,
  main = "Importanța variabilelor din arborele de decizie",
  xlab = "Variabile",
  ylab = "Valoarea importanței",
  col = "magenta",
  las = 2, 
  cex.names = 0.8 )

#Modelul de regresie logistica(binomiala)

#Scopul problemei: analiza influentei EBIT si a EPS asupra performantei financiare
#Variabila dependenta: clasa (1-performanta ridicata, 2-performanta slaba)
#Variabilele independente: EBIT si EPS

df_a=data.frame(antrenare)
df_t=data.frame(testare)
df_a$clasa <- as.factor(df_a$clasa)
df_t$clasa <- as.factor(df_t$clasa)

install.packages("ggplot2")
library(ggplot2)

plot <- ggplot(data=df_a,aes(x=df_a$X4,y=df_a$X5,col=clasa))
plot <- plot+geom_point(aes(size=5))
windows()
plot

model <- glm(clasa~X4+X5,data=df_a,family=binomial)
summary(model)

#Realizarea de predictii pe setul de testare

prob <- predict(model,df_t,type="response")
prob

#Matricea de confuzie
pred <- rep("1",dim(df_t)[1])
pred[prob>0.5]="2"
print(pred)
matrice_confuzie_reg <- table(pred,df_t$clasa)
print(matrice_confuzie_reg)

#Acuratetea
acuratete <- sum(diag(matrice_confuzie_reg))/sum(matrice_confuzie_reg)
print(acuratete)

#Curba ROC

install.packages("ROCR")
library(ROCR)

p <- predict(model,newdata=df_t,type="response")
pr <- prediction(p,df_t$clasa)
prf <- performance(pr,measure="tpr",x.measure="fpr")
plot(prf)

auc <- performance(pr,measure="auc")
auc <- auc@y.values[[1]]
auc
