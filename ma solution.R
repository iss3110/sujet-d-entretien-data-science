library(dplyr)
library(ggplot2)
library(caret)
library(nlme)
library(randomForest)
library(ade4)
library(factoextra)
library(FactoMineR)

train <- read.csv('dataset_train.csv',sep=";")

summary(train)
head(train)
train <- as.data.frame(train[order(train[,1],decreasing=F), ]) # tri selon la date

table(train$season)
n=length(train$datetime) # 10886

train <- train %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) # mettre la chaine de caractère en format date

## Prétraitement des données, estimation des valeurs manquantes

# des valeurs abérantes dans season, elle prend 12 modalités au lieu de 4 (-2,1.5,Mist..)

# J'ai choisi de le déduire à partir de la date :
season=rep("1",n)#hiver

index_ete=( as.numeric( format(train[,1],format = "%m%d")) > 0620) & (as.numeric(format(train[,1], format = "%m%d")) < 0923) #été
season[index_ete]="3" #ete

index_automne=((format(train$datetime,format = "%m%d") > "0922" & format(train$datetime, format = "%m%d") < "1221")) #automne
season[index_automne]="4" #aut

index_printemps=((format(train$datetime,format = "%m%d") > "0319" & format(train$datetime, format = "%m%d") < "0621"))#printemps
season[index_printemps]="2" #printemps

#index_hiver=as.logical(rep(1,n)-index_ete-index_automne-index_printemps)

season <- factor(season,levels = c(1,2,3,4),labels = c("1-hiver","2-printemps","3-été","4-automne"))
str(season)
table(season) # maintenant la variable season est bien codée

train$season <- season

## workingday:

# En résumé :
summary(train$workingday)

# éliminer les NA
train$workingday <- na.locf(train$workingday) 

# il y a des jours travaillés ou l'on a mis 3 au lieu de 1:
train$workingday[train$workingday == 3] <- 1

# mettre en forme factor
train$workingday <- as.factor(train$workingday)

# nouveau résumé
summary(train$workingday)

#workingdayy <- train$workingday
#workkkin <- train$workingday

#index_na_wo=which(is.na(workingdayy))
#index_na_wo # 131 NA's
#str(index_na_wo)

#train[63,]
# 1ere solution
#while(sum(which(is.na(workingdayy)))) # tant qu'il y a un NA
  #{
  #for (i in which(is.na(workingdayy)))
    #{ workingdayy[i] <- workingdayy[i+1]
  #  }
#}

#2eme solution : na.locf de zoo

# comparer temps d'exécution


#t1 <- system.time({
#  while(sum(which(is.na(workingdayy)))) {
#    for (i in which(is.na(workingdayy))) {
#      workingdayy[i] <- workingdayy[i+1]
#    }
#  }
#})

#t2 <- system.time({
#  workkkin <- na.locf(workkkin)
#})
#summary(workkkin)
#summary(workingdayy)
#print(t1)
#t2[3]


#### weather ####

# la même façon weather on remplace la valeur manquante par la précédente, càd le temps d'une heure avant
  
summary(train$weather)
train$weather <- as.factor(na.locf(train$weather))
summary(train$weather)

#### holiday ####
summary(train$holiday)
train$holiday <- as.factor(na.locf(train$holiday))
summary(train$holiday)
#### data frame ####

str(train)
summary(train)
train <- train %>%
  select(-casual, -registered) # On supprime les colonnes casual et registered, seul total nous interesse

summary(train)
#data frame final d'entrainement du modèle prêt à l'utilisation 

#### graphes ####
# ventes totales

p <- ggplot(train, aes(x=datetime ,y=count,col=season))
p <- p + geom_point() +
  xlab("dates") + 
  ylab("Ventes par heure") +
  labs(col='Ventes moyennes par saison')
p


# vente des clients enregistrés

#p <- ggplot(train, aes(x= train$datetime ,y=train$registred,col=train$season,group=as.factor(train$holiday)))
#p <- p + geom_point()+ geom_smooth(method=lm)
#p

# vente des clients non enregistrés :

#p <- ggplot(train, aes(x= train$datetime ,y=train$casual,col=train$season,group=as.factor(train$holiday)))
#p <- p + geom_point()+ geom_smooth(method=lm)
#p


## les données test

test <- read.csv("dataset_test.csv")
length(test$datetime)
summary(test)
str(test)

## on remarque que les données test concernent les 20 derniers jours de chaque mois

test <- test %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"),
         season = factor(season),
         holiday = factor(holiday),
         workingday = factor(workingday),
         weather = factor(weather)) 
str(test) # data bien propre et même structure que train
str(train)
# tout est identique entre test et train, manque que season à remettre sur 1,2,3,4
levels(train$season) <- c("1","2","3","4")
str(train)
#ok, on passe à l'entrainement du modèle

## 1er modèle classification et regression du package caret 

model <- train(count ~ ., data = train, method = "glm", trControl = trainControl(method = "cv", number = 100))

str(train)
# Prédiction sur les données de test

str(test)
predictions <- predict(model, newdata = test)
str(predictions)
head(predictions)
View(predictions)
length(predictions)
length(test$datetime)

# Évaluation de la performance du modèle
RMSE <- sqrt(mean((test$count - predictions)^2))
cat("La RMSE du modèle est :", RMSE)
View(test)


#### modele glm ####

mod=glm(train[,10]~train[,2],train,family="poisson")  # regarder gls ?
plot(mod)
coef(mod)

# test : 6493 de 2011/01/20 0:0:0  -->  2012/12/31 23:0:0
# train : 10886 de 2011/01/01 0:0:0  -->  2012/12/19 23: 0 :0



#### modèle de régression à effet mixte (données  qualitatives explicatives) ####

#il faut coder temperature, humidité et windspeed en facteurs pour cette etude

#temperature comme facteur
tempfact <- cut(train$temp,c(-10,0,15,30,55))
table(tempfact)
atempfact <- cut(train$atemp,c(-10,0,15,30,55))
table(atempfact)
humifact <- cut(train$humidity,c(0,30,50,70,100))
table(humifact)
summary(humifact)
windfact <- cut(as.integer(train$windspeed),10)

train.fact <- data.frame(season,holidayy,workingdayy,weather,tempfact,atempfact,humifact,windfact,train[,10:12])

summary(train.fact)
colnames(train.fact)

#library(dplyr)
#dft1 <- train %>% filter(!is.na(train$casual),!is.na(train$registered))#s
#train.fact1 <- train.fact %>% filter(!is.na(train.fact$casual),!is.na(train.fact$registered))

#modele de regression a effet mixte:
model1=lme(fixed=train$count~train$season+train$tempfact+train$atempfact,random=~1|train$tempfact)
coef(model1)
summary(model1)


## Random forest


# Créer un modèle Random Forest en utilisant les données d'entraînement
# L'argument count ~ . spécifie qu'e nous voulons'on veut prédire la 
# variable count en fonction de toutes les autres variables

rf <- randomForest(train$count ~ ., data = train)

# puis utiliser le modèle pour faire des prédictions sur  test :

predictions_rf <- predict(rf, newdata = test)

output <- data.frame(predictions_rf)
write.csv(output, file = "predictions_rf.csv", row.names = FALSE)



#### acp ####

#cercle dorrélation
#cor1
#cor(as.matrix((dft1[,c(2:3,6:12)])),method="spearman", use = "complete.obs") #completer les 7 valeurs manquantes
#holiday potentiellement a exculre: aucun lien avec aucune variable

rollers.pca=dudi.pca(dft1[,6:12],scale=TRUE)
summary(rollers.pca)
plot(rollers.pca$tab[,c(1,7)]) #temperature explique pas mal count,
plot(rollers.pca$tab[,c(4,7)]) #plutot faible valeurs de windspeed
plot(rollers.pca$tab[,c(6,7)]) #useless puisque count=casual+registred
fviz_eig(rollers.pca)
s.corcircle(rollers.pca$co)
?s.corcircle

#ACP sur les 7 dernieres variables quantitatives, avec season comme variable supp
res.pca = PCA(dft1[,c(2,6:12)],quali.sup=1,scale.unit=TRUE,axes=c(1,2))
summary.PCA(res.pca)
plot()
#ACP avec holiday comme variable supp
res.pca = PCA(dft1[,c(3,6:12)],quali.sup=1,quanti.sup = c(3,6,7),scale.unit=TRUE,axes=c(1,2))
summary.PCA(res.pca)
#avec workingday
res.pca = PCA(dft1[,c(4,6:12)],quali.sup=1,quanti.sup = c(3,6,7),scale.unit=TRUE,axes=c(1,2))
summary.PCA(res.pca)
#avec weather
res.pca = PCA(dft1[,c(5,6:12)],quali.sup=1,quanti.sup = c(3,6,7),scale.unit=TRUE,axes=c(1,2))
summary.PCA(res.pca)

?PCA
res.pca$eig
plot(res.pca)
model=lme()
?PCA 
#projection des modalités sur le plan de l'acp, le barycente de chaque modalité projeté sur le graph d'individus pas des variables

#aide a l'interpretation : la qualité de representation(var et indiv), et la contribution


#Il s'agit d'un problème de prédiction de valeurs manquantes, ou imputation de données manquantes. Il existe plusieurs modèles adaptés pour ce type de problème, parmi lesquels :

#K-Nearest Neighbors (KNN) : Cette méthode consiste à trouver les K observations les plus proches de la donnée manquante et d'estimer la valeur manquante en calculant la moyenne ou la médiane des K observations les plus proches.

#Modèles de régression : Les modèles de régression tels que la régression linéaire, la régression logistique ou la régression polynomiale peuvent être utilisés pour imputer les valeurs manquantes en fonction des valeurs des autres variables.

#Random Forest : C'est une méthode d'apprentissage ensembliste qui peut être utilisée pour la prédiction de valeurs manquantes. Elle consiste à entraîner plusieurs arbres de décision sur des sous-échantillons de données et de prendre la moyenne des prédictions de chaque arbre.

#Imputation par chaînes de Markov : Cette méthode utilise la structure de dépendance entre les variables pour imputer les valeurs manquantes. Elle suppose que la distribution des données manquantes dépend uniquement de la distribution des données disponibles.

