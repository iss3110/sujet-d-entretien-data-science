train <- read.csv('dataset_train.csv',sep=";")

str(train)
head(train)
summary(train)
train <- as.data.frame(train[order(train[,1],decreasing=F), ])

table(train$season)
# ?table
n=length(train$datetime) # 10886

## Prétraitement des données, estimation des valeurs manquantes

# des valeurs abérantes dans season, elle prend 12 modalités au lieu de 4 (-2,1.5,Mist..)

# J'ai choisi de le déduire à partir de la date :
season=rep("1",n)#hiver
index_ete=( as.numeric( format(timedata,format = "%m%d")) > 0620) & (as.numeric(format(timedata, format = "%m%d")) < 0923) #été
season[index_ete]="3" #ete

index_automne=((format(timedata,format = "%m%d") > "0922" & format(timedata, format = "%m%d") < "1221")) #automne
season[index_automne]="4" #aut

index_printemps=((format(timedata,format = "%m%d") > "0319" & format(timedata, format = "%m%d") < "0621"))#printemps
season[index_printemps]="2" #printemps

#index_hiver=as.logical(rep(1,n)-index_ete-index_automne-index_printemps)

season <- factor(season,levels = c(1,2,3,4),labels = c("1-hiver","2-printemps","3-été","4-automne"))
str(season)
table(season) # maintenant la variable season est bien codée

train$season <- season

View(train) # la variable season est bien remise
#### holiday ####

# il y a 73 valeurs non indiquées de la variable holiday
# au lieu d'éliminer les lignes et perdre des données, on peut estimer ces valeurs
index_na_holi=which(is.na(train$holiday))
holidayy=train$holiday
holidayy[index_na_holi]=holidayy[index_na_holi-1] #les valeurs non indiquées prennent la valeur la plus proche (supposant que c'est la plus probable)
holidayy=as.factor(holidayy)

train$holiday <- holidayy

## workingday:

workingdayy=train$workingday
index_na_wo=which(is.na(workingdayy))

#t[35,]
#ind=format(timedata,format = "%y%m%d")=="110507"
#t[ind,4]
#which(!is.na(t[ind,4]))
#workingdayy[35]=mean(t[ind,4][which(!is.na(t[ind,4]))])

for (i in index_na_wo)
{
  ind=format(timedata,format = "%y%m%d")==format(timedata[i],format = "%y%m%d") #les indices des lignes qui correspondent au meme jour que i, i parcours les NA.
  workingdayy[i]=round(mean(dt$workingday[which(!is.na(dt$workingday))])) #on prend la moyenne de workinday sur le meme jour
}
# il y a des jours travaillés ou l'on a mis 3 au lieu de 1:

for (i in 1:n)
  if (workingdayy[i]==3)
    workingdayy[i]=1

workingdayy=as.factor(workingdayy)
table(workingdayy)

train$workingday <- workingdayy

#### weather ####

#la meme pour weather :
weather=train$weather
index_na_we=which(is.na(weather))
index_na_we # on voit tous les indices des 53 valeurs manquantes dans la colonne weather
#weather le long de la journée i, on estimera donc la valeur manquante par la moyenne sur ce meme jour, arrondi en entier
weather[index_na_we]
for (i in index_na_we)
{
  ind=format(timedata,format = "%y%m%d")==format(timedata[i],format = "%y%m%d") #indices des lignes correspondant au meme jour i (qui vaut NA)
  weather[i]=round(mean(weather[ind][which(!is.na(weather[ind]))]))
}

weather=as.factor(weather)
table(weather)

train$weather <- weather

#### data frame ####

str(train)
summary(train)
train <- train %>%
  select(-casual, -registered) %>% # On supprime les colonnes casual et registered, seul total nous interesse
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"))

summary(train)
View(train) #data frame final d'entrainement du modèle prêt à l'utilisation 



#### graphes ####
# ventes totales
library(ggplot2)
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





#### modele glm ####

mod=glm(train[,10]~train[,2],train,family="poisson")  # regarder gls ?
plot(mod)
coef(mod)

# test : 6493 de 2011/01/20 0:0:0  -->  2012/12/31 23:0:0
# train : 10886 de 2011/01/01 0:0:0  -->  2012/12/19 23: 0 :0

test <- read.csv("dataset_test.csv")
View(test)
summary(test)
str(test)
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

## classification ey regression 

library(caret)
model <- train(count ~ ., data = train, method = "lm", trControl = trainControl(method = "cv", number = 5))

# Prédiction sur les données de test
count_pred <- data.frame(count_pred = rep(0, nrow(test))) # on initialise à 0
test <- cbind(test,count_pred)
View(test)
predictions <- predict(model, newdata = test)
str(predictions)
length(predictions)
length(test$datetime)

# Évaluation de la performance du modèle
RMSE <- sqrt(mean((test$count - predictions)^2))
cat("La RMSE du modèle est :", RMSE)
View(test)




#### acp ####

#cercle dorrélation
#cor1
#cor(as.matrix((dft1[,c(2:3,6:12)])),method="spearman", use = "complete.obs") #completer les 7 valeurs manquantes
#holiday potentiellement a exculre: aucun lien avec aucune variable
library(ade4)
library(factoextra)
rollers.pca=dudi.pca(dft1[,6:12],scale=TRUE)
summary(rollers.pca)
plot(rollers.pca$tab[,c(1,7)]) #temperature explique pas mal count,
plot(rollers.pca$tab[,c(4,7)]) #plutot faible valeurs de windspeed
plot(rollers.pca$tab[,c(6,7)]) #useless puisque count=casual+registred
fviz_eig(rollers.pca)
s.corcircle(rollers.pca$co)
?s.corcircle
library(FactoMineR)
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
library(factoextra)

?PCA
res.pca$eig
plot(res.pca)
model=lme()
?PCA 
#projection des modalités sur le plan de l'acp, le barycente de chaque modalité projeté sur le graph d'individus pas des variables

#aide a l'interpretation : la qualité de representation(var et indiv), et la contribution







#### modèle de régression à effet mixte (données  qualitatives explicatives) ####

library(nlme)
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

