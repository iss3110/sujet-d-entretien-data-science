data_train <- read.csv('dataset_train.csv',sep=";")

# dataset_train <- read_delim("dataset_train.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
# str(dataset_train)
str(data_train)
head(data_train)
summary(data_train)
dt=as.data.frame(data_train[order(data_train[,1],decreasing=F), ])

table(dt$season)
# ?table
n=length(dt$datetime)
#modele a effet mixtes ?
#On remarque que la variable season est mal codée, elle prend 12 modalités au lieu de 4

install.packages("timeDate")
library(timeDate)

timedata <- as.timeDate(dt$datetime)

season=rep("1",n)#hiver
index_ete=( as.numeric( format(timedata,format = "%m%d")) > 0620) & (as.numeric(format(timedata, format = "%m%d")) < 0923) #été
season[index_ete]="3" #ete

index_automne=((format(timedata,format = "%m%d") > "0922" & format(timedata, format = "%m%d") < "1221")) #automne
season[index_automne]="4" #aut

index_printemps=((format(timedata,format = "%m%d") > "0319" & format(timedata, format = "%m%d") < "0621"))#printemps
season[index_printemps]="2" #printemps

#index_hiver=as.logical(rep(1,n)-index_ete-index_automne-index_printemps)

season=as.factor(season)

#### holiday ####

#holiday:
#il y a 73 valeurs non indiquées de la variable holiday
index_na_holi=which(is.na(dt$holiday))
holidayy=dt$holiday
holidayy[index_na_holi]=holidayy[index_na_holi-1] #les valeurs non indiquées prennent la valeur la plus proche (supposant que c'est la plus probable)
holidayy=as.factor(holidayy)



#workingday:
workingdayy=dt$workingday
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

#### weather ####

#la meme pour weather :
weather=dt$weather
index_na_we=which(is.na(weather))
index_na_we
#weather le long de la journée i, on estimera donc la valeur manquante par la moyenne sur ce meme jour, arrondi en entier
weather[index_na_we]
for (i in index_na_we)
{
  ind=format(timedata,format = "%y%m%d")==format(timedata[i],format = "%y%m%d") #indices des lignes correspondant au meme jour i (qui vaut NA)
  weather[i]=round(mean(weather[ind][which(!is.na(weather[ind]))]))
}

weather=as.factor(weather)

#### data frame ####


#data frame final "propre" et trié:

dft=data.frame(timedata,season,holidayy,workingdayy,weather,dt$temp,dt$atemp,dt$humidity,dt$windspeed,dt$casual,dt$registered,dt$count)
names(dft)[match(names(dft)[c(1,3,4,6,7,8,9,10,11,12)],names(dft))] <- c("datetime","holiday","workingday","temp","atemp","humidity","windspeed","casual","registered","count")
head(dft)
summary(dft)
str(dft)
head(dft$registered)

library(dplyr)
#la library dplyr
dft1 <- dft %>% dplyr::filter(!is.na(dft$casual))#sans NA
# dft1 <- dft1 %>% dplyr::filter(!is.na(dft1$registred))
n_na <- length(dft1[,1])
# ?dplyr
summary(dft1)

#### graphes ####
colnames(dft1)[1] <- "timedata"
# ventes totales
library(ggplot2)
p <- ggplot(dft1, aes(x= dft1$timedata ,y=dft1$count,col=dft1$season,group=as.factor(dft1$holiday)))
p <- p + geom_point()+ geom_smooth(method=lm)
p

# vente des clients enregistrés

p <- ggplot(dft1, aes(x= dft1$timedata ,y=dft1$registred,col=dft1$season,group=as.factor(dft1$holiday)))
p <- p + geom_point()+ geom_smooth(method=lm)
p

# vente des clients non enregistrés :

p <- ggplot(dft1, aes(x= dft1$timedata ,y=dft1$casual,col=dft1$season,group=as.factor(dft1$holiday)))
p <- p + geom_point()+ geom_smooth(method=lm)
p




#### modele glm ####

mod=glm(dft[,12]~dft[,2],dft,family="poisson")  # regarder gls ?
plot(mod)
coef(mod)

#### préparation données test pour prédiction ####

# test : 6493 de 2011/01/20 0:0:0  -->  2012/12/31 23:0:0
# train : 10886 de 2011/01/01 0:0:0  -->  2012/12/19 23: 0 :0

dtest <- read.csv('C:/Users/issla/Desktop/entretien/dataset_test.csv',sep=",")
n_test <- length(dtest$datetime)
vente_casual <- as.vector(rep(0,n_test))
vente_registred <- rep(0,n_test)
vente_totale <- rep(0,n_test)
x1=as.factor(dtest$season)
x2=dtest$holiday
x3=dtest$workingday
x4=dtest$weather
x5=cut(dtest$temp,c(0.78,4.84,8.86,12.9,16.9,20.9,24.9,28.9,33,37,41))
summary(x5)
x6=cut(dtest$atemp,c(-0.1,5.23,9.7,14.2,18.6,23.1,27.6,32,36.5,41,45.5,50.1))
summary(x6)
x7=cut(dtest$humidity,c(-0.1,10,20,30,40,50,60,70,80,90,100))
x8=cut(dtest$windspeed,c(-0.056,5.6,11.2,16.8,22.4,28,33.6,39.2,44.8,50.4,56.1))
test.fact=data.frame(x1,x2,x3,x4,x5,x6,x7,x8,vente_casual,vente_registred,vente_totale)
colnames(test.fact)=c("season","holiday","workingday","weather","tempfact","atempfact","humifact","windfact","dcasual","dregistred","dcount")
summary(test.fact)
dim(test.fact)


#### prédiction modèle à effet mixte ####

#model1=lme(fixed=dcount~season+tempfact+atempfact,random=~1|season)
newtest=data.frame(x1,x5,x6,vente_totale)
colnames(newtest) <- c("season","tempfact","atempfact","dcount")
names(newtest)
summary(newtest)
pred=predict(model1,newdata=newtest)
plot(c(dft1$dt.count,3*pred),type="l")
summary(test.fact)
plot(model1)
summary(model1)
qqnorm(residuals(model1))
qqline(residuals(model1)) # Normality OK

plot(density(residuals(model1))) ## heteroscedasticity present (lme should handle this??) #

summary(model1)
anova(model1,test=T,type="marginal")

plot.lme(model1)

#### estimation non paramétrique ####
#delsol
library(sm)
d=density(dft1[,12],bw='ucv',kernel='e')
d2=sm.density(dft1[,12])
sm.regression(dft1[,c()],alti,h=h1,structure.2d='scaled')
predict(model1,newdata=test.fact,level=0:1)



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
tempfact <- cut(dft$temp,c(-10,0,15,30,55))
table(tempfact)
atempfact <- cut(dft$atemp,c(-10,0,15,30,55))
table(atempfact)
humifact <- cut(dft$humidity,c(0,30,50,70,100))
table(humifact)
summary(humifact)
windfact <- cut(as.integer(dft$windspeed),10)

dft.fact <- data.frame(season,holidayy,workingdayy,weather,tempfact,atempfact,humifact,windfact,dft[,10:12])

summary(dft.fact)
colnames(dft.fact)

#library(dplyr)
#dft1 <- dft %>% filter(!is.na(dft$dt.casual),!is.na(dft$dt.registered))#s
#dft.fact1 <- dft.fact %>% filter(!is.na(dft.fact$dt.casual),!is.na(dft.fact$dt.registered))

dcount=dft$count
dcasual=dft$casual
dregistred=dft$registered
#modele de regression a effet mixte:
model1=lme(fixed=dcount~season+tempfact+atempfact,random=~1|tempfact)
coef(model1)
summary(model1)
str(season)
class(season)
str(tempfact)
str(atempfact)
