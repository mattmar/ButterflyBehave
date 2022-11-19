setwd("D:/Master BOE/Q2/Memoir/Stat")

install.packages(corrr)

library("readxl")
library("ggplot2")
library("lme4")
library(tidyverse)
library(corrr)
library(data.table)
library(lubridate)
library(lmerTest)
library(sjPlot)
library(rptR)
library(ggpubr)
library(psych)
library(car)

data1=read_excel("./DONNE.xlsx", sheet = 1)
data2=read_excel("./DONNE.xlsx", sheet = 2)
data3=read_excel("./DONNE.xlsx", sheet = 3)

View(data1)
View(data2)
View(data3)


################ Exploration des données############################

data11$`Area 1`=as.numeric(data11$`Area 1`)
data11$`Area 2`=as.numeric(data11$`Area 2`)
data11$`Area 3`=as.numeric(data11$`Area 3`)
data11$`Area 4`=as.numeric(data11$`Area 4`)
data11$cumm_area=(data11$`Area 1`+data11$`Area 2`+data11$`Area 3`+data11$`Area 4`)

data11 <- na.omit(data1)
View(data11)

summary(data11)
head
str(data11)
describe.by(data11, group="Code_site")

data11$Thorax=as.numeric(data11$Thorax)
data11$abdomen=as.numeric(data11$abdomen)
data11$eggs=as.numeric(data11$eggs)
data11$surv_days=as.numeric(data11$surv_days)
data11$cumm_area=as.numeric(data11$cumm_area)

table1 <- data11[,lapply(.SD, mean, na.rm=T), by=Code_site, .SDcols=c("Thorax", "abdomen", "cumm_area", "eggs", "surv_days")] 
table1 #### Error: Must subset columns with a valid subscript vector.x Subscript `lapply(.SD, mean, na.rm = T)` has the wrong type `list`. i It must be logical, numeric, or character.

###### Thorax
data11$Code_site=as.factor(data11$Code_site)
ggplot(data11,aes(Code_site,y=Thorax))+
  geom_boxplot()

###### abdomen
data11$Code_site=as.factor(data11$Code_site)
ggplot(data11,aes(Code_site,y=abdomen))+
  geom_boxplot()

###### area
data11$Code_site=as.factor(data11$Code_site)
ggplot(data11,aes(Code_site,y=cumm_area))+
  geom_boxplot()

######## normalité des données
hist(data11$Thorax)
hist(data11$abdomen)
hist(data11$cumm_area)
shapiro.test(data11$Thorax) # Distribution normal
shapiro.test(data11$abdomen) # Distribution non normal
shapiro.test(data11$cumm_area) # Distribution normal

bartlett.test(data11$Thorax, data11$Code_site) #pas assez de données dans certaines macro area
bartlett.test(abdomen, data = data11)
bartlett.test(cumm_area, data = data11)


############ Testes non paramétrique  ########### car distribution gaussienne et non normal

####### Anova
anova_Thorax <- aov(Thorax ~ Code_site, data = data11)
summary(anova_Thorax) #pas significatif
anova_abdomen <- aov(abdomen ~ Code_site, data = data11)
summary(anova_abdomen) #pas significatif
anova_cummarea <- aov(cumm_area ~ Code_site, data = data11)
summary(anova_cummarea) # pas significatif

####### kruskal.test
kruskal.test(Thorax ~ Code_site, data = data11) #pas significatif -> pas de différence des moyennes
kruskal.test(cumm_area ~ Code_site, data = data11)#pas significatif -> pas de différence des moyennes
kruskal.test(abdomen ~ Code_site, data = data11) #pas significatif -> pas de différence des moyennes



############# Modèle linéaire généralisé ######################

#correlation entre les données
correlation <- rcorr(Thorax,Code_site, type = "spearman") ###Error in rcorr(Thorax, Code_site, type = "spearman") : could not find function "rcorr"
correlation
corrplot(correlation$r, type="upper", order="hclust", p.mat=correlation$P, sig.level=0.01, insig="blank", tl.cex=0.7  ,tl.col="black",tl.srt=45)

#chercher les valeurs extrêmes
dotchart(data11$Thorax, main="Thorax", group=data11$Code_site)
dotchart(data11$abdomen, main="abdomen", group=data11$Code_site)
dotchart(data11$cumm_area, main="area", group=data11$Code_site)

#normaliser les données
bestNormalize(data11$Thorax) ### Error in bestNormalize(data11$Thorax) : could not find function "bestNormalize"



# Build the model

#Réponse = Thorax 
#Explicative = Code_site, cumm_area, surv_days, abdomen, eggs

M1<-lmer(Thorax~Code_site+cumm_area+abdomen~eggs+surv_days, data = data11, REML = F) ###Error in model.frame.default(data = data11, drop.unused.levels = TRUE,  : object is not a matrixanova(M1)

M2<-lm(Thorax~Code_site, data = data11)
View(M2)

model1<-glm(surv_days~Code_site, family=poisson)


