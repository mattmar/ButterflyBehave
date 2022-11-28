install.packages("corrr")
install.packages("glmmTMB")
install.packages("AICcmodavg")

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
require(glmmTMB)
library(AICcmodavg)
library(ggplot2)

setwd("D:/Master BOE/Q2/Memoir/Stat")

data1=read_excel("./DONNE.xlsx", sheet = 1)
data2=read_excel("./DONNE.xlsx", sheet = 2)
data3=read_excel("./DONNE.xlsx", sheet = 3)

View(data1)
View(data2)
View(data3)

# Grouping based on origin

groups <- as.data.frame(matrix(c(
  "lebe", "belmont" , "torgny", 1,
  "lebo", "barvaux", "durbuy", 1,
  "ledo", "doische", "doische", 3,
  "ledou", "dourbes", "doische", 3,
  "ledu", "durbuy", "durbuy", 1,
  "ledy", "durbuy", "durbuy", 1,
  "lefe", "feschaux", "doische", 3,
  "lege", "gembloux", "nethen", 2,
  "lehan", "warret", "nethen", 2,
  "lema", "agimont", "doische", 3,
  "lemot", "membre", "doische", 3,
  "leon", "virton", "torgny",5,
  "lesp", "sant_pierre","nethen", 2,
  "leto", "torgny", "torgny", 5,
  "levi", "virion", "doische", 3,
  "levir", "virelles", "doische", 3,
  "levo", "virion", "doische", 3,
  "lewa", "waret", "nethen", 2),
  ncol=4, byrow=TRUE))
names(groups) <- c("id","area","ref_area","cluster")
groups$cluster <- factor(groups$cluster, levels=c(5,3,1,2), ordered=T)
groups <- groups[order(groups$cluster),]

view(groups)


################ Exploration des donnÃ©es############################
data11 <- na.omit(data1)
data11$`Area 1`=as.numeric(data11$`Area 1`)
data11$`Area 2`=as.numeric(data11$`Area 2`)
data11$`Area 3`=as.numeric(data11$`Area 3`)
data11$`Area 4`=as.numeric(data11$`Area 4`)
data11$cumm_area=(data11$`Area 1`+data11$`Area 2`+data11$`Area 3`+data11$`Area 4`)

View(data11)

summary(data11)
head
str(data11)
describeBy(data11, group="Code_site")

data11$Thorax=as.numeric(data11$Thorax)
data11$abdomen=as.numeric(data11$abdomen)
data11$eggs=as.numeric(data11$eggs)
data11$surv_days=as.numeric(data11$surv_days)
data11$cumm_area=as.numeric(data11$cumm_area)

table1 <- data11[,lapply(.SD, mean, na.rm=T), by=Code_site, .SDcols=c("Thorax", "abdomen", "cumm_area", "eggs", "surv_days")] 
table1 #### Error: Must subset columns with a valid subscript vector.x Subscript `lapply(.SD, mean, na.rm = T)` has the wrong type `list`. i It must be logical, numeric, or character.
theme_set(theme_bw())




###### Thorax
data11$Code_site=as.factor(data11$Code_site)
ggplot(data11,aes(Code_site ,y=Thorax))+
  geom_boxplot(aes(fill=Code_site),  
               Code_site="1",
               colour="black") + xlab("Aire géographique") +ylab("Poids thorax")

###### abdomen
data11$Code_site=as.factor(data11$Code_site)
ggplot(data11,aes(Code_site,y=abdomen))+
  geom_boxplot(aes(fill=Code_site),  
               Code_site="1",
               colour="black") + xlab("Aire géographique") +ylab("Poids abdomen")


###### area
data11$Code_site=as.factor(data11$Code_site)
ggplot(data11,aes(Code_site,y=cumm_area))+
  geom_boxplot(aes(fill=Code_site),  
               Code_site="1",
               colour="black") + xlab("Aire géographique") +ylab("Surfaice des ailes")

###### Survie
data11$Code_site=as.factor(data11$Code_site)
ggplot(data11,aes(Code_site,y=surv_days))+
  geom_boxplot(aes(fill=Code_site),  
               Code_site="1",
               colour="black") + xlab("Aire géographique") +ylab("Jours de survie")

######## normalitÃ© des donnÃ©es
install.packages("bestNormalize")
library(bestNormalize)
require(bestNormalize)
bestNormalize(data11$abdomen)
data11$abdomen=sqrt(data11$abdomen)

hist(data11$Thorax)
hist(data11$abdomen)
hist(data11$cumm_area)
shapiro.test(data11$Thorax) # Distribution normal
shapiro.test(data11$abdomen) # Distribution normal
shapiro.test(data11$cumm_area) # Distribution normal

bartlett.test(data11$Thorax, data11$Code_site) #pas assez de donnÃ©es dans certaines macro area
bartlett.test(data11$abdomen, data11$Code_site)
bartlett.test(data11$cumm_area, data11$Code_site)




############ Testes non paramÃ©trique  ########### car distribution gaussienne et non normal

####### Anova
anova_Thorax <- aov(Thorax ~ Code_site, data = data11)
summary(anova_Thorax) #pas significatif

anova_abdomen <- aov(abdomen ~ Code_site, data = data11)
summary(anova_abdomen) #pas significatif

anova_cummarea <- aov(cumm_area ~ Code_site, data = data11)
summary(anova_cummarea) # pas significatif

anova_survie <- aov(surv_days ~ Code_site, data = data11)
summary(anova_survie) #significatif


####### kruskal.test
kruskal.test(Thorax ~ Code_site, data = data11) #pas significatif -> pas de diffÃ©rence des moyennes
kruskal.test(cumm_area ~ Code_site, data = data11)#pas significatif -> pas de diffÃ©rence des moyennes
kruskal.test(abdomen ~ Code_site, data = data11) #pas significatif -> pas de diffÃ©rence des moyennes



############# ModÃ¨le linÃ©aire gÃ©nÃ©ralisÃ© ######################

#correlation entre les donnÃ©es
require(Hmisc)
require(corrplot)

correlation_thorax <- rcorr(data11$Thorax,data11$Code_site, type = "spearman") 
correlation_thorax
corrplot(correlation_thorax$r, type="upper", order="hclust", p.mat=correlation_thorax$P, sig.level=0.01, insig="blank", tl.cex=0.7  ,tl.col="black",tl.srt=45)

correlation_abdomen <- rcorr(data11$abdomen,data11$Code_site, type = "spearman") 
correlation_abdomen
corrplot(correlation_abdomen$r, type="upper", order="hclust", p.mat=correlation_abdomen$P, sig.level=0.01, insig="blank", tl.cex=0.7  ,tl.col="black",tl.srt=45)


correlation_ailes <- rcorr(data11$cumm_area,data11$Code_site, type = "spearman") 
correlation_ailes
corrplot(correlation_ailes$r, type="upper", order="hclust", p.mat=correlation$P, sig.level=0.01, insig="blank", tl.cex=0.7  ,tl.col="black",tl.srt=45)


correlation_survie <- rcorr(data11$surv_days,data11$Code_site, type = "spearman")
correlation_survie
corrplot(correlation_survie$r, type="upper", order="hclust", p.mat=correlation$P, sig.level=0.01, insig="blank", tl.cex=0.7  ,tl.col="black",tl.srt=45)


#chercher les valeurs extrÃªmes
dotchart(data11$Thorax, main="Thorax", group=data11$Code_site)
dotchart(data11$abdomen, main="abdomen", group=data11$Code_site)
dotchart(data11$cumm_area, main="area", group=data11$Code_site)




################## Build the model ##################

#######Thorax 
#Explicative = Code_site, cumm_area, surv_days, abdomen, eggs

Model_thorax1 = lmer(Thorax~Code_site+cumm_area+abdomen+surv_days+(1|area), data = data11) 
car::Anova(Model_thorax1, type=2)
summary(Model_thorax1)
AIC(Model_thorax1) #45.78387

Model_thorax2 = lm(Thorax~cumm_area+abdomen+surv_days, data = data11)
car::Anova(Model_thorax2, type=2)
summary(Model_thorax2)
AIC(Model_thorax2) ###############19.8901

Model_thorax3 = lm(Thorax~Code_site+cumm_area+abdomen+surv_days, data = data11)
car::Anova(Model_thorax3, type=2)
summary(Model_thorax3)
AIC(Model_thorax3) #21.84671

Model_thorax4 <-lm(Thorax~Code_site+(cumm_area*surv_days), data = data11)
summary(M2)
AIC(Model_thorax4)#52.617

Model_thorax5 = lm(Thorax~Code_site+cumm_area+abdomen, data = data11)
car::Anova(Model_thorax5, type=2)
summary(Model_thorax5)
AIC(Model_thorax5) #21.36181

Model_thorax6 = lm(Thorax~Code_site+abdomen+surv_days, data = data11)
car::Anova(Model_thorax3, type=2)
summary(Model_thorax3)
AIC(Model_thorax6) #23.55438



#######Abdomen 
#Explicative = Code_site, cumm_area, surv_days, thorax, eggs


Model_abdomen1 <- lm(abdomen~Thorax, data = data11)
car::Anova(Model_abdomen1, type=2)
summary(Model_abdomen1)
AIC(Model_abdomen1) ################## -11.88633

Model_abdomen2 <- lm(abdomen~Code_site+Thorax, data = data11)
car::Anova(Model_abdomen2, type=2)
summary(Model_abdomen2)
AIC(Model_abdomen2) # -10.08726

Model_abdomen3 <- lm(abdomen~(Thorax*cumm_area), data = data11)
car::Anova(Model_abdomen3, type=2)
summary(Model_abdomen3)
AIC(Model_abdomen3) #-5.58721




#######cumm_area 
#Explicative = Code_site, abdomen, surv_days, thorax

Model_ailes1 <- lm(cumm_area~Code_site+surv_days+Thorax+abdomen, data = data11)
AIC(Model_ailes1) #196.7801
summary(Model_ailes1)
car::Anova(Model_ailes1, type=2)

Model_ailes2 <- lm(cumm_area~Code_site+Thorax+surv_days, data = data11)
AIC(Model_ailes2) # 194.7814
summary(Model_ailes2)
car::Anova(Model_ailes2, type=2)

Model_ailes3 <- lm(cumm_area~surv_days+Thorax, data = data11)
AIC(Model_ailes3) ###################### 191.3525
summary(Model_ailes3)
car::Anova(Model_ailes2, type=2)




#######surv_days
#Explicative = Code_site, abdomen, cumm_area, thorax

Model_survie1<-glm(surv_days~Code_site+abdomen+cumm_area+Thorax, data= data11, family=poisson, na.action = na.omit)
AIC(Model_survie1) # 456.6961
summary(Model_survie1)

Model_survie2 = glmmTMB(surv_days~Code_site, data= data11, family=nbinom1)
AIC(Model_survie2) #496.4658
summary(Model_survie2)

Model_survie3 = glmmTMB(surv_days~Code_site+abdomen+Thorax+cumm_area, data= data11, family=nbinom1)
AIC(Model_survie3) #419.7242
summary(Model_survie3)

Model_survie4 = glmmTMB(surv_days~Code_site+Thorax+cumm_area, data= data11, family=nbinom1)
AIC(Model_survie4) ############## 417.7746
summary(Model_survie4)
car::Anova(Model_survie4, type=2)



############################# Plots

Model_survie4 = glmmTMB(surv_days~Code_site+Thorax+cumm_area, data= data11, family=nbinom1)
plot(resid(Model_survie4)~fitted(Model_survie4), xlab="predicted values", ylab="normalized residuals")+
  abline(h=0, lty=2)


Model_ailes2 <- lm(cumm_area~Code_site+Thorax+surv_days, data = data11)
plot(resid(Model_ailes2)~fitted(Model_ailes2), xlab="predicted values", ylab="normalized residuals")+
  abline(h=0, lty=2)


Model_abdomen2 <- lm(abdomen~Code_site+Thorax, data = data11)
plot(resid(Model_abdomen2)~fitted(Model_abdomen2), xlab="predicted values", ylab="normalized residuals")+
  abline(h=0, lty=2)


Model_thorax3 = lm(Thorax~Code_site+cumm_area+abdomen+surv_days, data = data11)
plot(resid(Model_thorax3)~fitted(Model_thorax3), xlab="predicted values", ylab="normalized residuals")+
  abline(h=0, lty=2)



######################################### Test #####################################################################

anova<-anova(Model_survie1, Model_survie2, Model_survie3, Model_survie4)
anova

ranova(Model_survie4)


ggplot(Model_survie4)

plot1 <- gglot(model1.1)

#normaliser les donnÃ©es
require(bestNormalize)
bestNormalize(data11$abdomen) ### Error in bestNormalize(data11$Thorax) : could not find function "bestNormalize"
data11$abdomen_2 = (data11$abdomen - mean(data11$abdomen))/sd(data11$abdomen) # SM: rescale the data

# SM: this is to check if mean and var are similar. In this case, it seems that the variance is quite small compared to the mean: maybe the poisson distibution is not good and you will have to use a negbinomial
mean(data11$surv_days)/var(data11$surv_days)
