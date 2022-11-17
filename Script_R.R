library("readxl")
library("ggplot2")
install.packages("usethis")
library("devtools")
devtools::install_github("dreamRs/esquisse")
esquisse::esquisser()
data1=read_excel("./DONNE.xlsx", sheet = 1)
data2=read_excel("./DONNE.xlsx", sheet = 2)
data3=read_excel("./DONNE.xlsx", sheet = 3)

View(data1)
View(data2)
View(data3)
str(data1)

######## Thorax
data1$Thorax=as.numeric(data1$Thorax)
data1$Code_site=as.factor(data1$Code_site)
ggplot(data1,aes(Code_site,y=Thorax))+
  geom_boxplot()

####### abdomen
data1$abdomen=as.numeric(data1$abdomen)
data1$Code_site=as.factor(data1$Code_site)
ggplot(data1,aes(Code_site,y=abdomen))+
  geom_boxplot()


####### area
data1$`Area 1`=as.numeric(data1$`Area 1`)
data1$`Area 2`=as.numeric(data1$`Area 2`)
data1$`Area 3`=as.numeric(data1$`Area 3`)
data1$`Area 4`=as.numeric(data1$`Area 4`)
data1$cumm_area=(data1$`Area 1`+data1$`Area 2`+data1$`Area 3`+data1$`Area 4`)

data1$Code_site=as.factor(data1$Code_site)
ggplot(data1,aes(Code_site,y=cumm_area))+
  geom_boxplot()


###### Anova
anova_Thorax <- aov(Thorax ~ Code_site, data = data1)
summary(anova_Thorax) #un peu significatif -> il y a une tendance

anova_abdomen <- aov(abdomen ~ Code_site, data = data1)
summary(anova_abdomen) #pas significatif

anova_cummarea <- aov(cumm_area ~ Code_site, data = data1)
summary(anova_cummarea) # très significatif




