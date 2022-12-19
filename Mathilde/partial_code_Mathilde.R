setwd("C:/Users/Utilisateur/Documents/Mémoire papillons")

library(sjmisc)
library(car)
library(psych)
library(ggpubr)
library(rptR)
library(sjPlot)
library(lmerTest)
library(lme4)
library(data.table)
library(tidyverse)

# Import RDS Leptidea
df.leptidea <- readRDS("df.leptidea2.RDS")
hist(df.leptidea$duration)

#column behaviour2 with grouped behaviours 
df.leptidea$behaviour2 = as.character(df.leptidea$behaviour)
df.leptidea$behaviour2=ifelse(df.leptidea$behaviour2 == "ef"|
                                df.leptidea$behaviour2 == "fe"|
                                df.leptidea$behaviour2 == "fp"|
                                df.leptidea$behaviour2 == "lh"|
                                df.leptidea$behaviour2 == "nf"|
                                df.leptidea$behaviour2 == "ov"|
                                df.leptidea$behaviour2 == "wg"|
                                df.leptidea$behaviour2 == "wa"|
                                df.leptidea$behaviour2 == "wn",
                              df.leptidea$behaviour2, "rest")
df.leptidea$behaviour2=ifelse(df.leptidea$behaviour2 == "ef"|
                                df.leptidea$behaviour2 == "fe"|
                                df.leptidea$behaviour2 == "fp"|
                                df.leptidea$behaviour2 == "lh"|
                                df.leptidea$behaviour2 == "nf"|
                                df.leptidea$behaviour2 == "ov"|
                                df.leptidea$behaviour2 == "rest",
                              df.leptidea$behaviour2, "walk")
df.leptidea$behaviour2=ifelse(df.leptidea$behaviour2 == "ef"|
                                df.leptidea$behaviour2 == "fe"|
                                df.leptidea$behaviour2 == "rest"|
                                df.leptidea$behaviour2 == "lh"|
                                df.leptidea$behaviour2 == "walk"|
                                df.leptidea$behaviour2 == "ov",
                              df.leptidea$behaviour2, "nf")
df.leptidea$behaviour2=ifelse(df.leptidea$behaviour2 == "ef"|
                                df.leptidea$behaviour2 == "fe"|
                                df.leptidea$behaviour2 == "rest"|
                                df.leptidea$behaviour2 == "nf"|
                                df.leptidea$behaviour2 == "walk",
                              df.leptidea$behaviour2, "ob")

#first visualisation of behavioural data 
#####

# Plot durations aggregated per behavioural category for all behaviours 
ggplot(df.leptidea, aes(x=behaviour2, y=c(duration)/sum(duration)*100)) +
  geom_col() +
  ylab("% of total time") +
  xlab("Type of behaviour")

#evolution of behaviour according to trial number 
#and the presence of social cue
#####

#creating a new tab with only the factors we want for the analysis of the effect of trials 
#data frame for testing duration 
testtrial=aggregate(cbind(duration,prop_duration)~id+arena+n_trial+behaviour2+type+duration_test,data=df.leptidea,"sum")
hist(testtrial$duration)
#problem here 
testtrial <- subset(testtrial, behaviour2 !="ob")
grep("ob",testtrial$behaviour2)
testtrial <- subset(testtrial, type !="S_STM" )
testtrial <- subset(testtrial, type !="NS_STM" )
testtrial <- subset(testtrial, type !="S_LTM" )
testtrial <- subset(testtrial, type !="NS_LTM" )
grep("S_STM",testtrial$type)
aggregate(id ~ n_trial, testtrial, function(x) length(unique(x)))
testtrial$n_trial[grep("6|7",testtrial$n_trial)] <- 5
# MM: Absolute resting time is much higher than any other behaviour, the model will have issue to handle this
aggregate(duration ~ n_trial+behaviour2, testtrial, "sum")
# MM: It is a wise decision to remove it from the model and test it separately afterwards
testtrial.noresting <- testtrial[-which(testtrial$behaviour2%in%"rest"),]
testtrial.resting <- testtrial[which(testtrial$behaviour2%in%"rest"),]
# MM: Check if the count distribution is overdispered: if the variance >>> mean 
mean(testtrial.noresting$duration)/var(testtrial.noresting$duration) # yes
# MM: Better we use negative binomial GLMER; but let's verify this with AIC.

###
#data visualisation 
testtrial.noresting$n_trial=as.factor(testtrial.noresting$n_trial)
testtrial.resting$n_trial=as.factor(testtrial.resting$n_trial)

ggplot(testtrial.noresting, aes(n_trial, y=duration, fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  facet_wrap(.~behaviour2, ncol=2, scales = "free_y")

ggplot(testtrial.resting, aes(n_trial, y=duration,fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  facet_wrap(.~behaviour2, ncol=1, scales = "free_y")

# Now we can check what's going on with resting
testtrial.resting$n_trial=as.integer(testtrial.resting$n_trial)
mod_absd.nb.rest <- glmer.nb(as.integer(duration) ~ n_trial+(1|id), data = testtrial.resting, control=glmerControl(optimizer="bobyqa"))
summary(mod_absd.nb.rest)
Anova(mod_absd.nb.rest)
hist(testtrial.resting$duration)
##/!\ errors: how come ? There is no errors in the df.leptidea 
#model don't work 
#error comes from line 67
#maybe elswhere 

#why does this not work anymore? 
ef=testtrial.noresting[testtrial.noresting$behaviour2=="ef",]
hist(ef$duration)
ef$n_trial=as.integer(ef$n_trial)
mod_ef.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|arena/id), data =ef, control=glmerControl(optimizer="bobyqa"))
summary(mod_ef.nb2)
Anova(mod_ef.nb2)
plot(fitted(mod_ef.nb2), resid(mod_ef.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_ef.nb2))
shapiro.test(resid(mod_ef.nb2))
#plot
ef$n_trial=as.factor(ef$n_trial)
ggplot(ef, aes(n_trial, y=duration, fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  coord_cartesian(xlim = c(1, 5), ylim = c(0, 800), clip = 'on')+
  scale_fill_brewer(palette="Dark2")
ggplot(ef, aes(n_trial, y=prop_duration, fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  coord_cartesian(xlim = c(1, 5), ylim = c(0, 0.8), clip = 'on')+
  scale_fill_brewer(palette="Dark2")

#test of feeding
fe=testtrial.noresting[testtrial.noresting$behaviour2=="fe",]
fe$n_trial=as.integer(fe$n_trial)
fe[fe$n_trial==5,3]=4
mod_fe.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|arena/id), data =fe, control=glmerControl(optimizer="bobyqa"))
summary(mod_fe.nb2)
Anova(mod_fe.nb2)
plot(fitted(mod_fe.nb2), resid(mod_fe.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_fe.nb2))
shapiro.test(resid(mod_fe.nb2))
hist(fe$duration)
#plot
fe$n_trial=as.factor(fe$n_trial)
ggplot(fe, aes(n_trial, y=duration, fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  coord_cartesian(xlim = c(1, 5), ylim = c(0, 1200), clip = 'on')+
  scale_fill_brewer(palette="Dark2")

#try to fit a model with a ^2 function? 

#test for wallking
walk=testtrial.noresting[testtrial.noresting$behaviour2=="walk",]
walk$n_trial=as.integer(walk$n_trial)
mod_walk.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|arena/id), data =walk, control=glmerControl(optimizer="bobyqa"))
summary(mod_walk.nb2)
Anova(mod_walk.nb2)
plot(fitted(mod_walk.nb2), resid(mod_walk.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_walk.nb2))
shapiro.test(walk$duration)
walk$n_trial=as.factor(walk$n_trial)
ggplot(walk, aes(n_trial, y=duration, fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  coord_cartesian(xlim = c(1, 5), ylim = c(0, 150), clip = 'on')+
  scale_fill_brewer(palette="Dark2")

##navigation flight
nf=testtrial.noresting[testtrial.noresting$behaviour2=="nf",]
nf$n_trial=as.integer(nf$n_trial)
#modelling effect of type and trial number on behaviour duration 
mod_nf.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|id), data =nf, control=glmerControl(optimizer="bobyqa"))
summary(mod_nf.nb2)
plot(fitted(mod_nf.nb2), resid(mod_nf.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_nf.nb2))
shapiro.test(resid(mod_nf.nb2))
#plot
nf$n_trial=as.factor(nf$n_trial)
ggplot(nf, aes(n_trial, y=duration, fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  coord_cartesian(xlim = c(1, 5), ylim = c(0, 250), clip = 'on')+
  scale_fill_brewer(palette="Dark2")

###
#time spent in the quadrats with host plant 


