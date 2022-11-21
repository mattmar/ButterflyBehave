setwd("~/Cours UCL/Memoire papillons")
setwd("C:/Users/lelevier/Downloads")

library(tidyverse)
library(data.table)
library(lubridate)
library(lme4)
library(lmerTest)
library(sjPlot)
library(rptR)
library(ggpubr)
library(psych)
library(car)
library(sjPlot)
library(sjmisc)

# Import RDS Leptidea
df.leptidea <- readRDS("df.leptidea.RDS")
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
grep("fp",df.leptidea$behaviour2)

#add a column with host plant presence 
quadrant=c("A1","A3","A5","B2","B4","C1","C5","D2","D4","E1","E3","E5")
vect=as.vector(df.leptidea$quadrant)
df.leptidea$hp_presence = ifelse(df.leptidea$quadrant %in% quadrant, "y", "n")

#add a column with the habitat of origin 
library("readxl")
collected=read_excel("./collected_specimens.xlsx", sheet="Sheet1")
collected=data.frame(
  id=collected$id,
  site=collected$site
)
collected$id=tolower(collected$id)
id=as.data.frame(match(df.leptidea$id,collected$id))
id

df.leptidea$id[df.leptidea$id == "leb01"] <- "lebo01"
collected$id[collected$id=="ledo1"]<- "ledo01"
collected$id[collected$id=="ledo2"]<- "ledo02"
collected$id[collected$id=="ledo3"]<- "ledo03"
collected$id[collected$id=="lemot01"]<- "lemo01"
#where is ledo05 in the collected? (lines 323 to 342)
df.leptidea$id[df.leptidea$id == "ledo1"] <- "ledo01"
df.leptidea$id[df.leptidea$id == "lesp2"] <- "lesp02"

df.leptidea$origin <- collected$site[match(df.leptidea$id,collected$id)]

saveRDS(df.leptidea,"df.leptidea2.RDS")

#####

#first visualisation of behavioural data 
#####

#TO DO : MAKE MORE GRAPHS
# Plot durations aggregated per behavioural category for all behaviours 
ggplot(df.leptidea, aes(x=behaviour2, y=c(duration)/sum(duration)*100)) +
  geom_col() +
  ylab("% of total time") +
  xlab("Type of behaviour")

#####

#evolution of behaviour according to trial number 
#####

#creating a new tab with only the factors we want for the analysis of the effect of trials 
#data frame for testing duration 
testtrial=aggregate(cbind(duration,prop_duration)~id+arena+n_trial+behaviour2+type+hp_presence+duration_test,data=df.leptidea,"sum")
testtrial <- subset(testtrial, behaviour2 !="ob")
grep("ob",testtrial$behaviour2)
testtrial <- subset(testtrial, type !="S_STM" )
testtrial <- subset(testtrial, type !="NS_STM" )
testtrial <- subset(testtrial, type !="S_LTM" )
testtrial <- subset(testtrial, type !="NS_LTM" )
grep("S_STM",testtrial$type)

#ouliers #### a regler demain avec le code de Pierre 
install.packages("outliers")
library(outliers)
grubs.test(testtrial$duration,type=11)

###
#checking data distribution:
testtrial$n_trial=as.factor(testtrial$n_trial)

ggplot(testtrial, aes(behaviour2, y=duration)) +
  geom_boxplot() +
  ylab("Average behaviour duration (in seconds)")+
  xlab("Type of behaviour")

library(rstatix)
res.kruskal=kruskal_test(duration~behaviour2, data= testtrial)
pwc2 <- testtrial %>% 
  wilcox_test(duration~behaviour2, p.adjust.method = "bonferroni")
pwc2
pwc2 <- pwc2 %>% add_xy_position(x = "behaviour2")
ggboxplot(testtrial, x = "behaviour2", y = "duration") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc2),
  )+
  xlab("Type of behaviour")+
  ylab("Average duration of behaviour (in seconds)")
#significant differences between nf and ef 
#but not between ef and walk (although almost) and between walk and nf
#curious
res.kruskal=kruskal_test(duration~behaviour2, data= testmemory)
pwc2 <- testmemory %>% 
  wilcox_test(duration~behaviour2, p.adjust.method = "bonferroni")
pwc2
pwc2 <- pwc2 %>% add_xy_position(x = "behaviour2")
ggboxplot(testmemory, x = "behaviour2", y = "duration") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc2),
  )+
  xlab("Type of behaviour")+
  ylab("Average duration of behaviour (in seconds)")

#looking at the effect of habitat of origin 
ggplot(testtrial, aes(origin, y=prop_duration)) +
  geom_boxplot() +
  facet_wrap(.~behaviour2, ncol=3, scales = "free_y")

#evolution of ob 
df.leptidea$n_trial=as.factor(df.leptidea$n_trial)
ggplot(df.leptidea[df.leptidea$behaviour2=="ob",], aes(type, y=prop_duration)) +
  geom_boxplot() +
  ylab("% of total time")

#####



### Modelling absolute behavioural durations
#####

# MM: We want to test trial number as a continuous variables 
#since we are not very interested in differences between single trials (eg, 3 vs 4 or 5 vs 2).
# MM: I see that the 6 and 7th trial are only for 8 and 2 ID and this is going to create problems in the model
#so let's aggregate 6 and 7 with 5

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
ggplot(testtrial.noresting, aes(n_trial, y=duration, fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  facet_wrap(.~behaviour2, ncol=2, scales = "free_y")

ggplot(testtrial.noresting, aes(n_trial, y=duration)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  facet_wrap(.~behaviour2, ncol=2, scales = "free_y")

ggplot(testtrial.resting, aes(n_trial, y=duration,fill=type)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  facet_wrap(.~behaviour2, ncol=1, scales = "free_y")

ggplot(testtrial.resting, aes(n_trial, y=duration)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  facet_wrap(.~behaviour2, ncol=1, scales = "free_y")

##glmer with poisson family 
mod_absd.poi=glmer(as.integer(duration) ~ n_trial*behaviour2+(1|arena/id), data = testtrial.noresting, family = "poisson")
# MM: another way to check for overdispersion is to see if the residulas exceed by far -2,2.
plot(fitted(mod_absd.poi), resid(mod_absd.poi), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Poisson'); abline(0,0)
# MM: Confirmed that Poisson it's not appropriate. We have problem of convergence, may be due to the eccessive number of interactions

#models testing effect of trial number 
#####

mod_absd.nb=glmer.nb(as.integer(duration) ~ n_trial*behaviour2+(1|arena/id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa"))
# MM: residuals pf NB look much better (not yet perfect)

plot(fitted(mod_absd.nb), resid(mod_absd.nb), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
# MM: AIC says the same!
AIC(mod_absd.poi,mod_absd.nb)
# MM: NB is going to be our golden standard
summary(mod_absd.nb)
# MM: Arena does not have much important as a random effect (var~0), all the variability is absorbed by ID, we can thus remove it to make the model lighter.
mod_absd.nb1 <- glmer.nb(as.integer(duration) ~ n_trial*behaviour2+(1|arena/id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa"))
plot(fitted(mod_absd.nb1), resid(mod_absd.nb1), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
text(fitted(mod_absd.nb1), resid(mod_absd.nb1), label=mod_absd.nb1@resp$y)
summary_data=summary(mod_absd.nb1)
summary_data
Anova(mod_absd.nb1)

####put table from parmentier 

#model not taking in account trial number: 
mod_absd.nb2 <- glmer.nb(as.integer(duration) ~ behaviour2+(1|arena/id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa"))
plot(fitted(mod_absd.nb2), resid(mod_absd.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
summary(mod_absd.nb2)
plot(fitted(mod_absd.nb2), resid(mod_absd.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
text(fitted(mod_absd.nb2), resid(mod_absd.nb2), label=mod_absd.nb2@resp$y)

anova=anova(mod_absd.nb1,mod_absd.nb2)
#difference almost significant ==> tendency to have an effect of trial number on feeding but not sure 
#no noticeable effects for other behaviours 


########### fin a way to download it
library(sjPlot)
tab_model(mod_absd.nb1)
library(htmlTable)
install.packages("gridExtra")
library(gridExtra)
install.packages("htlmtools")
library(saveWidget)

output=htmlTable(tab_model(mod_absd.nb1))
ggsave(grid.table(htmlTable(tab_model(mod_absd.nb1))),filename = "C:/Users/lelevier/Downloads/tab1.png")
#doesn't work, save a white image (nothing on it) 

library(kableExtra)
kbl <- kbl(anova)
kbl
table2 <- anova %>% kbl(caption="AOV") %>% kable_classic("striped",full_width=F) %>%
  column_spec(1, bold=T)
table2

hist(resid(mod_absd.nb1),xlab = "Residuals of the model",main="")
hist(resid(mod_absd.nb2),xlab = "Residuals of the model",main="")
shapiro.test(resid(mod_absd.nb1))
shapiro.test(resid(mod_absd.nb2))
plot(mod_absd.nb1)
#look in the codes from Rennes if we can find something 


# As a general outcome, less time is spent in navigation than escape or feeding (makes sense)
# Increasing the number of trials there is an increase in feeding but not navigation that remains stable
# Trial per se decreases the time spent in any behaviour, but this needs to be interpreted considering the intercept and the interaction
# We can see all this by plotting the model linear combinations

# All terms and interactions
theme_set(theme_sjplot())
plot_model(mod_absd.nb1, type = "int", pred.type = c("fe"),axis.title = c("Number of trial","Predicted duration of the behaviour"),title = "",legend.title = "Type of behaviour")
?plot_model

plot_model(mod_absd.nb1, type = "pred", terms=c("n_trial","behaviour2 [nf, ef]"), pred.type = c("fe"),axis.title = c("Number of trial","Predicted duration of the behaviour"),title = "",legend.title = "Type of behaviour")
# Trials
plot_model(mod_absd.nb1, type = "pred", terms="n_trial", pred.type = c("fe"))
#####
# Now we can check what's going on with resting
testtrial.resting <- testtrial[which(testtrial$behaviour2%in%"rest"),]
mod_absd.nb.rest <- glmer.nb(as.integer(duration) ~ n_trial+(1|id), data = testtrial.resting, control=glmerControl(optimizer="bobyqa"))
summary(mod_absd.nb.rest)
Anova(mod_absd.nb.rest)
# Trial per se decreases the time spent in resting as well, however the association is weak and unimportant
# Overall we can say that the number of trials has a positive effect on time spent feeding while a negative effect for all other behaviours. Why?

#add an "origin" column 
#####
df.leptidea$origin <- collected$site[match(df.leptidea$id,collected$id)]

#tests of effect of type on behavours  
#####
mod_type.nb1 <- glmer.nb(as.integer(duration) ~ behaviour2*type+(1|arena/id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa"))
summary(mod_type.nb1)
Anova(mod_type.nb1)

mod_type.nb0 <- glmer.nb(as.integer(duration) ~ behaviour2*type*n_trial+(1|arena/id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa"))


mod_type.nb2 <- glmer.nb(as.integer(duration) ~ behaviour2+type:n_trial+(1|arena/id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa"))
summary(mod_type.nb2)
Anova(mod_type.nb2)

anova(mod_absd.nb2,mod_type.nb1)
anova(mod_type.nb1,mod_type.nb2,mod_type.nb3)
#no significant differences 
#==> conclusion: no clear effect of trial on behavior for absolute behavior

plot(fitted(mod_type.nb1), resid(mod_type.nb1), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_type.nb1),xlab="residuals of the model",main="")
shapiro.test(resid(mod_type.nb1))

###comparative histograms
nf=testtrial.noresting[testtrial.noresting$behaviour2=="nf",]
nfS=nf[nf$type=="S",]
nfNS=nf[nf$type=="NS",]
Histogram_1 <- hist( nfS$duration, plot = FALSE)
Histogram_2 <- hist( nfNS$duration, plot = FALSE)


#modelling effect of type and trial number on behaviour duration 
mod_nf.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|arena/id), data =nf, control=glmerControl(optimizer="bobyqa"))
summary(mod_nf.nb2)
plot(fitted(mod_nf.nb2), resid(mod_nf.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_nf.nb2)).
shapiro.test(resid(mod_nf.nb2))

ef=testtrial.noresting[testtrial.noresting$behaviour2=="ef",]
mod_ef.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|arena/id), data =ef, control=glmerControl(optimizer="bobyqa"))
summary(mod_ef.nb2)
Anova(mod_ef.nb2)
plot(fitted(mod_ef.nb2), resid(mod_ef.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_ef.nb2))
shapiro.test(resid(mod_ef.nb2))

mod_rest.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|arena/id), data =testtrial.resting, control=glmerControl(optimizer="bobyqa"))
summary(mod_rest.nb2)
Anova(mod_rest.nb2)
plot(fitted(mod_rest.nb2), resid(mod_rest.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_ef.nb2))
shapiro.test(resid(mod_ef.nb2))


#test of feeding
fe=testtrial.noresting[testtrial.noresting$behaviour2=="fe",]
fe[fe$n_trial==5,3]=4
mod_fe.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|arena/id), data =fe, control=glmerControl(optimizer="bobyqa"))
summary(mod_fe.nb2)
Anova(mod_fe.nb2)
plot(fitted(mod_fe.nb2), resid(mod_fe.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_fe.nb2))
shapiro.test(resid(mod_fe.nb2))
hist(fe$duration)
ggplot(fe, aes(type, y=duration)) +
  geom_boxplot() +
  ylab("Duration of behaviour (in seconds)") +
  xlab("Number of trials")+
  facet_wrap(.~n_trial, ncol=5, scales = "free_y")

walk=testtrial.noresting[testtrial.noresting$behaviour2=="walk",]
mod_walk.nb2 <- glmer.nb(as.integer(duration) ~ type*n_trial+(1|arena/id), data =walk, control=glmerControl(optimizer="bobyqa"))
summary(mod_walk.nb2)
Anova(mod_fe.nb2)
plot(fitted(mod_walk.nb2), resid(mod_fe.nb2), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_walk.nb2))
shapiro.test(walk$duration)

########################## Do LATER: it doesn't put p-values at the right place 

?wilcox.test
res.kruskal=wilcox.test(walk$duration)
pwc2 <- walk %>% 
  wilcox_test(duration~type, p.adjust.method = "bonferroni")
pwc2
pwc2 <- pwc2 %>% add_xy_position(x = "type")
ggboxplot(walk, x = "type", y = "duration") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc2),
  )+
  xlab("Type of behaviour")+
  ylab("Average duration of behaviour (in seconds)")

hist(walk$duration)


# MM: Absolute resting time is much higher than any other behaviour, the model will have issue to handle this
aggregate(duration ~ n_trial+behaviour2, testtrial, "sum")

plot (Histogram_1, col = rgb(1,0,0,0.4),xlab = 'Observations',freq = FALSE, main = 'Comparative Histogram')
plot (Histogram_2, xaxt = 'n', yaxt = 'n',col = rgb(0,0,1,0.4), add = TRUE, freq = FALSE)

?bartlett.test


ggplot(testtrial.noresting, aes(type, y=duration)) +
  geom_boxplot() +
  ylab("Average duration of the behaviour (in seconds)") +
  xlab("Type of test (social or non-social)")+
  facet_wrap(.~behaviour2, ncol=2, scales = "free_y")

ggplot(testtrial.resting, aes(type, y=duration)) +
  geom_boxplot() +
  ylab("Average duration resting behaviours (in seconds)") +
  xlab("Type of test (social or non-social)")+
  facet_wrap(.~n_trial, ncol=3, scales = "free_y")

#effect of trial number and type on duration for nf
ggplot(testtrial.noresting[testtrial.noresting$behaviour2=="nf",], aes(type, y=duration)) +
  geom_boxplot() +
  ylab("Average duration of the behaviour (in seconds)") +
  xlab("Type of test (social or non-social)")+
  facet_wrap(.~n_trial, ncol=3, scales = "free_y")

#effect of trial number and type on duration for ef
ggplot(testtrial.noresting[testtrial.noresting$behaviour2=="ef",], aes(type, y=duration)) +
  geom_boxplot() +
  ylab("Average duration of the behaviour (in seconds)") +
  xlab("Type of test (social or non-social)")+
  facet_wrap(.~n_trial, ncol=5, scales = "free_y")

#effect of trial number and type on duration for fe
ggplot(fe, aes(type, y=duration)) +
  geom_boxplot() +
  ylab("Average duration of the behaviour (in seconds)") +
  xlab("Type of test (social or non-social)")+
  facet_wrap(.~n_trial, ncol=5, scales = "free_y")

#effect of trial number and type on duration for walk
ggplot(testtrial.noresting[testtrial.noresting$behaviour2=="walk",], aes(type, y=duration)) +
  geom_boxplot() +
  ylab("Average duration of the behaviour (in seconds)") +
  xlab("Type of test (social or non-social)")+
  facet_wrap(.~n_trial, ncol=5, scales = "free_y")


#####

#test of differences for proportional durations 

#the problem is here 
#####

testtrial.noresting$prop_duration=testtrial.noresting$prop_duration*10000000000
####
hist(testtrial.noresting$prop_duration)
mod_absd.nb1 <- glmer.nb(as.integer(prop_duration) ~ n_trial*behaviour2+(1|arena/id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa"))
plot(fitted(mod_absd.nb1), resid(mod_absd.nb1), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
text(fitted(mod_absd.nb1), resid(mod_absd.nb1), label=mod_absd.nb1@resp$y)
summary_data=summary(mod_absd.nb1)
summary_data
Anova(mod_absd.nb1)

#test of differences in the memory tests 
#####

#create dataframe: 
testmemory=aggregate(cbind(duration,prop_duration)~id+arena+n_trial+behaviour2+type+hp_presence+duration_test,data=df.leptidea,"sum")
testmemory <- subset(testmemory, type !="S" )
testmemory <- subset(testmemory, type !="NS" )
aggregate(id ~ n_trial, testmemory, function(x) length(unique(x)))
testmemory$n_trial[grep("6|7",testmemory$n_trial)] <- 5
testmemory <- subset(testmemory, behaviour2 !="ob")

testmemory.noresting <- testmemory[-which(testmemory$behaviour2%in%"rest"),]
testmemory.resting <- testmemory[which(testmemory$behaviour2%in%"rest"),]
ggplot(testmemory, aes(behaviour2, y=duration)) +
  geom_boxplot() +
  ylab("Average behaviour duration (in seconds)")+
  xlab("Type of behaviour")


####why is it doing that????
#p-values in the wrong places 
library(rstatix)
res.kruskal=kruskal_test(duration~behaviour2, data= testmemory)
pwc2 <- testmemory %>% 
  wilcox_test(duration~behaviour2, p.adjust.method = "bonferroni")
pwc2
pwc2 <- pwc2 %>% add_xy_position(x = "behaviour2")
ggboxplot(testmemory, x = "behaviour2", y = "duration") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc2),
  )+
  xlab("Type of behaviour")+
  ylab("Average duration of behaviour (in seconds)")

ggplot(testmemory.noresting, aes(type, y=duration)) +
  geom_boxplot() +
  ylab("Average duration of the behaviour (in seconds)") +
  xlab("Type of test")+
  facet_wrap(.~behaviour2, ncol=2, scales = "free_y")

mod_type_mem.nb1 <- glmer.nb(as.integer(duration) ~ type*behaviour2+((1|arena/id)), data = testmemory.noresting, control=glmerControl(optimizer="bobyqa"))
summary(mod_type_mem.nb1)
Anova(mod_type_mem.nb1)

plot(fitted(mod_type_mem.nb1), resid(mod_type_mem.nb1), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
hist(resid(mod_type_mem.nb1), xlab="Residuals of the model",main="")
shapiro.test(resid(mod_type_mem.nb1))

theme_set(theme_sjplot())
plot_model(mod_type_mem.nb1, type = "int", pred.type = c("fe"),axis.title = c("Type of test","Predicted duration of the behaviour"),title = "",legend.title = "Type of behaviour")

#second one looks better but we can't really trust it 

#####

#Shannon index 
#####
veg_data=read_excel("./veg_data.xlsx", sheet="Feuil1")
veg_data=data.frame(
  area=veg_data$AreaID,
  plot=veg_data$PlotID,
  x=veg_data$x,
  y=veg_data$y,
  lotus=veg_data$abu_Lotp,
  vicia=veg_data$abu_viciasep
)

veg_data$lotus=as.numeric(veg_data$lotus)
veg_data$vicia=as.numeric(veg_data$vicia)

veg_data=data.frame(
  area=veg_data$area,
  plot=veg_data$plot,
  x=veg_data$x,
  y=veg_data$y,
  hp=veg_data$lotus+veg_data$vicia
)


#creation of the oviposition table 
#####

ov=subset(df.leptidea,behaviour=="ov")
NSov=subset(ov,type=="NS")
Sov=subset(ov,type=="S")
Mov=subset(ov,!type=="NS")
Mov=subset(Mov,!type=="S")
install.packages("xlsx")
library(xlsx)
write.xlsx(ov,"ov.xlsx")
