# Install and load the packages
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(ggpubr, tidyverse, DescTools, bestNormalize, lme4, lmerTest, Hmisc, data.table, lubridate, rptR, sjPlot, kableExtra, corrplot, PerformanceAnalytics, factoextra, knitr, psych, car)

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

#column behaviour2 with grouped behaviours 
df.leptidea$behaviour2 <- factor(df.leptidea$behaviour)
levels(df.leptidea$behaviour2) <- c("ef","fe","fe","ov","nf","ov","rest","rest","rest","nf","nf","nf")

#Check for old behaviours
grep("fp",df.leptidea$behaviour2)

#add a column with host plant presence 
quadrant=c("A1","A3","A5","B2","B4","C1","C5","D2","D4","E1","E3","E5")
df.leptidea$hp_presence = ifelse(df.leptidea$quadrant %in% quadrant, "y", "n")

collected=read_excel("./collected_specimens.xlsx", sheet="Sheet1")
collected=data.frame(
  id=collected$id,
  site=collected$site
)

#####
#first visualisation of behavioural data 
#####

# Plot durations aggregated per behavioural category for all behaviours 
ggplot(df.leptidea, aes(x=behaviour, y=c(duration)/sum(duration)*100)) +
  geom_col() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))

ggplot(df.leptidea, aes(x=behaviour, y=c(duration)/sum(duration)*100)) +
  geom_boxplot() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))

#plot duration aggregated per behavioural category with grouped behavior 
ggplot(df.leptidea, aes(x=behaviour2, y=c(duration)/sum(duration)*100)) +
  geom_col() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))

ggplot(df.leptidea, aes(x=behaviour2, y=c(duration)/sum(duration)*100)) +
  geom_boxplot() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))

# Plot durations aggregated per behavioural category for all behaviours 
ggplot(df.leptidea, aes(x=behaviour, y=c(duration)/sum(duration)*100)) +
  geom_col() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))

ggplot(df.leptidea, aes(x=behaviour, y=c(duration)/sum(duration)*100)) +
  geom_boxplot() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))

#plot duration aggregated per behavioural category with grouped behavior 
ggplot(df.leptidea, aes(x=behaviour2, y=c(duration)/sum(duration)*100)) +
  geom_col() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))

ggplot(df.leptidea, aes(x=behaviour2, y=c(duration)/sum(duration)*100)) +
  geom_boxplot() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))

#####

#evolution of behaviour according to trial number 
#####

#creating a new tab with only the factors we want for the analysis of the effect of trials 
#data frame for testing duration 
testtrial=aggregate(cbind(duration,prop_duration)~id+arena+n_trial+behaviour2+type+hp_presence,data=df.leptidea,"sum")
testtrial <- subset(testtrial, behaviour2 !="ov")
grep("ov",testtrial$behaviour2)
testtrial <- subset(testtrial, type !="S_STM" | type !="S_LTM" | type !="NS_STM" | type !="NS_LTM")

### Check data distributions
# MM: Boxplots are useful to have an initial idea but they're hard to interpet.
gplotrial <- reshape2::melt(testtrial, measure.vars=c("duration","prop_duration"))
ggplot(gplotrial, aes(x=as.factor(n_trial), y=value)) +
  geom_boxplot() +
  ylab("% of total time") +
  facet_grid(variable~behaviour2, scales = "free_y")
# MM: Histograms are better, it seems that almost all our behaiours have distribution resembling count data, however resting time is problematic.
ggplot(gplotrial, aes(x=value, fill=behaviour2)) +
  geom_histogram(alpha=0.5) +
  ylab("% of total time") +
  facet_wrap(~variable, scale="free_x")
###

### Modelling absolute behavioural durations
# MM: We want to test trial number as a continuous variables since we are not very interested in differences between single trials (eg, 3 vs 4 or 5 vs 2).
# MM: I see that the 6 and 7th trial are only for 8 and 2 ID and this is going to create problems in the model, so let's aggregate 6 and 7 with 5
aggregate(id ~ n_trial, testtrial, function(x) length(unique(x)))
testtrial$n_trial[grep("7",testtrial$n_trial)] <- 6
# MM: Absolute resting time is much higher than any other behaviour, the model will have issue to handle this
aggregate(duration ~ n_trial+behaviour2, testtrial, "sum")
# MM: It is a wise decision to remove it from the model and test it separately afterwards
testtrial.noresting <- testtrial[-which(testtrial$behaviour2%in%"rest"),]
# MM: Check if the count distribution is overdispered: if the variance >>> mean 
mean(testtrial.noresting$duration)/var(testtrial.noresting$duration) # yes
# MM: Better we use negative binomial GLMER; but let's verify this with AIC.
mod_absd.poi=glmer(as.integer(duration) ~ n_trial*behaviour2+(1|arena)+(1|id), data = testtrial.noresting, family = "poisson")
# MM: another way to check for overdispersion is to see if the residulas exceed by far -2,2.
plot(fitted(mod_absd.poi), resid(mod_absd.poi), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Poisson'); abline(0,0)
# MM: Confirmed that Poisson it's not appropriate. We have problem of convergence, may be due to the eccessive number of interactions
mod_absd.nb=glmer.nb( as.integer(duration) ~ n_trial*behaviour2+(1|arena)+(1|id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa") )
# MM: residuals pf NB look much better (not yet perfect)
plot(fitted(mod_absd.nb), resid(mod_absd.nb), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
text(fitted(mod_absd.nb), resid(mod_absd.nb), label=mod_absd.nb@resp$y)
# MM: AIC says the same!
AIC(mod_absd.poi,mod_absd.nb)
# MM: NB is going to be our golden standard
summary(mod_absd.nb)
# MM: Arena does not have much important as a random effect (var~0), all the variability is absorbed by ID, we can thus remove it to make the model lighter.
mod_absd.nb1 <- glmer.nb( as.integer(duration) ~ n_trial*behaviour2+(1|id), data = testtrial.noresting, control=glmerControl(optimizer="bobyqa") )
plot(fitted(mod_absd.nb1), resid(mod_absd.nb), col='steelblue', pch=16, xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial'); abline(0,0)
summary(mod_absd.nb1)
# As a general outcome, less time is spent in navigation than escape or feeding (makes sense)
# Increasing the number of trials there is an increase in feeding but not navigation that remains stable
# Trial per se decreases the time spent in any behaviour, but this needs to be interpreted considering the intercept and the interaction
# We can see all this by plotting the model linear combinations
# All terms and interactions
theme_set(theme_sjplot())
plot_model(mod_absd.nb1, type = "int", pred.type = c("fe"))
# Just flights
plot_model(mod_absd.nb1, type = "pred", terms=c("n_trial","behaviour2 [nf, ef]"), pred.type = c("fe"))
# Trials
plot_model(mod_absd.nb1, type = "pred", terms="n_trial", pred.type = c("fe"))
#####
# Now we can check what's going on with resting
testtrial.resting <- testtrial[which(testtrial$behaviour2%in%"rest"),]
mod_absd.nb.rest <- glmer.nb(as.integer(duration) ~ n_trial+(1|id), data = testtrial.resting, control=glmerControl(optimizer="bobyqa"))
summary(mod_absd.nb.rest)
# Trial per se decreases the time spent in resting as well, however the association is weak and unimportant
# Overall we can say that the number of trials has a positive effect on time spent feeding while a negative effect for all other behaviours. Why?
###

################## Old code #####################
#test with lmer models, normalization of the data 
#####
hist(testtrial2$duration)
hist(testtrial3$prop_duration)

bestNormalize(testtrial2$duration)
#best option according to the function: "orderNorm" 
bestNormalize(testtrial3$prop_duration)
#best option according to the function: "orderNorm" too 

#Normalisation of data 
duration <- orderNorm(testtrial2$duration)
x2 <- predict(duration)
testtrial2$duration <- x2

prop_duration <- orderNorm(testtrial3$prop_duration)
x2 <- predict(prop_duration)
testtrial3$prop_duration <- x2

#verification
hist(testtrial2$duration)
hist(testtrial3$prop_duration)

shapiro.test(testtrial2$duration)
#p=0.201 ==> normal distribution 
shapiro.test(testtrial3$prop_duration)
#p=0.8229 ==> normal distribution 

###
#tests of models 
#with a grouped dataset

mod1=lmer(duration ~ behaviour*trial+(1|id)+(1|arena), data = testtrial2)
shapiro.test(resid(mod1))
#p-value = 1.169e-11 (a bit better than previous methods)
hist(resid(mod1))
#still no normality 
plot(resid(mod1))
#doesn't look so bad
summary(mod1)
Anova(mod1)
#significant effect of interaction behaviour:trial 

mod2=lmer(duration ~ behaviour+behaviour:trial+(1|id)+(1|arena), data = testtrial2)
shapiro.test(resid(mod2))
#p-value = 1.169e-11 
hist(resid(mod2))
#still no normality 
plot(resid(mod2))
#doesn't look so bad
summary(mod2)
Anova(mod2)
#significant effect of behaviour:trial disappeared 

mod3=lmer(duration ~ behaviour+trial+(1|id)+(1|arena), data = testtrial2)
shapiro.test(resid(mod3))
#p-value = 2.317e-11 
hist(resid(mod3))
#still no normality 
plot(resid(mod3))
#doesn't look so bad
summary(mod3)
Anova(mod3)
#no significant effect of trial 

anova(mod1,mod2,mod3)
#significant difference between mod1 and mod3


