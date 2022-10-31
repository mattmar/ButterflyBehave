#Initial processing Leptidea data for data analysis

setwd("~/Cours UCL/Memoire papillons")
### House-keeping
## Library
library("readxl")
library(ggplot2)

#pre-processing 
#####
# Import excel sheet with infos on all trials (tested_specimens.xlsx)
leptidea.trials <- read_excel("./tested_specimens.xlsx", sheet="leptidea")

# Derive transcript file names from folder (transcribed_recordings)
setwd('./transcribed_recordings')
files <- list.files(recursive=T)
files <- gsub(".*/","",files)
files <- gsub("\\..*","",files)

# Transform in dataframe and everything in lowercase to avoid confusion
files.df <- read.table(text=tolower(files),col.names=c("file"))
transcript.df <- read.table(text=tolower(leptidea.trials$transcript), col.names="record",sep = ",")
transcript.df <- read.table(text=transcript.df[which(!transcript.df$record%in%tolower(" ledo02_2_sm_20220623_151241_mm")),], col.names="record", sep = ",") 
# Remove a recording transcribed twice (by MM and MB)

# Match records from trial files with file names to select only files of interest 
rmat <- transcript.df$record[!is.na(match(transcript.df$record,files.df$file))]
leptidea.trials$frmatch <- transcript.df$record[match(transcript.df$record,files.df$file)]

# Save a file with data matching column for a visual check
leptidea.trials$frmatch<-match(tolower(leptidea.trials$transcript), files.df$file)
write.csv(leptidea.trials, "tested_specimens_matched.xlsx")

### Data import and first visualisation 
## Reimport file list and format it
dfiles <- list.files(pattern="*xlsx", recursive=TRUE, full.names = FALSE)
dfiles1 <- dfiles[tolower(gsub(".*/|\\..*","",dfiles))%in%tolower(leptidea.trials$transcript)]

#Check to see is there any residual Gonepteryx (not wanted for Leptidea)
grep("Go",dfiles1)

## Import all selected transcript sheets in a list in R
data <- lapply(dfiles1, read_excel)
# Name the list with file name
names(data) <- gsub("\\..*","",gsub(".*/","",dfiles1)) 

## Data processing to prepare them for analysis (derive duration and length, format date and time)
data1 <- lapply(names(data), function(Y) {
  message(Y)
  x<-data[Y][[1]]
  if(any(is.na(x$day))) {x <- x[-which(is.na(x$day)),]}
  if( any(grep("\\/",x$day) )) { 
    x$day<- gsub("\\/", "-", x$day); 
    strReverse <- function(x) {sapply(lapply(strsplit(x, "-"), rev), paste, collapse="-")}
    x$day<-strReverse(x$day)
  }
  x$start_time <- as.POSIXct(paste(x$day,strftime(x$start_time, format="%H:%M:%S", tz = "UTC")))
  x$activity_time <- as.POSIXlt(paste(x$day,strftime(x$activity_time, format="00:%M:%S")))
  x$activity_time <- x$start_time + x$activity_time$hour*60*60 + x$activity_time$min*60 + x$activity_time$sec
  x$length <- difftime(x$activity_time,x$start_time,units="secs")
  x$duration <- x$length - c(NA, x$length[1:(length(x$length)-1)])
  x$duration <- c(x$duration[2:length(x$duration)], x$length[length(x$length)]-x$length[length(x$length)])
  # Check for negative durations
  if(any(x$duration<0)) {
    print(as.data.frame(x[which(x$duration<0),]))
    stop("duration is negative")
  }
  x$n <- 1
  x$behaviour <- tolower(x$behaviour)
  return(x)
})

## Check for missing columns
cnames <- names(data1[[1]])
sapply(data1, ncol)

# Add missing columns if any is missing
data2 <- lapply(
  data1, function(x) {
    if( ncol(x)!=13 ) {
      if( any(!cnames %in% names(x)) )
        message("missing column", names(x))
      x[,cnames[which(!cnames%in%names(x))]] <- ""
      x <- x[cnames]
    } else{
      return(x)
    }
  }
)
names(data2)<-names(data)
sapply(data2, ncol)

## Check that the behavioural categories are correct 
behc <- tolower(c("EF", "NF", "FP", "FE", "WN", "WA", "WG", "RN", "RA", "RG", "LH", "OV"))

badbehaviours <- unlist(lapply(
  names(data2), function(Y) {
    x <- data2[Y][[1]]
    x$behaviour <- tolower(x$behaviour)
    if( any(!x$behaviour%in%behc) ){
      Z <- paste(Y, " unknown behaviour:", x$behaviour[which(!x$behaviour%in%behc)])
    } else {
      Z <- NULL
    }
    return(Z)
  }
))
badbehaviours # Those are bad bbehaviours! Fix them

# More checks (not very useful)
# checkcc <- as.data.frame(do.call(rbind,lapply(data2, names)))
# checkcc$file <- dfiles1
# write.csv(checkcc, "/home/matteo/own_data/PoD/topics/sos/data_analysis/checkcc.xlsx")

nnn <- names(data2)
## Aggregate data for a first visualisation
data3 <- lapply(1:length(data2), function(x) {
  y <- aggregate(duration ~ behaviour+id+day+start_time+arena+quadrant,data=data2[[x]],sum)
  y$transcript <- tolower(nnn[x])
  return(y)
})
lepagg <- do.call(rbind.data.frame, data3)
lepagg$duration <- as.integer(lepagg$duration)

# Merge dataset with overview data sheet using transcript name (to add type of test)
leptidea.trials$transcript <- tolower(leptidea.trials$transcript)
df.leptidea <- merge(lepagg, leptidea.trials[,c("type","transcript")], by="transcript", all.x=T)

# Check if dates are correct
#unique(row.names(df.leptidea[which(df.leptidea$start_time<as.POSIXct("2022-01-01")),]))
#df.leptidea[which(df.leptidea$start_time<as.POSIXct("2022-01-01")),]

# Add trial number to df.leptidea by using time
trr <- aggregate(id~as.factor(start_time)+arena,df.leptidea,"unique",simplify=FALSE)
names(trr)[1] <-"start_time"
trr$id <- unlist(trr$id)
trr <- trr[order(trr$id, partial=trr$start_time),]
trr$n_trial <-NA
for (g in unique(trr$id)) {
  trr[trr$id%in%g,]$n_trial <- as.integer(droplevels(trr[trr$id%in%g,]$start_time))
}

# Here the final dataset for Leptidea
df.leptidea <- merge(df.leptidea, trr[,-c(2)], by=c("id","start_time"),all.x=TRUE)

# Add a column for proportional time for each behaviour relatively to total test duration
df.leptidea <- merge(df.leptidea, aggregate(duration~id+n_trial, df.leptidea, "sum"), by=c("id","n_trial"))
names(df.leptidea)[9] <-"duration"
names(df.leptidea)[11] <-"duration_test"
df.leptidea$prop_duration <- df.leptidea$duration/df.leptidea$duration_test

#column behaviour2 with grouped behaviours 
df.leptidea$behaviour2 = as.character(df.leptidea$behaviour)
df.leptidea$behaviour2=ifelse(df.leptidea$behaviour2 == "ef"|
                                df.leptidea$behaviour2 == "fe"|
                                df.leptidea$behaviour2 == "fp"|
                                df.leptidea$behaviour2 == "lh"|
                                df.leptidea$behaviour2 == "nf"|
                                df.leptidea$behaviour2 == "ov",
                              df.leptidea$behaviour2, "rest")
df.leptidea$behaviour2=ifelse(df.leptidea$behaviour2 == "ef"|
                                df.leptidea$behaviour2 == "fe"|
                                df.leptidea$behaviour2 == "rest"|
                                df.leptidea$behaviour2 == "lh"|
                                df.leptidea$behaviour2 == "ov",
                              df.leptidea$behaviour2, "nf")
grep("fp",df.leptidea$behaviour2)

#add a column with host plant presence 
quadrant=c("A1","A3","A5","B2","B4","C1","C5","D2","D4","E1","E3","E5")
vect=as.vector(df.leptidea$quadrant)
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
testtrial=data.frame(
  id=df.leptidea$id,
  behaviour=df.leptidea$behaviour2,
  arena=df.leptidea$arena,
  duration=df.leptidea$duration,
  trial=df.leptidea$n_trial,
  prop_duration=df.leptidea$prop_duration,
  type=df.leptidea$type,
  hp_presence=df.leptidea$hp_presence
)

#data frame for testing duration 
testtrial2=aggregate(testtrial,duration~id+arena+trial+behaviour+type+hp_presence,sum)
testtrial2=testtrial2[order(testtrial2$id), ] 
testtrial2<- subset(testtrial2, behaviour !="ov")
grep("ov",testtrial2$behaviour)
testtrial2<- subset(testtrial2, type !="S_STM")
testtrial2<- subset(testtrial2, type !="S_LTM")
testtrial2<- subset(testtrial2, type !="NS_STM")
testtrial2<- subset(testtrial2, type !="NS_LTM")

#data frame for testing proportion
testtrial3=aggregate(testtrial,prop_duration~id+arena+trial+behaviour+type+hp_presence,sum)
testtrial3=testtrial3[order(testtrial2$id), ]
testtrial3<- subset(testtrial3, behaviour !="ov")
grep("ov",testtrial3$behaviour)
testtrial3<- subset(testtrial3, type !="S_STM")
testtrial3<- subset(testtrial3, type !="S_LTM")
testtrial3<- subset(testtrial3, type !="NS_STM")
testtrial3<- subset(testtrial3, type !="NS_LTM")

# Install and load the packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggpubr, tidyverse, DescTools, bestNormalize, lme4, lmerTest, Hmisc, data.table, lubridate, rptR, sjPlot, kableExtra, corrplot, PerformanceAnalytics, factoextra, knitr, psych, car)

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

###
#checking data distribution:
hist(testtrial2$duration)
hist(testtrial3$prop_duration)

#trial number as a factor 
testtrial2$trial=as.factor(testtrial2$trial)
testtrial3$trial=as.factor(testtrial3$trial)

ggplot(testtrial2, aes(trial, y=duration)) +
  geom_boxplot() +
  ylab("% of total time") +
  facet_wrap(.~behaviour, ncol=3, scales = "free_y")

ggplot(testtrial3, aes(trial, y=prop_duration)) +
  geom_boxplot() +
  ylab("% of total time") +
  facet_wrap(.~behaviour, ncol=3, scales = "free_y")
#####

#models I tested without normalizing that don't work 
#####

mod1=glmer(duration ~ behaviour*trial+(1|id)+(1|arena), data = testtrial2, family = inverse.gaussian(link = "log"))
shapiro.test(resid(mod1))
#p-value < 2.2e-16
hist(resid(mod1))
#(was also tested with all the other possible link function of the inverse.gaussian)
#(nothing worked)

mod1=glmer(duration ~ behaviour*trial+(1|id)+(1|arena), data = testtrial2, family = binomial(link = "log"))
#not working (tested with all possible link)

mod1=glmer(duration ~ behaviour*trial+(1|id)+(1|arena), data = testtrial2, family = quasibinomial(link = "logit"))
#why quasibinomial function cannot be used with glmer? 
mod1=glmer(duration ~ behaviour*trial+(1|id)+(1|arena), data = testtrial2, family =quasipoisson(link = "log"))
#quasipoisson neither? 

##could you explain why we cannot use "quasi" family for glmer? 

##Gamma ditribution don't work neither 
#gives too high p-values for the coefficients of the model
#that are not coherent with plots in descriptive analysis 
mod1=glmer(duration ~ behaviour*trial+(1|id)+(1|arena), data = testtrial2, family = Gamma(link = "log"))
shapiro.test(resid(mod1))
#p-value = 0.5796
hist(resid(mod1))
summary(mod1)
#very highly significant p values ==> weird, not in accordance with plots 
#probably not good 

mod2=glmer(duration ~ behaviour*trial+(1|id)+(1|arena), data = testtrial2, family = poisson(link="sqrt"))
shapiro.test(resid(mod2))
#p-value = 2.288e-09 
hist(resid(mod2))
#still no normality, tested with other link function of poisson family ==> don't work

#####

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


