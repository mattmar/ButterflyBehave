#Initial processing Gon. data for data analysis

### House-keeping
## Library
library(readxl)
library(ggplot2)
library(tidyverse)

# Import excel sheet with infos on all trials (tested_specimens.xlsx)
gonepteryx.trials <- read_excel("./tested_specimens.xlsx", sheet="gonepteryx")

# Derive transcript file names from folder (transcribed_recordings)
setwd('./transcribed_recordings/')
files <- list.files(recursive=T)
files <- gsub(".*/","",files)
files <- gsub("\\..*","",files)

# Transform in dataframe and everything in lowercase to avoid confusion
files.df <- read.table(text=tolower(files),col.names=c("file"))
transcript.df <- read.table(text=tolower(gonepteryx.trials$transcript), col.names="record",sep = ",")
transcript.df <- read.table(text=transcript.df[which(!transcript.df$record%in%tolower("Goma11_NS_3_20220516_145700_sm")),], col.names="record", sep = ",") # Remove a recording transcribed twice (by MM and SM)

# Match records from trial files with file names to select only files of interest 
rmat <- transcript.df$record[!is.na(match(transcript.df$record,files.df$file))]
gonepteryx.trials$frmatch <- transcript.df$record[match(transcript.df$record,files.df$file)]

# Save a file with data matching column for a visual check
gonepteryx.trials$frmatch<-match(tolower(gonepteryx.trials$transcript), files.df$file)
write.csv(gonepteryx.trials, "tested_specimens_Gon_matched.xlsx")

### Data import and first visualisation 
## Reimport file list and format it
dfiles <- list.files(pattern="*xlsx", recursive=TRUE, full.names = FALSE)
dfiles1 <- dfiles[tolower(gsub(".*/|\\..*","",dfiles))%in%tolower(gonepteryx.trials$transcript)]

#Check to see is there any residual Leptidea (not wanted for Gonepteryx)
grep("Le",dfiles1)

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
gonepteryx.trials$transcript <- tolower(gonepteryx.trials$transcript)
df.gonepteryx <- merge(lepagg, gonepteryx.trials[,c("type","transcript")], by="transcript", all.x=T)

# Check if dates are correct
#unique(row.names(df.gonepteryx[which(df.gonepteryx$start_time<as.POSIXct("2022-01-01")),]))
#df.gonepteryx[which(df.gonepteryx$start_time<as.POSIXct("2022-01-01")),]

# Add trial number to df.gonepteryx by using time
trr <- aggregate(id~as.factor(start_time)+arena,df.gonepteryx,"unique",simplify=FALSE)
names(trr)[1] <-"start_time"
 trr$id <- unlist(trr$id)

trr <- trr[order(trr$id, partial=trr$start_time),]
trr$n_trial <-NA
for (g in unique(trr$id)) {
 trr[trr$id%in%g,]$n_trial <- as.integer(droplevels(trr[trr$id%in%g,]$start_time))
}

# Here the final dataset
df.gonepteryx <- merge(df.gonepteryx, trr[,-c(2)], by=c("id","start_time"),all.x=TRUE)

# Plot durations aggregated per behavioural category
ggplot(df.gonepteryx, aes(x=behaviour, y=c(duration)/sum(duration)*100)) +
  geom_col() +
  ylab("% of total time") +
  ggtitle(paste("# of Tests:",length(data),"# of Individuals:", length(unique(lepagg$id))))


# =======================
#       HEATMAPS
#========================
library(readr)
library(tidyverse)
library(stringr)

col1 = "#d0e3e2" 
col2 = "#b00b1e"

## relative heatmaps: time spent in quadrants as a proportion of the total trial time per individual
## use average proportion of time?

df.gonepteryx.grouped<- df.gonepteryx

df.gonepteryx.grouped <- df.gonepteryx.grouped %>%
  dplyr::filter(quadrant != "A2/A3" & quadrant != "A1/A2") %>% # two double quadrants; for now let's remove them. They are two "rn" from GoMa11 2022-05-17 14:38:00 (4 seconds) and GoMa12 2022-06-05 15:50:00 (153 seconds)
  separate(quadrant, into = c("x_position", "y_position"), sep = 1, remove = FALSE, extra = "merge") %>%
  dplyr::mutate(x_position = toupper(x_position))

df.gonepteryx.grouped$id <- tolower(df.gonepteryx.grouped$id)

df.gonepteryx.grouped$duration_weighted <- ifelse(grepl(c("[+]"),df.gonepteryx.grouped$y_position),
                                           df.gonepteryx.grouped$duration*0.5, df.gonepteryx.grouped$duration)
df.gonepteryx.grouped$duration_weighted <- ifelse(df.gonepteryx.grouped$y_position == "1+" | df.gonepteryx.grouped$y_position == "5+",
                                           df.gonepteryx.grouped$duration*0.33, df.gonepteryx.grouped$duration_weighted)                                    
df.gonepteryx.grouped$y_position_expanded <- as.numeric(str_extract_all(df.gonepteryx.grouped$y_position,"\\(?[0-9,.]+\\)?"))

df.gonepteryx.grouped <- df.gonepteryx.grouped %>%
  group_by(id, start_time, arena, n_trial) %>%
  dplyr::mutate(trial_duration = sum(duration)) %>%
  ungroup() %>%
  dplyr:: mutate(duration_prop = duration/trial_duration) %>%
  dplyr:: mutate(duration_weighted_prop = duration_weighted/trial_duration)


df.gonepteryx.grouped.behaviours = df.gonepteryx.grouped %>%
  group_by(behaviour, arena, x_position, y_position) %>%
  summarise(global.proportion = mean(duration_prop, na.rm = TRUE))

df.gonepteryx.grouped.behaviours.weighted = df.gonepteryx.grouped %>%
  group_by(behaviour, arena, x_position, y_position_expanded) %>%
  summarise(global.proportion = mean(duration_weighted_prop, na.rm = TRUE))

df.gonepteryx.grouped.presence = df.gonepteryx.grouped %>%
  group_by(arena, x_position, y_position) %>%
  summarise(global.proportion = mean(duration_prop, na.rm = TRUE))

df.gonepteryx.grouped.presence.weighted = df.gonepteryx.grouped %>%
  group_by(arena, x_position, y_position_expanded) %>%
  summarise(global.proportion = mean(duration_weighted_prop, na.rm = TRUE))

# heatmaps for presence
# weighted

ggplot(data = df.gonepteryx.grouped.presence.weighted, 
       aes(x_position, y_position_expanded)) +
  geom_tile(aes(fill = as.numeric(global.proportion)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10) +
  labs(x = "x", y = "y",
       title = "Gonopteryx, time spent (weighted)") +
  guides(fill=guide_legend(title="time (trial proportion)")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(arena~.)


# heatmaps for some behaviours
# weighted

behaviours = c("nf", "ef", "fe")

for(i in 1:length(behaviours)) {
  plotdata <- subset(df.gonepteryx.grouped.behaviours.weighted, behaviour == behaviours[i])
  graph <- ggplot(data = plotdata, 
                  aes(x_position, y_position_expanded)) +
    geom_tile(aes(fill = as.numeric(global.proportion)), colour = "white", na.rm = TRUE) +
    scale_fill_gradient(low = col1, high = col2, n.breaks = 10) +
    labs(x = "x", y = "y",
         title = paste("Gonepteryx, behaviour: ", as.character(behaviours[i]), ", (weighted)",
                       sep = "")) +
    guides(fill=guide_legend(title="time (trial proportion)")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    facet_wrap(arena~.)
  print(graph)
  rm(plotdata, graph)
}

# as ovideposition are point events, it is better to sum

df.gonepteryx.count.ov = subset(df.gonepteryx.grouped, behaviour == "ov") %>%
  group_by(arena, x_position, y_position_expanded) %>%
  summarise(n.ov = n())

ggplot(data = df.gonepteryx.count.ov, 
       aes(x_position, y_position_expanded)) +
  geom_tile(aes(fill = as.numeric(n.ov)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 5) +
  labs(x = "x", y = "y",
       title = "Gonepteryx, n. of ovideposition events") +
  guides(fill=guide_legend(title="n. of ovideposition")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(arena~.)


