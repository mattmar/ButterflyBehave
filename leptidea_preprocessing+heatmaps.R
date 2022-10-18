#Initial processing Leptidea data for data analysis

### House-keeping
## Library
library(readxl)
library(ggplot2)

# Import excel sheet with infos on all trials (tested_specimens.xlsx)
my_data <- read_excel('/home/matteo/own_data/PoD/topics/sos/experimental_data/2022/tested_specimens.xlsx', sheet="leptidea")
my_data$time <- format(as.POSIXct(my_data$time), format = "%H:%M:%S")

# Derive transcript file names from folder (transcribed_recordings)
setwd('/home/matteo/own_data/PoD/topics/sos/experimental_data/2022/transcribed_recordings')
files <- list.files(recursive=T)
files <- gsub(".*/","",files)
files <- gsub("\\..*","",files)

# Transform in dataframe and everything in lowercase to avoid confusion
files.df <- read.table(text=tolower(files),col.names=c("file"))
transcript.df <- read.table(text=tolower(my_data$transcript), col.names="record",sep = ",")
transcript.df <- read.table(text=transcript.df[which(!transcript.df$record%in%tolower(" ledo02_2_sm_20220623_151241_mm")),], col.names="record", sep = ",") # Remove a recording transcribed twice (by MM and MB)

# Match records from trial files with file names to select only files of interest 
rmat <- transcript.df$record[!is.na(match(transcript.df$record,files.df$file))]
my_data$frmatch <- transcript.df$record[match(transcript.df$record,files.df$file)]

# Save a file with data matching column for a visual check
my_data$frmatch<-match(tolower(my_data$transcript), files.df$file)
write.csv(my_data, "tested_specimens_matched.csv")

### Data import and first visualisation 
## Reimport file list and format it
dfiles <- list.files(pattern="*xlsx", recursive=TRUE, full.names = FALSE)
dfiles1 <- dfiles[tolower(gsub(".*/|\\..*","",dfiles))%in%tolower(my_data$transcript)]

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
	x$start_time <- as.POSIXct(paste(x$day,strftime(x$start_time, format="%H:%M:%S", tz = "UTC"))) # added tz to solve an issue where all the hours were raised by +1
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
names(data2) <- names(data)
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
badbehaviours # Those are bad behaviours! Fix them

# More checks (not very useful)
# checkcc <- as.data.frame(do.call(rbind,lapply(data2, names)))
# checkcc$file <- dfiles1
# write.csv(checkcc, "/home/matteo/own_data/PoD/topics/sos/data_analysis/checkcc.xlsx")

## Aggregate data for a first visualisation
data3 <- lapply(data2, function(x) {
	y <- aggregate(duration ~ behaviour+id+day+start_time+arena+quadrant,data=x,sum)
	return(y)
	})
lepagg <- do.call(rbind.data.frame, data3)
lepagg$duration <- as.integer(lepagg$duration)

#Plot durations aggregated per behavioural category
ggplot(lepagg, aes(x=behaviour, y=c(duration)/sum(duration)*100)) +
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

lepagg2 <- lepagg
lepagg2$quadrant <- ifelse(lepagg2$quadrant == "E1/D1", "D1/E1", lepagg2$quadrant) #strange quadrants found


lepagg2 <- lepagg2 %>%
  dplyr::filter(quadrant != "D1/E1") %>%
  separate(quadrant, into = c("x_position", "y_position"), sep = 1, remove = FALSE, extra = "merge") %>%
  dplyr::mutate(x_position = toupper(x_position)) %>%
  tidyr::separate(day, into = c("day", NA), sep = " ")

lepagg2$day <- as.POSIXct(lepagg2$day, tryFormats = "%Y-%m-%d")
lepagg2$time <- format(as.POSIXct(lepagg2$start_time), format = "%H:%M:%S")

lepagg2$names <- rownames(lepagg2) #some ids in the rownames did not match with the ids in the file
lepagg2 <- lepagg2 %>% separate(names, into = c("id", NA), sep = "_")
lepagg2$id <- tolower(lepagg2$id)
lepagg2$id <- ifelse(lepagg2$id == "lady45", "ledy45", lepagg2$id) #fix a spelling error

ggplot(lepagg2,
#ggplot(lepagg2[!is.na(as.numeric(lepagg2$y_position)), ], 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10) +
  labs(x = "x", y = "y") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(vars(arena))

# SINGLE ARENAS (ONLY INTERIOR)

ggplot(data = subset(lepagg2[!is.na(as.numeric(lepagg2$y_position)), ], arena == 1), 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10, limits = c(0,1200)) +
  labs(x = "x", y = "y", title = "Leptidea arena 1") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(data = subset(lepagg2[!is.na(as.numeric(lepagg2$y_position)), ], arena == 2), 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10, limits = c(0,1200)) +
  labs(x = "x", y = "y", title = "Leptidea arena 2") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(data = subset(lepagg2[!is.na(as.numeric(lepagg2$y_position)), ], arena == 3), 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10, limits = c(0,1200)) +
  labs(x = "x", y = "y", title = "Leptidea arena 3") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggplot(data = subset(lepagg2[!is.na(as.numeric(lepagg2$y_position)), ], arena == 4), 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 13, limits = c(0,1400)) +
  labs(x = "x", y = "y", title = "Leptidea arena 4") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# DIVIDE BY TREATMENT (SOCIAL-NON SOCIAL)

my_individuals <- my_data %>%
  select(id, day, time, trial_n, type) %>%
  mutate(id = tolower(id))
  unique

intersect(my_individuals$id, lepagg2$id)
intersect(paste(my_individuals$id, my_individuals$day, my_individuals$time),
          paste(lepagg2$id, lepagg2$day, lepagg2$time))

lepagg2$link = paste(lepagg2$id, lepagg2$day, lepagg2$time)
my_individuals$link <- paste(my_individuals$id, my_individuals$day, my_individuals$time)
lepagg3 <- dplyr::left_join(lepagg2, my_individuals[,c(4:6)], by = c("link"), keep = FALSE)

# LOTS OF NAs: must check why

unique(subset(lepagg3, is.na(type))$id)


ggplot(lepagg3,
       #ggplot(lepagg3[!is.na(as.numeric(lepagg3$y_position)), ], 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10) +
  labs(x = "x", y = "y") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
  facet_grid(arena ~ type)


ggplot(data = subset(lepagg3[!is.na(as.numeric(lepagg3$y_position)), ], arena == 1), 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10, limits = c(0,1200)) +
  labs(x = "x", y = "y", title = "Leptidea arena 1") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
  facet_grid(.~ type)

ggplot(data = subset(lepagg3[!is.na(as.numeric(lepagg3$y_position)), ], arena == 2), 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10, limits = c(0,1200)) +
  labs(x = "x", y = "y", title = "Leptidea arena 2") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
  facet_grid(.~ type)

ggplot(data = subset(lepagg3[!is.na(as.numeric(lepagg3$y_position)), ], arena == 3), 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 10, limits = c(0,1200)) +
  labs(x = "x", y = "y", title = "Leptidea arena 3") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
  facet_grid(.~ type)


ggplot(data = subset(lepagg3[!is.na(as.numeric(lepagg3$y_position)), ], arena == 4), 
       aes(x_position, y_position)) +
  geom_tile(aes(fill = as.numeric(duration)), colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2, n.breaks = 13, limits = c(0,1400)) +
  labs(x = "x", y = "y", title = "Leptidea arena 4") +
  guides(fill=guide_legend(title="seconds spent in the quadrant")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom") +
  facet_grid(.~ type)

