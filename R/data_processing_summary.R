### This is an an example of how raw RFID data can be processed and summarised using R.
### Note in particular use of the following functions
###  - strptime() to correctly interpret dates and times (allowing summary functions to work)
###  - format() on values from strptime() to express things in week, month etc, so you can use it to summarise by day.
###  - The bit "Resample full data set" summarises PIT 'hits' by 10 second blocks (i.e. treats all hits of the same code as one)

library(xlsx)

### LOAD PIT TAG DATA:
pit <- read.xlsx('PIT TAG RINGS.xlsx',sheetIndex=1)
pit$PIT <- as.vector(pit$PIT)
pit$RING <- as.vector(pit$RING)
pit <- pit[!is.na(pit$RING),]
pit <- droplevels(pit)

### LOAD RINGING DATA:
if(file.exists('../../../Dropbox/000_Ringing/ipmr backup/Exports/ABBCRA PIT TAGS.csv')) {
  ring <- read.csv('../../../Dropbox/000_Ringing/ipmr backup/Exports/ABBCRA PIT TAGS.csv',header=T)  
} else {
  if(file.exists('C:/Users/jm340/Dropbox/000_Ringing/ipmr backup/Exports/ABBCRA PIT TAGS.csv')) {
    ring <- read.csv('C:/Users/jm340/Dropbox/000_Ringing/ipmr backup/Exports/ABBCRA PIT TAGS.csv',header=T)  
  } else { stop("Can't find IPMR PIT tag export!")}
}

### LOAD FEEDER READER DATA:
files <- list.files('2016/BFRM feeders/')[grep('.csv',list.files('2016/BFRM feeders/'))]
feed <- read.csv(paste('2016/BFRM feeders/',files[length(files)],sep=''),header=T)
# Match ring no's to FEED data:
feed$ring <- pit$RING[match(feed$TAG, pit$PIT)]
# Exclude test reads:
feed <- feed[!feed$ring=='TEST',]
# Match ringing date, species and age to feed data:
# First find original ringing data (exclude retraps):
ring1 <- ring[ring$RTYPE=='N' | ring$RTYPE=='C',]
ring1 <- droplevels(ring1)
ring1$date <- unlist(lapply(strsplit(as.character(as.vector(ring1$DATE)), " "), function(x) x[1]))
ring1$subsite <- as.character(ring1$USERV3)

feed$age <- ring1$AGE[match(feed$ring, ring1$RING)]
feed$sp <- ring1$SPEC[match(feed$ring, ring1$RING)]
feed$site <- ring1$SITE[match(feed$ring, ring1$RING)]
feed$subsite <- ring1$subsite[match(feed$ring, ring1$RING)]
feed$ringdate <- ring1$date[match(feed$ring, ring1$RING)]

### Drop strange blank lines:
feed <- feed[-grep("NA",row.names(feed)),]

feed <- droplevels(feed)

### Re-establish correct date values:
feed$DATETIME <- strptime(feed$DATETIME,"%Y-%m-%d %H:%M:%S")
feed$DATE <- as.Date(paste(feed$YEAR, feed$MONTH, feed$DAY),'%Y %m %d')
feed$WEEK <- format(feed$DATE, "%V")
feed$WEEK <- factor(feed$WEEK)

### How many PIT tagged birds total?
unique(feed$ring)
length(unique(feed$ring))
### How many (different) PIT tagged birds per feeder?
lapply(tapply(feed$ring, feed$LOC, unique),length)
### How many (different) PIT tagged birds per week?
perweek <- as.data.frame(lapply(tapply(feed$ring, feed$WEEK, unique),length))
names(perweek) <- names(lapply(tapply(feed$ring, feed$WEEK, unique),length))
perweek
datesweek <- data.frame(date=unique(feed$DATE))
datesweek <- data.frame(date=datesweek[order(datesweek$date),])
datesweek$week <- format(datesweek$date,"%V")

### Summarise no. visits by feeder and date:
feed_d <- data.frame(DATE=seq(min(feed$DATE), max(feed$DATE), 1))
feed_d <- cbind(feed_d, matrix(NA, ncol=12, nrow=nrow(feed_d)))
names(feed_d)[2:length(names(feed_d))] <- paste("F", names(feed_d)[2:length(names(feed_d))], sep='')
feed_summary <- as.data.frame(tapply(feed$TAG, list(feed$DATE, feed$LOC), length))
feed_summary$DATE <- row.names(feed_summary)
row.names(feed_summary) <- NULL
feed_summary <- subset(feed_summary, select=c(names(feed_summary)[length(names(feed_summary))], names(feed_summary)[grep("F",names(feed_summary))]))

feed_d[,2:10] <- feed_summary[match(as.character(feed_d$DATE),as.character(feed_summary$DATE)),2:10]

feed_d_sept16 <- feed_d[as.Date(feed_d$DATE, "%Y-%m-%d")>=as.Date("2016-09-01", format="%Y-%m-%d"),]

par(mar=c(2,4,2,1))
par(mfrow=c(9,1))
barplot(feed_d_sept16$F1, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))
barplot(feed_d_sept16$F2, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))
barplot(feed_d_sept16$F3, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))
barplot(feed_d_sept16$F4, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))
barplot(feed_d_sept16$F5, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))
barplot(feed_d_sept16$F6, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))
barplot(feed_d_sept16$F7, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))
barplot(feed_d_sept16$F8, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))
barplot(feed_d_sept16$F9, names.arg=feed_d_sept16$DATE, xaxt='n', ylim=c(0,max(feed_d_sept16[,2:10], na.rm=T)))



# Number of different feeders visited by birds ringed as pulli:
tapply(feed$LOC, feed$ring, function(x) levels(factor(as.vector(x))))

# Focus on birds ringed as pulli:
feed1 <- feed[feed$age==1,]
feed1 <- droplevels(feed1)
feed1$ring <- factor(feed1$ring)
levels(feed1$ring)
nlevels(feed1$ring)

# Number of different feeders visited by birds ringed as pulli:
feed_chicks <- tapply(feed1$LOC, feed1$ring, function(x) levels(factor(as.vector(x))))
feed_chicks
length(feed_chicks)

# Match CORT DOSE & CORT CONTROL:
cort_rings <- ring1$RING[grep('CORT DOSE', ring1$USERV4)]
cont_rings <- ring1$RING[grep('CORT CONT', ring1$USERV4)]
corts <- data.frame(ring=c(as.vector(cort_rings),as.vector(cont_rings)), treat=c(rep('DOSE',length(cort_rings)),rep('CONT',length(cont_rings)) ) )
feed_dosed <- data.frame(ring = names(feed_chicks), no_feeders=unlist(lapply(feed_chicks, length)))
feed_dosed$treat <- corts$treat[match(feed_dosed$ring, corts$ring)]
feed_dosed <- feed_dosed[order(feed_dosed$treat),]
# Add latest date seen for each
last_chick_dates <- as.Date(NA)
for (i in 1:length(feed_dosed$ring)) {
  last_chick_dates <- c(last_chick_dates, max(feed1$DATE[feed1$ring==feed_dosed$ring[i]],na.rm=T))
}
last_chick_dates <- last_chick_dates[2:length(last_chick_dates)]
feed_dosed$last_date <- last_chick_dates
rm(last_chick_dates)
feed_dosed
# Average no. feeders used between CORT and CONT birds:
tapply(feed_dosed$no_feeders, factor(feed_dosed$treat), mean)


### Number of resightings per PIT tagged bird in 2016 only:
pits_sum <- subset(ring1, select=c('RING','SPEC','DATE','SITE','USERV2'))
pits_sum$DATE <- as.Date(as.vector(pits_sum$DATE),'%d/%m/%Y %H:%M:%S')
no <- as.vector(NULL)
first <- as.vector(NULL)
last <- as.vector(NULL)
for(i in 1:nrow(pits_sum)) {
  no <- c(no, nrow(feed[which(feed$ring==as.vector(pits_sum$RING[i])),]))
  first <- c(first, format(min(feed[which(feed$ring==as.vector(pits_sum$RING[i])),'DATETIME'], na.rm=T), '%Y-%m-%d'))
  last <- c(last, format(max(feed[which(feed$ring==as.vector(pits_sum$RING[i])),'DATETIME'], na.rm=T), '%Y-%m-%d'))
}
out <- as.data.frame(cbind(no, first, last))
names(out) <- c('no','first','last')

pits_sum <- cbind(pits_sum, out)

pits_sum_2016 <- pits_sum[pits_sum$DATE>as.Date("2015-12-31",'%Y-%m-%d'),]
pits_sum_2016$no <- as.numeric(as.vector(pits_sum_2016$no))
pits_sum_2016 <- pits_sum_2016[pits_sum_2016$no>0,]
pits_sum_2016$first <- as.Date(as.vector(pits_sum_2016$first),'%Y-%m-%d')
pits_sum_2016$last <- as.Date(as.vector(pits_sum_2016$last),'%Y-%m-%d')
pits_sum_2016$nodays <- pits_sum_2016$last-pits_sum_2016$first

write.csv(pits_sum_2016, '2016/BFRM feeders/2016 PIT regs summary.csv', row.names=F)


### Resample full data set.
### Collapse registrations by "visit", defined as registrations >10 seconds apart.

feed$check <- FALSE
for (i in 1:(nrow(feed)-1)) {
  if ( (feed$ring[i+1]==feed$ring[i]) && (difftime(feed$DATETIME[i+1],feed$DATETIME[i],units='secs')<10) )  {
    feed$check[i+1] <- TRUE    
  }
}
feed_ss <- feed[!(feed$check),]

feed_ss_corts <- feed_ss[feed_ss$ring %in% corts$ring,]

corts_visits <- tapply(feed_ss_corts$DATETIME, list(feed_ss_corts$ring, feed_ss_corts$LOC), length)
corts_visits_p <- t(apply(corts_visits, 1, function(x) x/sum(x,na.rm=T)))
shannon <- function(x) {
  x_nrm <- x[!is.na(x)]
  return(-sum(x_nrm*log(x_nrm)))
}
shannon_scores <- as.vector(apply(corts_visits_p, 1, shannon))
corts_visits <- as.data.frame(corts_visits)
corts_visits$shannon <- shannon_scores
  
  


