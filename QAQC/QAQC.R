library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)
library(tidyr)
##stringr may not be needed anymore - test without installing to see if code still runs; delete library(stringr) if so
##library(stringr)

# set wd
# Orla
setwd("~/Documents/R_Work_Directory/NEA_PostFlight/QAQC")

# Katherine
# setwd("~/R_Work_Directory/NEA_PostFlight/QAQC")

# Sharon
#setwd("C:/Users/shsu/Desktop/Rworkdirectory/NEA_PostFlight/QAQC")

# read in the data ####
filenames <- list.files(".//raw_data//", pattern="*RAW.csv")
date <- substr(filenames, 1, 8)
date <- max(date)

filename <- paste(".//raw_data//", date, "_RAW.csv", sep="")
dat <- read.csv(filename)

# Change time format ####
## Convert Mysticetus output to hh:mm:ss, remove the decimal at end of string, and remove colons 
dat$TrkTime..EDT. <- as_hms(ymd_hms(dat$TrkTime..EDT.))
dat$TrkTime..EDT. <- str_sub(dat$TrkTime..EDT., 1, str_length(dat$TrkTime..EDT.)-2)
dat$TrkTime..EDT. <- gsub(':', '', dat$TrkTime..EDT.)


# Column work: create and merge, then delete, re order, and rename ####

##merge rectype and rectypeEFFORT into one column and edits and effort_edits into one column
dat$rectype <- with(dat, pmax(rectype, rectypeEFFORT, na.rm = TRUE))
dat$edits <- with(dat, pmax(edits, effort_edits, na.rm = TRUE))

##create anglel and angler columns
dat$anglel <- ifelse(dat$rectype == "S", NA, dat$angle)
dat$angler <- ifelse(dat$rectype == "P", NA, dat$angle)

##remove original columns we dont want
##****do we want to remove TrkDist..nm from URI files? no we want to keep it and move it to far right column
dat <- subset(dat, select = -c(gpsspeed, HeadingPlatMagnetic..T., effort_edits, rectypeEFFORT, angle))

#rename columns
dat <- dat %>%
  rename("time" = "TrkTime..EDT.") %>%
  rename("lat" = "TrkLatitude") %>%
  rename("long" = "TrkLongitude") %>%
  rename("alt" = "TrkAltitude..m.") %>%
  rename("heading" = "HeadingPlatTrue..T.") %>%
  rename("gpsspeed" = "PlatformSpeed..kts.") %>%
  rename("TrackDist" = "TrkDist..nm.") %>%
  mutate(time = as.numeric(time))

##reorder columns 
dat <- dat[, c(21, 8:11, 1:3, 5, 4, 12:14, 20, 15:19, 22, 64:65, 23:44, 47:50, 6, 51:61, 46, 45, 62:63, 7)]

# Create legnos, delete events before and after survey ####

##assign eventnos
dat$eventno <- seq.int(nrow(dat))

##remove lines before first on-watch
dat1 <- dat %>% 
  filter(legtype == "1") %>%
  filter(time == min(time)) %>%
  mutate(time = as.numeric(time))

dat <- dat %>% 
  filter(time >= dat1$time)




# Auto fill effort, weather, alt, speed ####

##fill in legtype,legstage, legno, wx variables
dat[dat == ''] <- NA
dat <- dat %>% fill(legtype,legstage,legno,glarel,glarer,beaufort,cloud,wx,visiblty, .direction="down")

##remove lines after last off-watch
dat2 <- dat %>%
  filter(legtype == "1") %>%
  filter(time == max(time))

dat <- dat %>%
  filter(time < dat2$time)

##autofill setalt and setvel with 1000 feet and 100 knots
##****manually change values as needed
dat <- dat %>% 
  mutate(setalt = coalesce(setalt, 1500)) %>%
  mutate(setvel = coalesce(setvel, 100))


# A few misc things ####

##set number of decimal places for heading, alt, and gpsspeed columns
dat[,'heading']=round(dat[,'heading'],0)
dat[,'alt']=round(dat[,'alt'],0)
dat[,'gpsspeed']=round(dat[,'gpsspeed'],1)

# get rid of NA
dat[is.na(dat)] <- ""

# add in X rectypes
dat$rectype <- dat$rectype %>%
  replace(.=="", "X")


##write URI csv to output folder ####
write.csv(dat, paste0("output/", date, "_URI.csv"), row.names=FALSE)


## HERE ENDS THE DAY OF CHANGES NEEDED! ALL YE WHO WANT TO DO THE FIRST PASS CONTINUE PAST THIS LINE... AT YOUR PERIL... ####


# You can run this code if you did the day of stuff a while ago and want to load the URI without rerunning stuff ####

filenames <- list.files(".//output//", pattern="*URI.csv")
date <- substr(filenames, 1, 8)
date <- max(date)

filename <- paste(".//output//", date, "_URI.csv", sep="")
datURI <- read.csv(filename)

#datURI <- read.csv("C:/Users/oobrien/Documents/R_Work_Directory/NEA_PostFlight/data/20240225_URI.csv")
# delete data from the lunch break ####

datURI2 <- datURI %>%
  slice(which.max(legtype == "0") : n())

dat1 <- datURI2 %>%
  filter(legtype == "0") %>%
  mutate(max = max(eventno)) %>%
  mutate(min = min(eventno))

datURI <- datURI %>% 
  filter(eventno <= dat1$min | eventno > dat1$max)


datURI <- datURI %>%
  mutate(glarel = ifelse(legtype != 2, "", glarel)) 
datURI <- datURI %>%
  mutate(glarer = ifelse(legtype != 2, "", glarer)) 
datURI <- datURI %>%  
mutate(legno = ifelse(legtype ==3 |legtype ==1, "", legno))

# speed ####
dat1 <- datURI %>%
  filter(legtype == 2) %>%
  filter(gpsspeed > 120 | gpsspeed < 80)
print(dat1$eventno)

# altitude for 1000 feet
dat2 <- datURI %>%
  filter(legtype == 2) %>%
  filter(alt > 365 | alt < 244)
print(dat2$eventno)

# altitude for 1500 feet
dat2 <- datURI %>%
  filter(legtype == 2) %>%
  filter(alt > 518 | alt < 396)
print(dat2$eventno)

# heading for N-S lines
dat3 <- datURI %>%
  filter(legtype == 2) %>%
  filter(heading < 170 | heading > 190) %>% 
  filter(heading > 10) %>%
  filter(heading <350)
print(dat3$eventno)

# heading for N-S lines --- NEED TO ADJUST THESE HEADINGS
dat3 <- datURI %>%
  filter(legtype == 2) %>%
  filter(heading < 170 | heading > 190) %>% 
  filter(heading > 10) %>%
  filter(heading <350)
print(dat3$eventno)

# get rid of duplicate 2/1, 2/5, etc

datURI$legstage[datURI$legstage == '-'] <- 999
datURI$legstage <- as.numeric(datURI$legstage)
row <- as.numeric(nrow(datURI))

for (i in 1:(nrow(datURI))) {
  if (datURI$legtype[i] == 2 & datURI$legstage[i] == 1) { # only continue if an ON track position
    datURI$legtype[i+1] <- 2
    datURI$legtype[i+2] <- 2
    datURI$legtype[i+3] <- 2
    
    datURI$legstage[i+1] <- 2
    datURI$legstage[i+2] <- 2
    datURI$legstage[i+3] <- 2 }
  
  if(datURI$legtype[i] == 2 & datURI$legstage[i] == 3) { # only continue if a BREAK track position
    datURI$legtype[i+1] <- 4
    datURI$legtype[i+2] <- 4
    datURI$legtype[i+3] <- 4
    
    datURI$legstage[i+1] <- 999
    datURI$legstage[i+2] <- 999
    datURI$legstage[i+3] <- 999 }
  
  if(datURI$legtype[i] == 2 & datURI$legstage[i] == 4) { # only continue if a RESUME track position
    datURI$legtype[i+1] <- 2
    datURI$legtype[i+2] <- 2
    datURI$legtype[i+3] <- 2
    
    datURI$legstage[i+1] <- 2
    datURI$legstage[i+2] <- 2
    datURI$legstage[i+3] <- 2 }
  
  else {
    
    
  }
}

# assign sightnos 
dat4 <- datURI %>%
  filter(rectype == "S" | rectype == "P")
dat4 <- dat4 %>%
  mutate(sightno=row_number())
dat4 <- dat4[,c(5, 20)]
data_frame_merge <- merge(datURI, dat4, by = 'eventno', all = TRUE)
data_frame_merge <- data_frame_merge %>%
  mutate(sightno.x = sightno.y)
datURI <- data_frame_merge[,c(1:65)] %>%
  relocate(eventno, .after=year)

datURI <- datURI %>%
  rename("sightno" = "sightno.x")
datURI[is.na(datURI)] <- ""


datURI$legstage[datURI$legstage == 999] <- '-'

# 
# # and all this BS below is just for the 2/5s
# 
# l1 <- datURI %>%
#   filter(legstage != lag(legstage, 1)) %>%
#   filter(legstage != 1 & legstage != 2)
# 
# x <- l1$eventno
# l2 <- l1$legstage
# y <- matrix(nrow = length(x), ncol= 1, data = NA)
# #test <- datURI
# 
# for (i in 1:length(y)) {
#   y[i] <- which(datURI$eventno == x[i])
#   
# }
# 
# #test <- datURI
# idx <- seq(from = 1, to = length(y), by = 2)
# 
# for (i in idx){ #(i in 1:length(y.odd)){
#   if (datURI$eventno[(y[i]+1)] > datURI$eventno[(y[i+1]-1)]) {
#     print("Looks OK!")
#     
#   }
#   else{
#     datURI$legtype[(y[i]+1):(y[i+1]-1)] <- datURI$legtype[y[i+1]]
#     datURI$legstage[(y[i]+1):(y[i+1]-1)] <- datURI$legstage[y[i+1]]
#   }
#   end
# }

# save data file
write.csv(datURI, paste0("output/", date, "_URI.csv"), row.names=FALSE)





#### adding in PA Sightings ####

datURI <- read.csv("output/20230810_URI.csv")
datPA <- read.csv("output/20230810PA.csv")

datPA <- datPA %>%
  mutate(time = Time)
combine <- left_join(datURI, datPA, by = join_by(time))





































