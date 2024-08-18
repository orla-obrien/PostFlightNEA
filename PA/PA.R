setwd("~/Documents/R_Work_Directory/NEA_PostFlight/PA")


library(tidyverse)
library(data.table)
#pacman::p_load(bayesbio)
#library(bayesbio)
library(lubridate)
library(fuzzyjoin)

URI <- read.csv("data/20231106_URI.csv")
PA <- read.csv("data/20231106_PA.csv")

URI <- as.data.table(URI)
PA <- as.data.table(PA)

PAselect <- PA %>%
  dplyr::select(Time:CONF, PASight)

##change column to match URI
PAselect <- PAselect %>%
  rename("time.y"="Time") %>%
  rename("idrel.y"="IDREL") %>%
  rename("confidnc.y"="CONF") %>%
  rename("number.y"="Number") %>%
  rename("speccode.y"="SPEC.CODE") %>%
  mutate(time.y=as.numeric(time.y)) %>%
  mutate(numcalf.y="") %>%
  mutate(anhead.y="") %>%
  mutate(photos.y="2")
  

PAselect <- PAselect %>% 
  #mutate(!!!setNames(rep(NA, length(collist)), collist)) %>%
  filter(!is.na(time.y))
PAselect <- PAselect %>% 
  arrange(time.y) %>%
  mutate(sightno.y=seq(501, length.out = nrow(PAselect)))

PAselect <- PAselect[,c(1, 10, 6, 2:3, 7:9, 4:5)]




URI$time <- as.numeric(URI$time)

combinedfile <- difference_join(URI, PAselect, by = c("time" = "time.y"), max_dist = 2, mode = "full", 
                         distance_col = "distance_col")

combinedfile[is.na(combinedfile)] <- ""
write.csv(combinedfile, paste0("output/combined_test.csv"), row.names=FALSE)


