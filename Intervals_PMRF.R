## PMRF Feb 2020 Report:
## Calculate intervals b/t locations

## Michaela A. Kratofil
## 05 OCT 2020

#######################################################

# load packages
library(tidyverse)
library(lubridate)

# read in location data
locs <- read.csv('Douglas Filtered/TtTag002-035_DouglasFiltered_KS_r20d3lc2_2020MAYv2.csv', header = T)
locs <- read.csv("Fastloc-GPS/Raw files/GmTag231-180166-1-FastGPS.csv")
# review data
str(locs)
summary(locs)

locs$date <- as.POSIXct(locs$date, tz = 'UTC') # format date
locs$animal <- as.factor(locs$animal) # make animal/tag a factor

## Take each tag through one at a time; easier to check for inconsistencies ##
# subset out 1 tag 
sub <- filter(locs, animal %in% c("TtTag034", "TtTag035"))
sub <- locs
l <- bind_rows(g231, sub) %>%
  arrange(animal, date)
write.csv(l, "PMRF Feb 2020 Report/KauaiFeb2020_GmTtTags_DouglasFiltered_Argos_2020Oct05.csv", row.names = F)

# calculate delta t (change in time)
sub$deltaT <- as.numeric(NA) # deltaT
sub$deltaT_r <- as.numeric(NA) # rounded deltaT

for (i in 1:nrow(sub)) {
  
  Start = sub[i, "date"]
  End = sub[i + 1, "date"]
  
  sub[i, "deltaT"] = difftime(End, Start, units = 'hours') 
  sub[i, "deltaT_r"] = round(difftime(End, Start, units = 'hours'), 0) # round digits
  
  
}

sub.nons <- filter(sub, !is.na(deltaT))
median(sub.nons$deltaT)
max(sub.nons$deltaT)
