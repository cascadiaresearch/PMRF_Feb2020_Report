## PMRF Feb 2020 Report:
## Proportion of time spent in PMRF Range calcs

## Michaela A. Kratofil
## Updated 07 OCT 2020

## ================================================= ##
library(tidyverse)
library(lubridate)

## Tursiops tags 
tt <- read.csv("GIS output/TtTags_Kauai_crawl_1hrStep_FixedPath_GIS_20200928.csv", header = T)
tt$date <- as.POSIXct(tt$date, tz = "UTC")
tt.sum <- tt %>%
  group_by(animal) %>%
  tally()
tt.space <- tt %>%
  filter(DistToShore > 0) %>%
  group_by(animal) %>%
  summarise(
    medDistToShore = median(DistToShore),
    maxDistToShore = max(DistToShore),
    medDepth = median(abs(depth)),
    maxDepth = max(abs(depth))
  )
write.csv(tt.space, "PMRF Feb 2020 Report/Kauai_TtTags_SpaceUse_Sum.csv", row.names = F)

tt.pmrf <- tt %>%
  filter(PMRF == T) %>%
  group_by(animal) %>%
  tally()

tt.pmrf.locs <- filter(tt, PMRF == T)
tt.pmrf.sum <- tt %>%
  #filter(PMRF == T) %>%
  group_by(animal) %>%
  summarise(start = first(date),
            end = last(date),
            days = as.numeric(difftime(end, start, units = "days")),
            hours = as.numeric(difftime(end, start, units = "hours")))

tt$minutes <- minute(tt$date)
tt.pred <- filter(tt, minutes == 0)
tt.sum <- tt.pred %>%
  group_by(animal) %>%
  tally()
tt.space <- tt.pred %>%
  filter(DistToShore > 0) %>%
  group_by(animal) %>%
  summarise(
    medDistToShore = median(DistToShore),
    maxDistToShore = max(DistToShore),
    medDepth = median(abs(depth)),
    maxDepth = max(abs(depth))
  )
write.csv(tt.space, "PMRF Feb 2020 Report/Kauai_TtTags_SpaceUse_Sum.csv", row.names = F)

tt.pmrf <- tt.pred %>%
  filter(PMRF == T) %>%
  group_by(animal) %>%
  tally()

tt.pmrf.locs <- filter(tt.pred, PMRF == T)
tt.pmrf.sum <- tt.pred %>%
  #filter(PMRF == T) %>%
  group_by(animal) %>%
  summarise(start = first(date),
            end = last(date),
            days = as.numeric(difftime(end, start, units = "days")),
            hours = as.numeric(difftime(end, start, units = "hours")))

## pilot whales 
gm <- read.csv("PMRF Feb 2020 Report/Kauai_All_GmTags_ArgosGPS_crawl_1hrStep_FixedPath_GIS_20201006.csv")
west <- filter(gm, community == "Western")
unique(west$animal)
length(unique(west$animal))

gm$date <- as.POSIXct(gm$date, tz = "UTC")
gm$minutes <- minute(gm$date)
gm.pred <- filter(gm, minutes == 0)

gm.sum <- gm.pred %>%
  group_by(animal) %>%
  tally()

gm.space <- gm.pred %>%
  filter(DistToShore > 0) %>%
  group_by(animal) %>%
  summarise(
    medDistToShore = median(DistToShore),
    maxDistToShore = max(DistToShore),
    medDepth = median(abs(depth)),
    maxDepth = max(abs(depth))
  )
write.csv(gm.space, "PMRF Feb 2020 Report/Kauai_GmTags_SpaceUse_Sum.csv", row.names = F)

gm.pmrf <- gm.pred %>%
  filter(PMRF == T) %>%
  group_by(animal) %>%
  tally()
