## PMRF Feb 2020 Report:
## Kernel density estimation

## Michaela A. Kratofil
## Updated: 07 Oct 2020

##==================================================== ##

## load packages
library(tidyverse)
library(lubridate)

## do tursiops tags first 
tt <- read.csv("PMRF Feb 2020 Report/TtTags_Kauai_crawl_1hrStep_FixedPath_2020Sep28.csv")
tt$date <- ymd_hms(tt$date, tz = "UTC")
tt$minutes <- minute(tt$date)

## choose only predicted locations 
tt.pred <- tt %>%
  filter(minutes == 00)

## remove first day of records
sub <- tt.pred %>%
  group_by(animal) %>%
  slice(25:n()) %>%
  ungroup()

## remove TtTag019 (IC) and TtTag034 (pseudoreplicate)
sub <- sub %>%
  filter(animal != "TtTag019") %>%
  filter(animal != "TtTag034")

## thin locations so have a 12 hour interval
sub <- sub %>%
  group_by(animal) %>%
  slice(seq(2, n(), by = 12))

## now do kernel density estimation
library(adehabitatHR)
library(sp)
library(sf)
prj <- st_crs(3750)
wgs <- st_crs(4326)

xysp <- SpatialPointsDataFrame(sub, 
                               coords = cbind(sub$longitud, sub$latitude),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

xysp.utm <- spTransform(xysp, CRS("+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(xysp.utm)

## calculate kernel density/utility distribution
ud <- kernelUD(xysp.utm, h = "href", kern = "bivnorm", grid = 1000)
ud
ud.df <- as.data.frame(ud)
image(ud)
#plotLSCV(ud)

## 95% home range
hr95 <- getverticeshr(ud, percent = 95, unout = "km2")
plot(hr95)
hr95.df <- as.data.frame(hr95)
hr95.wgs <- spTransform(hr95, CRS("+proj=longlat +datum=WGS84 +no_defs"))
hr95.wgs <- as(hr95.wgs, "sf")

## 99% home range
hr99 <- getverticeshr(ud, percent = 99, unout = "km2")
plot(hr99)
hr99.df <- as.data.frame(hr99)
hr99.wgs <- spTransform(hr99, CRS("+proj=longlat +datum=WGS84 +no_defs"))
hr99.wgs <- as(hr99.wgs, "sf")

## 50% home range
hr50 <- getverticeshr(ud, percent = 50, unout = "km2")
plot(hr50)
hr50.df <- as.data.frame(hr50)
hr50.wgs <- spTransform(hr50, CRS("+proj=longlat +datum=WGS84 +no_defs"))
hr50.wgs <- as(hr50.wgs, "sf")

## write polygon
library(rgdal)
st_write(hr99.wgs, "PMRF Feb 2020 Report/KMZs/Kauai_TtTags_12hr_no19_HR_99CI_polygon.kml", driver = "kml")

tt.12hr.n19.hr50 <- hr50
tt.12hr.n19.hr95 <- hr95
tt.12hr.n19.hr99 <- hr99

## maps 
pmrf <- readOGR("Shapefiles", "PMRF_UTM4N", verbose=F) %>%
  as(., 'sf')
pmrf <- st_transform(pmrf, 3750)


## make polygons as sf objects and make sure crs is set correctly
hr50.sf <- as(tt.12hr.n19.hr50, "sf") %>%
  st_transform(3750)

hr95.sf <- as(tt.12hr.w19.hr95, "sf") %>%
  st_transform(3750)

hr99.sf <- as(tt.12hr.w19.hr99, "sf") %>%
  st_transform(3750)

## get coastline shapfile
coast <- readOGR("Shapefiles", "Coastline", verbose = F) %>%
  as(., 'sf') %>%
  st_transform(crs = 3750)

library(ggplot2)
library(ggspatial)

theme_map <- function() {
  theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black', size = 1.25),
          axis.text = element_text(colour = 'black', size = 12),
          plot.title = element_text(colour = 'black', face = 'bold')) #+
  
}

hr99.coords <- as.data.frame(st_coordinates(hr99.sf))

# Ocean basemap if want
esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# for map island labels:
island <- c(paste0("Kaua\u02BBi"), paste0("Ni\u02BBihau"), paste0("O\u02BBahu"),
            paste0("Moloka\u02BBi"), "Maui", paste0("L\u0101na\u02BBi"), paste0("Kaho\u02BBolawe"),
            paste0("Hawai\u02BBi"))

# coordinates for map labels
lat <- c(22.05, 21.999, 21.517, 21.310436, 20.749506, 20.681225, 20.439420, 19.591887)
lon <- c(-159.534170, -160.26, -158.040082, -156.992012, -156.257611, -156.946387, -156.626687, -155.535081)

# make into dataframe and project
lon.df <- as.data.frame(lon)
labs <- cbind(island, lat)
labs.df <- as.data.frame(labs)
labs.df <- bind_cols(labs.df, lon.df)
labs.sf <- st_as_sf(labs.df, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = 3750)

labs.coords <- as.data.frame(st_coordinates(labs.sf)) %>%
  bind_cols(., labs.df)

ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = "none") +
  geom_sf(data = hr99.sf, fill = "goldenrod", color = "black", alpha = 0.4) +
  geom_sf(data = hr95.sf, fill = "orange", color = "black", alpha = 0.4) +
  geom_sf(data = hr50.sf, fill = "red", color = "black", alpha = 0.5) +
  geom_sf(data = coast) +
  geom_text(data = labs.coords, aes(X,Y, label = island, fontface = "bold"), size = 4.5) +
  geom_sf(data = pmrf, color = "red", size = 1.5, fill = NA) +
  coord_sf(crs = 3750,
           xlim = c(min(hr99.coords$X) - 30000, max(hr99.coords$X) + 10000),
           ylim = c(min(hr99.coords$Y) - 10000, max(hr99.coords$Y) + 10000)) +
  theme_map() +
  annotation_scale(location = 'bl', text_cex = .9) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                       style = north_arrow_fancy_orienteering(),
                       height = unit(2, "cm"),
                       width = unit(2, "cm")) +
  xlab("") +
  ylab("")

ggsave("PMRF Feb 2020 Report/Maps/Kauai_TtTags_KDE_12hr_wTtTag019_Map.jpg", width = 10, height = 8, units = "in")

## calculate percent overlap of core area (50%) with PMRF
int <- as_tibble(st_intersection(pmrf, hr50.sf))
int$areaOv <- st_area(int$geometry)
plot(int$geometry)

## ============================================================================== ##

## now do pilot whales  
gm <- read.csv("PMRF Feb 2020 Report/Kauai_All_GmTags_crawl_1hrStep_FixedPath_2020Oct06.csv")
gm$date <- ymd_hms(gm$date, tz = "UTC")
gm$minutes <- minute(gm$date)

## choose only predicted locations 
gm.pred <- gm %>%
  filter(minutes == 00)

## remove first day of records
sub <- gm.pred %>%
  group_by(animal) %>%
  slice(25:n()) %>%
  ungroup()

## filter western resident community IDs, and remove pseudoreplicates
west.tags <- c(
  "GmTag013",
  "GmTag049",
  "GmTag050",
  "GmTag051",
  "GmTag063",
  "GmTag064",
  "GmTag070",
  "GmTag079",
  "Gmtag080",
  "GmTag081",
  "GmTag083",
  "GmTag114",
  "GmTag115",
  "GmTag132",
  "GmTag133",
  "GmTag155",
  "GmTag156",
  "GmTag214",
  "GmTag231"
)

sub <- sub %>%
  filter(animal %in% west.tags) 

## thin locations so have a 12 hour interval
sub <- sub %>%
  group_by(animal) %>%
  slice(seq(2, n(), by = 12))

## now do kernel density estimation
xysp <- SpatialPointsDataFrame(sub, 
                               coords = cbind(sub$longitud, sub$latitude),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

xysp.utm <- spTransform(xysp, CRS("+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(xysp.utm)

## calculate kernel density/utility distribution
ud <- kernelUD(xysp.utm, h = "href", kern = "bivnorm", grid = 1000)
ud
ud.df <- as.data.frame(ud)
image(ud)
#plotLSCV(ud)

## 95% home range
hr95 <- getverticeshr(ud, percent = 95, unout = "km2")
plot(hr95)
hr95.df <- as.data.frame(hr95)
hr95.wgs <- spTransform(hr95, CRS("+proj=longlat +datum=WGS84 +no_defs"))
hr95.wgs <- as(hr95.wgs, "sf")

## 99% home range
hr99 <- getverticeshr(ud, percent = 99, unout = "km2")
plot(hr99)
hr99.df <- as.data.frame(hr99)
hr99.wgs <- spTransform(hr99, CRS("+proj=longlat +datum=WGS84 +no_defs"))
hr99.wgs <- as(hr99.wgs, "sf")

## 50% home range
hr50 <- getverticeshr(ud, percent = 50, unout = "km2")
plot(hr50)
hr50.df <- as.data.frame(hr50)
hr50.wgs <- spTransform(hr50, CRS("+proj=longlat +datum=WGS84 +no_defs"))
hr50.wgs <- as(hr50.wgs, "sf")

## write polygon
st_write(hr50.wgs, "PMRF Feb 2020 Report/KMZs/Kauai_GmTags_WestComm_12hr_HR_50CI_polygon.kml", driver = "kml")


## make polygons as sf objects and make sure crs is set correctly
hr50.sf <- as(hr50, "sf") %>%
  st_transform(3750)

hr95.sf <- as(hr95, "sf") %>%
  st_transform(3750)

hr99.sf <- as(hr99, "sf") %>%
  st_transform(3750)

hr99.coords <- as.data.frame(st_coordinates(hr99.sf))

ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = "none") +
  geom_sf(data = hr99.sf, fill = "goldenrod", color = "black", alpha = 0.4) +
  geom_sf(data = hr95.sf, fill = "orange", color = "black", alpha = 0.4) +
  geom_sf(data = hr50.sf, fill = "red", color = "black", alpha = 0.5) +
  geom_sf(data = coast) +
  geom_text(data = labs.coords, aes(X,Y, label = island, fontface = "bold"), size = 4) +
  geom_sf(data = pmrf, color = "red", size = 1.5, fill = NA) +
  coord_sf(crs = 3750,
           xlim = c(min(hr99.coords$X) - 30000, max(hr99.coords$X) + 10000),
           ylim = c(min(hr99.coords$Y) - 10000, max(hr99.coords$Y) + 10000)) +
  theme_map() +
  annotation_scale(location = 'bl', text_cex = .9) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering(),
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) +
  xlab("") +
  ylab("")

ggsave("PMRF Feb 2020 Report/Maps/Kauai_GmTags_WestComm_KDE_12hr_Map.jpg", width = 10, height = 8, units = "in")

## calculate percent overlap of core area (50%) with PMRF
int <- as_tibble(st_intersection(pmrf, hr50.sf))
int$areaOv <- st_area(int$geometry)
plot(int$geometry)
