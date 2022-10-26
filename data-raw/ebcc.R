#' ---
#' title: "Extract species-specific EBCC data"
#' ---

#' Data was downloaded from GBIF.org on the 15th of January 2021
#' The EBCC data file is located at t6p/group_hof/@BayKlif/data/EBCC
#' and the file is named: 0155958-200613084148143.csv

rm(list=ls()); gc()

# Set working directory
#filedir <- "/home/matt/t6p/group_hof/@BayKlif/data/EBCC/"
#filedir <- "Z:/group_hof/@BayKlif/data/EBCC/"
filedir <- "/home/matt/Documents/bavDC/extdata"

# Read data
library(vroom)
#dat <- vroom::vroom(paste0(filedir, "0155958-200613084148143.csv"))
dat <- vroom::vroom(paste0(filedir, "/0083342-210914110416597/occurrence.txt"))
dat[1:5, 1:5]

# Filter data by certain columns
library(dplyr); library(magrittr)
dat %<>% dplyr::select(species, occurrenceStatus, decimalLatitude, decimalLongitude) %>%
  tidyr::drop_na()
dat$occurrenceStatus <- factor(dat$occurrenceStatus, levels=c("ABSENT", "PRESENT"), labels=c(0,1))
dat$occurrenceStatus <- as.numeric(as.character(dat$occurrenceStatus))
dat %<>% group_by(decimalLongitude, decimalLatitude) %>% 
  tidyr::pivot_wider(names_from=species, values_from=occurrenceStatus, values_fn = last)
gc()

# Turn Bird data into Spatial Points
#sf_birds <- sf::st_as_sf(dat, coords = c("decimalLongitude", "decimalLatitude"), 
#                         crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

dat2 <- dat[,c("decimalLatitude","decimalLongitude","Asio flammeus")]
library(sp)
coordinates(dat2) <- ~decimalLongitude+decimalLatitude # set coordinates
raster::projection(dat2) <- CRS("+proj=longlat +datum=WGS84") # set projection
#dat2 <- sp::spTransform(dat2, "+proj=utm +zone=37 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#r_dat <- raster::rasterFromXYZ(dat2, res=c(50000,50000), digits=0,
#                      crs="+proj=utm +zone=37 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
dat2 <- sp::spTransform(dat2, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
r_dat <- raster::rasterFromXYZ(dat2, res=c(50000,50000), digits=0,
                               crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
plot(r_dat)
r_dat

# Only select points lying inside bavaria
library(sf)
data("bavaria", package="bavDC")
sf_ebcc_bav <- st_crop(sf_birds, st_buffer(bavaria, dist=20000))
plot(st_geometry(sf_ebcc_bav))
plot(st_geometry(bavaria), add=T)

ebcc_bav <- sf_ebcc_bav %>% as("Spatial") %>% as.data.frame()
colnames(ebcc_bav)[1:5]
colnames(ebcc_bav)[495:496]
colnames(ebcc_bav)[495:496] <- c("decimalLongitude", "decimalLatitude")
save(ebcc_bav, file="data/ebcc_bav.rda", compress="xz")

load("~/Documents/bavDC/data/ebcc_bav.rda")
# Convert data into a SpatialPointsDataFrame
library(sp); library(raster); library(dplyr)
ebcc_bav <- ebcc_bav %>% as.data.frame() %>% tidyr::drop_na()
#ebcc_bav$decimalLongitude <- round(ebcc_bav$decimalLongitude, digits=2)
#ebcc_bav$decimalLatitude <- round(ebcc_bav$decimalLatitude, digits=2)
unique(ebcc_bav$decimalLongitude)
unique(ebcc_bav$decimalLatitude)
ebcc_bav <- ebcc_bav[,c(495,496,20)]
coordinates(ebcc_bav) <- ~decimalLongitude+decimalLatitude # set coordinates
raster::projection(ebcc_bav) <- CRS("+proj=longlat +datum=WGS84") # set projection
ebcc_bav <- sp::spTransform(ebcc_bav, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
r_ebcc <- raster::rasterFromXYZ(ebcc_bav, digits=0, res=c(53000, 53000), 
                                crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
r_ebcc
x11(); plot(r_ebcc)

gridded(ebcc_bav) <- TRUE


ebcc_bav <- as.data.frame(ebcc_bav, xy=T) %>% 
  dplyr::select(decimalLongitude, decimalLatitude, `Vanellus.spinosus`, 
                `Vanellus.gregarius`, `Vanellus.leucurus`) %>% 
  mutate_at(names(.), ~as.numeric(unlist(.)))
colnames(ebcc_bav)
head(ebcc_bav)
str(ebcc_bav)

raster::rasterFromXYZ(ebcc_bav, )
