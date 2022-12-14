#' ---
#' title: "Create CCI data for Bavaria"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()

## CCI Land-cover data

# Information on the data can be found here:
# http://maps.elie.ucl.ac.be/CCI/viewer/download.php

# Data for 1992 - 2015 ist available from:
# ftp://geo10.elie.ucl.ac.be/CCI/LandCover/byYear/

# Data for 2016-2020 is available from:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-land-cover?tab=form

# Specify file directory
filedir <- "/home/matt/Documents/LC_CCI/"

# Define filenames
years <- 1992:2020
#filenames <- paste0("ESACCI-LC-L4-LCCS-Map-300m-P1Y-", years, "-v2.0.7.nc")

# Download files
#lapply(filenames, function(x){download.file(paste0("", x), destfile=paste0(filedir, x))})

# Load files
files <- c(list.files(filedir, pattern=".tif", full.names=T), 
           list.files(filedir, pattern=".nc", full.names=T))
dat <- raster::stack(files)

# Crop by extent of bavaria
load("data/bavaria.rda")
r_cci_bav <- raster::mask(raster::crop(dat, bavaria, snap="out"), sf::st_buffer(bavaria, 10000))
names(r_cci_bav) <- years
r_cci_bav
raster::plot(r_cci_bav[[1]])
cci_bav <- raster::mask(r_cci_bav, bavaria)
raster::plot(cci_bav[[1]])
cci_bav
rm(dat); gc()

# Save to file
library(dplyr)
round10 <- function(x, na.rm = FALSE) round(x/10, digits=0)*10
cci_bav <- as.data.frame(raster::rasterToPoints(cci_bav)) %>% 
  mutate_at(vars(-c(x,y)), round10) %>%
  mutate_at(vars(-c(x,y)), factor, 
            levels = seq(0,220, by=10), 
            labels=c("No Data", "Cropland, rainfed", "Cropland, irrigated or post-flooding",
                     "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)",
                     "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)",
                     "Tree cover, broadleaved, evergreen, closed to open (>15%)",
                     "Tree cover, broadleaved, deciduous, closed to open (>15%)",
                     "Tree cover, needleleaved, evergreen, closed to open (>15%)",
                     "Tree cover, needleleaved, deciduous, closed to open (>15%)",
                     "Tree cover, mixed leaf type (broadleaved and needleleaved)",
                     "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
                     "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)", "Shrubland", "Grassland", "Lichens and mosses", 
                     "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", "Tree cover, flooded, fresh or brakish water", 
                     "Tree cover, flooded, saline water", "Shrub or herbaceous cover, flooded, fresh/saline/brakish water",
                     "Urban areas", "Bare areas", "Water bodies", "Permanent snow and ice"))
colnames(cci_bav) <- c("x", "y", years)
head(cci_bav)
save(cci_bav, file="data/cci_bav.rda", compress="xz")

# Re-sample to TK25
load("data/tk4tel_grid.rda")
tk4tel_r <- raster::rasterFromXYZ(tk4tel_grid)
(cci_bav_gk <- raster::projectRaster(r_cci_bav, crs=sp::CRS("+init=epsg:31468")))
cci_bav_tk4tel <- raster::resample(cci_bav_gk, tk4tel_r, method="ngb")
cci_bav_tk4tel <- raster::mask(cci_bav_tk4tel, tk4tel_r)
cci_bav_tk4tel <- as.data.frame(raster::rasterToPoints(cci_bav_tk4tel))
colnames(cci_bav_tk4tel) <- c("x", "y", years)
head(cci_bav_tk4tel)

# Define categories
library(dplyr)
round10 <- function(x, na.rm = FALSE) round(x/10, digits=0)*10
cci_bav_tk4tel <- cci_bav_tk4tel %>% mutate_at(vars(-c(x,y)), round10) %>%
  mutate_at(vars(-c(x,y)), factor, levels = seq(0,220, by=10), 
            labels=c("No Data", "Cropland, rainfed", "Cropland, irrigated or post-flooding",
                     "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)",
                     "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)",
                     "Tree cover, broadleaved, evergreen, closed to open (>15%)",
                     "Tree cover, broadleaved, deciduous, closed to open (>15%)",
                     "Tree cover, needleleaved, evergreen, closed to open (>15%)",
                     "Tree cover, needleleaved, deciduous, closed to open (>15%)",
                     "Tree cover, mixed leaf type (broadleaved and needleleaved)",
                     "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
                     "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)", "Shrubland", "Grassland", "Lichens and mosses", 
                     "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", "Tree cover, flooded, fresh or brakish water", 
                     "Tree cover, flooded, saline water", "Shrub or herbaceous cover, flooded, fresh/saline/brakish water",
                     "Urban areas", "Bare areas", "Water bodies", "Permanent snow and ice"))

# Save to file
save(cci_bav_tk4tel, file="data/cci_bav_tk4tel.rda", compress="xz")
