#' ECAD/E-OBS Data for Bavaria

#' Ensemble version:
#' "We acknowledge the E-OBS dataset from the EU-FP6 project UERRA (http://www.uerra.eu) and 
#' the Copernicus Climate Change Service, and the data providers in the ECA&D project (https://www.ecad.eu)"

#' "Cornes, R., G. van der Schrier, E.J.M. van den Besselaar, and P.D. Jones. 2018: 
#' An Ensemble Version of the E-OBS Temperature and Precipitation Datasets, J. Geophys. Res. Atmos., 123. doi:10.1029/2017JD028200"Website

#' E-OBS Data used here was downloaded from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=form
#' but other data can also be downloaded from here: http://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php

#' Some first climate indices data can be downloaded from: https://www.ecad.eu/download/millennium/millennium.php
#' but rather use the more detailed output from here: http://surfobs.climate.copernicus.eu/dataaccess/access_eobs_indices.php
#' 

rm(list=ls()); gc()

# Load processNC package
library(processNC)

# Load other packages
library(terra)

# Set file directory
setwd("/home/matt/Documents/bdc")

# Set file directory
filedir <- "/home/matt/Documents/e-obs"

# Load europe outline
load("data/bavaria.rda")

# List files
files <- list.files(filedir, pattern=".nc", full.names=T)

# Load, crop & mask elevation data
elev <- terra::rast(files[grep(files, pattern="elev")])
elev <- terra::crop(elev, vect(sf::st_as_sf(bavaria)), snap="out", mask=T)
plot(elev); gc()
plot(sf::st_geometry(bavaria), add=T)
writeCDF(elev, filename=paste0("rawdata/", sub("reg_v23.1e.nc", "", 
                                               basename(files[grep(files, pattern="elev")])), 
                               "bav.nc"), overwrite=T)

# Remove elevation data from file list
files <- files[-grep(files, pattern="elev")]

# Load and summarise climate data
name <- c("pr", "pr", "tas", "tas", "tasmin", "tasmin", "tasmax", "tasmax")
lapply(1:length(files), function(z){
  if(!file.exists(paste0("rawdata/", sub("reg_v23.1e.nc", "", basename(files[z])), 
                         "bav_yearmon.nc"))){
    #dat <- summariseNC(files[z], ext=vect(sf::st_as_sf(bavaria)), cores=7,
    #                   group_col="month", name=name[y], startdate=1950, enddate=2020)
    years <- seq(1950,2020, by=10)
    dat <- lapply(years, function(y){
      temp <- tempfile(fileext=".nc")
      if(y == 2020){
        aggregateNC(files[z], outfile=temp, group_col="yearmon", var=name[z], startdate=y, enddate=y)
      } else{
        aggregateNC(files[z], outfile=temp, group_col="yearmon", var=name[z], startdate=y, enddate=y+9)
      }
      temp2 <- terra::rast(temp); rm(temp)
      terra::crop(temp2, vect(sf::st_as_sf(bavaria)), snap="out", mask=T)
    }); gc()
    dat <- terra::rast(dat); gc()
    writeCDF(dat, filename=paste0("rawdata/", sub("reg_v23.1e.nc", "", basename(files[z])), 
                                  "bav_yearmon.nc")); rm(dat); gc() 
  }
})

library(terra)
dat1 <- terra::rast("rawdata/elev_ens_0.1deg_bav.nc")
plot(dat1)
dat1_df <- as.data.frame(dat1, xy=T)
colnames(dat1_df)
assign(paste0("e-obs_", sub(".nc", "", basename("rawdata/elev_ens_0.1deg_bav.nc"))), value=dat1_df)
save(list=paste0("e-obs_", sub(".nc", "", basename("rawdata/elev_ens_0.1deg_bav.nc"))), 
     file=paste0("data/e-obs_", sub(".nc", ".rda", basename("rawdata/elev_ens_0.1deg_bav.nc"))), compress="xz")

files <- list.files("rawdata/", pattern="bav_yearmon.nc", full.names=T)
lapply(1:length(files), function(z){
  dat <- terra::rast(files[z])
  dat <- crop(dat, dat1)
  dat <- dat[[terra::time(dat) < "2021-01-15 UTC"]]
  dat_df <- as.data.frame(dat, xy=T)
  colnames(dat_df)[3:ncol(dat_df)] <- as.character(zoo::as.yearmon(terra::time(dat)))
  assign(paste0("e-obs_",  sub(".nc", "", basename(files[z]))), value=dat_df)
  save(list=paste0("e-obs_",  sub(".nc", "", basename(files[z]))), 
       file=paste0("data/e-obs_",  sub(".nc", ".rda", basename(files[z]))), compress="xz")
})

## Calculate bioclimatic variables

rm(list=ls()); gc()

# Load data
load("data/e-obs_rr_ens_mean_0.1deg_bav_yearmon.rda")
load("data/e-obs_tn_ens_mean_0.1deg_bav_yearmon.rda")
load("data/e-obs_tx_ens_mean_0.1deg_bav_yearmon.rda")

rr_1969_1999 <- `e-obs_rr_ens_mean_0.1deg_bav_yearmon` %>%
  pivot_longer(names_to="yearmon", values_to="val", -c(x,y)) %>% 
  mutate(yr = substr(yearmon,5,8), mon = substr(yearmon,1,3)) %>%
  filter(yr %in% c(1969:1999)) %>% select(-c(yearmon, yr)) %>% 
  group_by(x, y, mon) %>% summarise(mn=mean(val))
tn_1969_1999 <- `e-obs_tn_ens_mean_0.1deg_bav_yearmon` %>%
  pivot_longer(names_to="yearmon", values_to="val", -c(x,y)) %>% 
  mutate(yr = substr(yearmon,5,8), mon = substr(yearmon,1,3)) %>%
  filter(yr %in% c(1969:1999))  %>% select(-c(yearmon, yr)) %>% 
  group_by(x, y, mon) %>% summarise(mn=mean(val))
tx_1969_1999 <- `e-obs_tx_ens_mean_0.1deg_bav_yearmon` %>%
  pivot_longer(names_to="yearmon", values_to="val", -c(x,y)) %>% 
  mutate(yr = substr(yearmon,5,8), mon = substr(yearmon,1,3)) %>%
  filter(yr %in% c(1969:1999)) %>% select(-c(yearmon, yr)) %>% 
  group_by(x, y, mon) %>% summarise(mn=mean(val))

prec <- rr_1969_1999 %>% group_by(x,y) %>% tidyr::spread(mon, mn)
prec_xy <- prec %>% ungroup() %>% dplyr::select(c(x,y))
prec <- prec %>% ungroup() %>% dplyr::select(-c(x,y))
tasmin <- tn_1969_1999 %>% group_by(x,y) %>% tidyr::spread(mon, mn) %>% ungroup() %>% dplyr::select(-c(x,y))
tasmax <- tx_1969_1999 %>% group_by(x,y) %>% tidyr::spread(mon, mn) %>% ungroup() %>% dplyr::select(-c(x,y))
`e-obs_bioclim_1969_1999_bav` <- dismo::biovars(as.matrix(prec), as.matrix(tasmin), as.matrix(tasmax)) %>%
  as.data.frame()
rm(prec, tasmin, tasmax); invisible(gc())
`e-obs_bioclim_1969_1999_bav`$x <- prec_xy$x
`e-obs_bioclim_1969_1999_bav`$y <- prec_xy$y
head(`e-obs_bioclim_1969_1999_bav`)
summary(`e-obs_bioclim_1969_1999_bav`)

#' Save to file
save(list="e-obs_bioclim_1969_1999_bav", file="data/e-obs_bioclim_1969_1999_bav.rda", compress="xz")
rm(`e-obs_bioclim_1969_1999_bav`); gc()

rr_1979_2009 <- `e-obs_rr_ens_mean_0.1deg_bav_yearmon` %>%
  pivot_longer(names_to="yearmon", values_to="val", -c(x,y)) %>% 
  mutate(yr = substr(yearmon,5,8), mon = substr(yearmon,1,3)) %>%
  filter(yr %in% c(1979:2009)) %>% select(-c(yearmon, yr)) %>% 
  group_by(x, y, mon) %>% summarise(mn=mean(val))
tn_1979_2009 <- `e-obs_tn_ens_mean_0.1deg_bav_yearmon` %>%
  pivot_longer(names_to="yearmon", values_to="val", -c(x,y)) %>% 
  mutate(yr = substr(yearmon,5,8), mon = substr(yearmon,1,3)) %>%
  filter(yr %in% c(1979:2009))  %>% select(-c(yearmon, yr)) %>% 
  group_by(x, y, mon) %>% summarise(mn=mean(val))
tx_1979_2009 <- `e-obs_tx_ens_mean_0.1deg_bav_yearmon` %>%
  pivot_longer(names_to="yearmon", values_to="val", -c(x,y)) %>% 
  mutate(yr = substr(yearmon,5,8), mon = substr(yearmon,1,3)) %>%
  filter(yr %in% c(1979:2009)) %>% select(-c(yearmon, yr)) %>% 
  group_by(x, y, mon) %>% summarise(mn=mean(val))

prec <- rr_1979_2009 %>% group_by(x,y) %>% tidyr::spread(mon, mn)
prec_xy <- prec %>% ungroup() %>% dplyr::select(c(x,y))
prec <- prec %>% ungroup() %>% dplyr::select(-c(x,y))
tasmin <- tn_1979_2009 %>% group_by(x,y) %>% tidyr::spread(mon, mn) %>% ungroup() %>% dplyr::select(-c(x,y))
tasmax <- tx_1979_2009 %>% group_by(x,y) %>% tidyr::spread(mon, mn) %>% ungroup() %>% dplyr::select(-c(x,y))
`e-obs_bioclim_1979_2009_bav` <- dismo::biovars(as.matrix(prec), as.matrix(tasmin), as.matrix(tasmax)) %>%
  as.data.frame()
rm(prec, tasmin, tasmax); invisible(gc())
`e-obs_bioclim_1979_2009_bav`$x <- prec_xy$x
`e-obs_bioclim_1979_2009_bav`$y <- prec_xy$y
head(`e-obs_bioclim_1979_2009_bav`)
summary(`e-obs_bioclim_1979_2009_bav`)

#' Save to file
save(list="e-obs_bioclim_1979_2009_bav", file="data/e-obs_bioclim_1979_2009_bav.rda", compress="xz")
rm(`e-obs_bioclim_1979_2009_bav`); gc()
