# Altitude data for Bavaria

# EU-DEM & EU-DEM Hillshade Data was downloaded from: 
#https://ec.europa.eu/eurostat/de/web/gisco/geodata/reference-data/elevation/eu-dem/eu-dem-laea
#https://ec.europa.eu/eurostat/de/web/gisco/geodata/reference-data/elevation/eu-dem/hillshade

# Set working directory
setwd("C:/Users/Admin/Documents/bavDC")

# Load packages
library(terra); library(dplyr)
library(sp); library(sf)

load("data/bavaria.rda")

####################

## Load hillshade data
if(!file.exists("data/hillshade_bav_500m.rda")){
  if(!file.exists("extdata/hillshade_eur.tif")){
    filedir <- "extdata/EU_DEM_Mosaic_hillshade/"
    file <- list.files(filedir, pattern=".tif$", full.names=T)
    hillshade_eur <- rast(file)
    
    # Crop & mask data
    europe_laea <- st_transform(europe, crs=crs(hillshade_eur))
    europe_laea <- vect(europe_laea)
    hillshade_eur <- crop(hillshade_eur,europe_laea); invisible(gc())
    #hillshade_eur <- mask(hillshade_eur, europe_laea); invisible(gc())
    writeRaster(hillshade_eur, filename="extdata/hillshade_eur.tif"); rm(hillshade_eur); invisible(gc())
  } else{
    hillshade_eur <- terra::rast("extdata/hillshade_eur.tif")
    # Transform europe
    bavaria_laea <- st_transform(bavaria, crs=crs(hillshade_eur))
  }

  hillshade_bav <- mask(crop(hillshade_eur, vect(bavaria_laea)), vect(bavaria_laea)); rm(hillshade_eur); gc()
  #plot(hillshade_bav[[1]])
  #plot(bavaria_laea, add=T)
  hillshade_bav
  
  # Turn into 500 m raster
  hillshade_bav_mean <- aggregate(hillshade_bav, fact=20, fun=mean, na.rm=T)
  hillshade_bav_median <- aggregate(hillshade_bav, fact=20, fun=median, na.rm=T)
  hillshade_bav_min <- aggregate(hillshade_bav, fact=20, fun=min, na.rm=T)
  hillshade_bav_max <- aggregate(hillshade_bav, fact=20, fun=max, na.rm=T)
  hillshade_bav_sd <- aggregate(hillshade_bav, fact=20, fun=sd,na.rm=T)
  hillshade_bav_500m <- rast(list(hillshade_bav_mean, hillshade_bav_median, hillshade_bav_min, 
                                 hillshade_bav_max, hillshade_bav_sd))
  rm(hillshade_bav_mean, hillshade_bav_median, hillshade_bav_min, hillshade_bav_max, hillshade_bav_sd); gc()
  
  # Turn into data.frame
  hillshade_bav_500m <- as.data.frame(hillshade_bav_500m, xy=T)
  colnames(hillshade_bav_500m) <-  c("x", "y", "hillshade_mean", "hillshade_median", "hillshade_min", 
                                "hillshade_max", "hillshade_sd")
  
  #library(ggplot2)
  #hillshade_bav_500m %>% ggplot() + geom_tile(aes(x=x, y=y, fill=hillshade_mean))
  
  # Save to file
  save(hillshade_bav_500m, file="data/hillshade_bav_500m.rda", compress="xz")
}

####################

# Create 500m eu_dem data for Bavaria
if(!file.exists("data/eu_dem_bav_500m.rda")){
  ## Load DEM data
  if(!file.exists("extdata/eu_dem_eur.tif")){
    filedir <- "extdata/EU_DEM_mosaic_1000K/"
    file <- list.files(filedir, pattern=".tif$", full.names=T)
    eu_dem_eur <- rast(file)
    # Crop & mask data
    europe_laea <- st_transform(europe, crs=crs(eu_dem_eur))
    europe_laea <- vect(europe_laea)
    eu_dem_eur <- crop(eu_dem_eur,europe_laea); invisible(gc())
    writeRaster(eu_dem_eur, filename="extdata/eu_dem_eur.tif"); rm(eu_dem_eur); invisible(gc())
  } else{
    eu_dem_eur <- terra::rast("extdata/eu_dem_eur.tif")
    # Transform europe
    bavaria_laea <- st_transform(bavaria, crs=crs(eu_dem_eur))
  }
  
  eu_dem_bav <- mask(crop(eu_dem_eur, vect(bavaria_laea)),vect(bavaria_laea)); rm(eu_dem_eur); gc()
  
  # Turn into 500 m raster
  eu_dem_bav_mean <- aggregate(eu_dem_bav, fact=20, fun=mean, na.rm=T)
  eu_dem_bav_median <- aggregate(eu_dem_bav, fact=20, fun=median, na.rm=T)
  eu_dem_bav_min <- aggregate(eu_dem_bav, fact=20, fun=min, na.rm=T)
  eu_dem_bav_max <- aggregate(eu_dem_bav, fact=20, fun=max, na.rm=T)
  eu_dem_bav_sd <- aggregate(eu_dem_bav, fact=20, fun=sd,na.rm=T)
  eu_dem_bav_500m <- rast(list(eu_dem_bav_mean, eu_dem_bav_median, eu_dem_bav_min, 
                                  eu_dem_bav_max, eu_dem_bav_sd))
  rm(eu_dem_bav_mean, eu_dem_bav_median, eu_dem_bav_min, eu_dem_bav_max, eu_dem_bav_sd); gc()
  
  # Turn into data.frame
  eu_dem_bav_500m <- as.data.frame(eu_dem_bav_500m, xy=T)
  colnames(eu_dem_bav_500m) <- c("x", "y", "altitude_mean", "altitude_median", "altitude_min", 
                            "altitude_max", "altitude_sd")
  
  #library(ggplot2)
  #eu_dem_bav_500m %>% ggplot() + geom_tile(aes(x=x, y=y, fill=altitude_mean))
  
  # Save to file
  save(eu_dem_bav_500m, file="data/eu_dem_bav_500m.rda", compress="xz")
}