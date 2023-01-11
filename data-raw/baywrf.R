#' BayWRF: A convection-resolving atmospheric dataset for the region of Bavaria over the thirty-year period of September 1987 to August 2018.
#' 
#' BAYWRF data were produced using the atmospheric model WRF v. 4.1 configured with two nested domains, 
#' of 7.5- and 1.5-km grid spacing, centred over Bavaria. 
#' The model was forced at its outer lateral boundaries by ERA5 reanalysis data at three-hourly frequency. 
#' The dataset covers the period of 1 September 1987 to 31 August 2018.
#' Here, we provide a selection of daily mean data from the finest WRF domain (D2; 1.5-km grid spacing) 
#' cropped to the extent of Bavaria and below ~ 200 hPa. 
#' For these data, perturbation and base-state atmospheric pressure (WRF variables P and PB) and 
#' geopotential (PH and PHB) were combined to generate full model fields, 
#' while perturbation potential temperature (T) was converted to atmospheric temperature.
#' These data were generated as part of the BayTreeNet project, 
#' which aims to investigate the impact of climate change on forest ecosystems in Bavaria. 
#' The BayTreeNet project is sponsored by the Bavarian State Ministry of Science and 
#' the Arts in the context of the Bavarian Climate Research Network (bayklif).

#' Further information about these data and how they were generated is provided in:
#' Collier, E. and Mölg, T.: BAYWRF: a high-resolution present-day climatological 
#' atmospheric dataset for Bavaria, Earth Syst. Sci. Data, 12, 3097–3112, 
#' https://doi.org/10.5194/essd-12-3097-2020, 2020.

# Data is available from: https://osf.io/aq58b/

# And can be downloaded with the osfr package

library(osfr)
baywrf_rp <- osf_retrieve_node("aq58b")

#metadata <- osf_ls_files(baywrf_rp, pattern="cdump_wrfout_d02_1987-09.3D")
#osf_download(metadata, path="/home/matt/Documents/WRF", recurse=T, conflicts="skip")

######

# Check metadata file!!!

######

#files <- osf_ls_files(baywrf_rp, path="3D_DATA", n_max=50)
#osf_download(files, path="/home/matt/Documents/WRF", recurse=T, conflicts="skip")

setwd("/home/matt/Documents/bavDC")
rm(list=ls()); gc()

# List files
(files <- list.files("/home/matt/Documents/BayWRF", pattern="3D.nc", full.names=T, recursive = T))

# Obtain size and other specifications of file
library(ncdf4); library(terra)
nc <- ncdf4::nc_open(files[1])

# Obtain times
nc$var[[1]]$name
times <- ncdf4::ncvar_get(nc, nc$var[[1]])

x <- ncdf4::ncvar_get(nc, nc$var[[2]])
y <- ncdf4::ncvar_get(nc, nc$var[[3]])
min(x); max(x)
min(y); max(y)

# List varnames
(vars <- sapply(1:length(nc$var), function(x) nc$var[[x]]$longname))

# Close nc file
ncdf4::nc_close(nc); rm(nc)

# Load outline of bavaria
load("data/bavaria.rda")

# Obtain & pre-process temperature data
baywrf_tas_bav <- lapply(files, function(z){
  nc <- ncdf4::nc_open(z)
  
  # Obtain times
  times <- as.Date(sapply(ncdf4::ncvar_get(nc, nc$var[[1]]), 
                          function(x) strsplit(x, split="_")[[1]][1]))
  
  # Load data
  sub_dat <- ncdf4::ncvar_get(nc, nc$var[[5]])
  
  # Bring data into correct format
  sub_dat <- terra::rast(sub_dat)
  terra::ext(sub_dat) <- c(8.506531,14.271,47.05833,50.74763)
  crs(sub_dat) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # Calculate mean temperature
  sub_dat <- terra::mean(sub_dat)
  names(sub_dat) <- unique(zoo::as.yearmon(times))
  
  # Close nc file
  ncdf4::nc_close(nc)
  
  # Mask data by outline of Bavaria
  sub_dat <- terra::mask(terra::crop(sub_dat, terra::vect(bavaria)), terra::vect(bavaria))
  return(sub_dat)
})
baywrf_tas_bav <- terra::rast(baywrf_tas_bav)
names(baywrf_tas_bav)
baywrf_tas_bav <- baywrf_tas_bav - 273.15

# Plot map of minimum temperature data
terra::plot(baywrf_tas_bav[[100]])
crs(baywrf_tas_bav[[11]]) <- "+proj=longlat +datum=WGS84 +no_defs"
tas_bav <- project(baywrf_tas_bav[[11]], y="+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m")

plot(sf::st_geometry(bavaria), add=T, col=NA)

baywrf_tas_bav <- baywrf_tas_bav %>% as.data.frame(xy=T)
save(baywrf_tas_bav, file="inst/extdata/baywrf_tas_bav.rda", compress="xz")

# Obtain & pre-process rainfall data
baywrf_pr_bav <- lapply(files, function(z){
  nc <- ncdf4::nc_open(z)
  
  # Obtain times
  times <- as.Date(sapply(ncdf4::ncvar_get(nc, nc$var[[1]]), 
                          function(x) strsplit(x, split="_")[[1]][1]))
  
  # Load data
  sub_dat <- ncdf4::ncvar_get(nc, nc$var[[5]])
  
  # Bring data into correct format
  sub_dat <- terra::rast(sub_dat)
  terra::ext(sub_dat) <- c(8.506531,14.271,47.05833,50.74763)
  
  # Calculate precipitation
  sub_dat <- terra::mean(sub_dat)
  names(sub_dat) <- unique(zoo::as.yearmon(times))
  
  # Close nc file
  ncdf4::nc_close(nc)
  
  # Mask data by outline of Bavaria
  sub_dat <- terra::mask(terra::crop(sub_dat, terra::vect(bavaria)), 
                         terra::vect(bavaria))
  return(sub_dat)
})
baywrf_pr_bav <- terra::rast(baywrf_pr_bav)
names(baywrf_pr_bav)

# Plot map of total rainfall data
terra::plot(baywrf_pr_bav[[8]])
plot(sf::st_geometry(bavaria), add=T, col=NA)

baywrf_pr_bav <- baywrf_pr_bav %>% as.data.frame(xy=T)
save(baywrf_pr_bav, file="inst/extdata/baywrf_pr_bav.rda", compress="xz")

####################

# Obtain & pre-process tasmax & tasmin data

setwd("/home/matt/Documents/bavDC")
rm(list=ls()); gc()

# List files
tasmax_files <- list.files("/home/matt/Documents/BayWRF", pattern="daymax", 
                            full.names=T, recursive = T)
tasmax_yearmon <- sapply(tasmax_files, function(x){
  substr(strsplit(basename(x), split="_")[[1]][4], start=0, stop=7)
  })
tasmax_files <- split(tasmax_files, tasmax_yearmon)

(tasmin_files <- list.files("/home/matt/Documents/BayWRF", pattern="daymin", 
                            full.names=T, recursive = T))
tasmin_yearmon <- sapply(tasmin_files, function(x){
  substr(strsplit(basename(x), split="_")[[1]][4], start=0, stop=7)
})
tasmin_files <- split(tasmin_files, tasmin_yearmon)

# Obtain size and other specifications of file
library(ncdf4); library(terra)
nc <- ncdf4::nc_open(tasmax_files[[1]][1])
print(nc)

# Obtain times
times <- ncdf4::ncvar_get(nc, nc$var[[1]])

# List varnames
(vars <- sapply(1:length(nc$var), function(x) nc$var[[x]]$longname))

# Close nc file
ncdf4::nc_close(nc); rm(nc)

# Load outline of bavaria
load("data/bavaria.rda")

# Obtain & pre-process minimum temperature data
baywrf_tasmin_bav <- lapply(1:length(tasmin_files), function(z){
  sub_dat <- lapply(unlist(tasmin_files[[z]]), function(y){
    # Open nc file
    nc <- ncdf4::nc_open(y)
    
    # Load data
    sub_dat <- ncdf4::ncvar_get(nc, nc$var[[2]])

    # Bring data into correct format
    sub_dat <- terra::rast(sub_dat)
    terra::rast(ncols=350, nrows=350, resolution = c(1500, 1500), 
                extent = c(-172000, xmax=353500, ymin=1875000, ymax=2400500),
                crs="+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m")
    terra::ext(sub_dat) <- c(-163000, xmax=362000, ymin=1864500, ymax=2389500)
    crs(sub_dat) <- "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m"
    sub_dat
    sub_dat <- terra::project(sub_dat, y="+proj=longlat +datum=WGS84 +no_defs")
    
    # Mid-coordinates as given by NetCDF Info: 48.9281578063965, 11.4047546386719
    (ext(sub_dat)[4]- ext(sub_dat)[3])/2+ ext(sub_dat)[3]
    (ext(sub_dat)[2]- ext(sub_dat)[1])/2+ ext(sub_dat)[1]
    
    r <- terra::rast(ncols=271, nrows=271, resolution = c(0.0212711, 0.01361365), 
                     extent = c(8.506531, 14.271, 47.05833, 50.74763))
    sub_dat <- project(sub_dat, r)
    
    # Close nc file
    ncdf4::nc_close(nc)
    return(sub_dat)
  })
  sub_dat <- terra::rast(sub_dat)
  
  # Calculate mean temperature
  sub_dat <- terra::mean(sub_dat)
  names(sub_dat) <- unique(zoo::as.yearmon(unique(tasmin_yearmon)[[z]]))

  # Mask data by outline of Bavaria
  sub_dat <- terra::mask(terra::crop(sub_dat, terra::vect(bavaria)), 
                         terra::vect(bavaria))
  return(sub_dat)
}); gc()
baywrf_tasmin_bav <- terra::rast(baywrf_tasmin_bav); gc()
names(baywrf_tasmin_bav)

baywrf_tasmin_bav <- baywrf_tasmin_bav - 273.15

# Plot map of minimum temperature data
terra::plot(baywrf_tasmin_bav[[8]])
plot(sf::st_geometry(bavaria), add=T, col=NA)

baywrf_tasmin_bav <- baywrf_tasmin_bav %>% as.data.frame(xy=T)
head(baywrf_tasmin_bav)
save(baywrf_tasmin_bav, file="inst/extdata/baywrf_tasmin_bav.rda", compress="xz")

baywrf_tasmax_bav <- lapply(1:length(tasmax_files), function(z){
  sub_dat <- lapply(unlist(tasmax_files[[z]]), function(y){
    # Open nc file
    nc <- ncdf4::nc_open(y)
    
    # Load data
    sub_dat <- ncdf4::ncvar_get(nc, nc$var[[2]])
    
    # Bring data into correct format
    sub_dat <- terra::rast(sub_dat)
    terra::rast(ncols=350, nrows=350, resolution = c(1500, 1500), 
                     extent = c(-172000, xmax=353500, ymin=1875000, ymax=2400500),
                     crs="+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m")
    terra::ext(sub_dat) <- c(-163000, xmax=362000, ymin=1864500, ymax=2389500)
    crs(sub_dat) <- "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m"
    sub_dat
    sub_dat <- terra::project(sub_dat, y="+proj=longlat +datum=WGS84 +no_defs")
   
    # Mid-coordinates as given by NetCDF Info: 48.9281578063965, 11.4047546386719
    (ext(sub_dat)[4]- ext(sub_dat)[3])/2+ ext(sub_dat)[3]
    (ext(sub_dat)[2]- ext(sub_dat)[1])/2+ ext(sub_dat)[1]

    r <- terra::rast(ncols=271, nrows=271, resolution = c(0.0212711, 0.01361365), 
                     extent = c(8.506531, 14.271, 47.05833, 50.74763))
    sub_dat <- project(sub_dat, r)
    
    # Close nc file
    ncdf4::nc_close(nc)
    return(sub_dat)
  })
  sub_dat <- terra::rast(sub_dat)
  
  # Calculate mean temperature
  sub_dat <- terra::mean(sub_dat)
  names(sub_dat) <- unique(zoo::as.yearmon(unique(tasmax_yearmon)[[z]]))
  
  # Mask data by outline of Bavaria
  sub_dat <- terra::mask(terra::crop(sub_dat, terra::vect(bavaria)), 
                         terra::vect(bavaria))
  return(sub_dat)
}); gc()
baywrf_tasmax_bav <- terra::rast(baywrf_tasmax_bav); gc()
names(baywrf_tasmax_bav)

baywrf_tasmax_bav <- baywrf_tasmax_bav - 273.15

# Plot map of maximum precipitation data
terra::plot(baywrf_tasmax_bav[[8]])
plot(sf::st_geometry(bavaria), add=T, col=NA)

baywrf_tasmax_bav <- baywrf_tasmax_bav %>% as.data.frame(xy=T)
head(baywrf_tasmax_bav)
save(baywrf_tasmax_bav, file="inst/extdata/baywrf_tasmax_bav.rda", compress="xz")

# Example documentation for baywrf data

#'
#' @docType data
#' @name baywrf_tasmin_bav
#' @title BayWRF minimum temperature data of Bavaria
#' @description BayWRF monthly mean minimum air temperature data of Bavaria
#' @usage data(baywrf_tasmin_bav)
#' @details BayWRF daily minimum air temperature data from September 1987 - August 2018 
#' aggregated to monthly data and cropped to the outline of Bavaria 
#' @format A \code{sf} object with 51084 observations and 375 variables.
NULL
