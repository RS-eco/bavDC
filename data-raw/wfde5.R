#' WFDE5 Data for Europe

#' Data was downloaded from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.20d54e34?tab=form

rm(list=ls()); gc()

# Load processNC package
library(processNC)

# Load other packages
library(terra)

# Set file directory
filedir <- "C:/Users/Admin/Documents/WFDE5"

# Load bavaria outline
load("data/bavaria.rda")

# Specify variables
var <- c("Rainf", "Snowf", "Tair")

# List files
files <- list.files(filedir, pattern=paste0(var[1], "_"), full.names=T)
head(files)

# Load climate data
#dat <- terra::rast(files[1])
#dat
#time(dat)[1:10]

# Check format

#rm(dat); gc()

# Load files
lapply(var, function(z){
  # List files
  files <- list.files(filedir, pattern=paste0(z, "_"), full.names=T)
  
  if(!file.exists(paste0("extdata/", z, "_bav_yearmon.nc"))){
    # Loop through individual files
    all_dat <- lapply(files, function(x){
      # Load data file
      dat <- terra::rast(x)
      
      # Crop and mask by outline of Bavaria
      dat <- terra::mask(terra::crop(dat, bavaria), vect(sf::st_as_sf(bavaria)))
      
      if(z %in% c("Tair")){
        avg <- mean(dat); rm(dat); gc()
      } else if(z %in% c("Rainf", "Snowf")){
        avg <- sum(dat)
      }
      return(avg)
    })
    all_dat <- terra::rast(all_dat)
    all_dat
    names(all_dat) <- basename(files)
    plot(all_dat[[1]])
    
    writeCDF(all_dat ,filename=paste0("extdata/", z, "_bav_yearmon.nc"))
  }
})

# Specify variables
var <- c("Rainf", "Snowf", "Tair")

# Specify dates
dates <- as.Date(paste0(as.numeric(sapply(files, function(x) strsplit(basename(x), "_")[[1]][4])), "15"), "%Y%m%d")

# Load files
lapply(var, function(z){
  dat <- terra::rast(paste0("extdata/", tolower(z), "_bav_yearmon.nc"))
  time(dat) <- dates
  names(dat) <- as.character(zoo::as.yearmon(dates))
  dat_df <- as.data.frame(dat, xy=T)
  assign(paste0("wfde5_", tolower(z), "_bav"), value=dat_df)
  save(list=paste0("wfde5_", tolower(z), "_bav"), 
       file=paste0("data/wfde5_", tolower(z), "_bav.rda"), compress="xz")
})

