#' WFDE5 Data for Europe

#' Data was downloaded from: https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.20d54e34?tab=form

rm(list=ls()); gc()

# Load processNC package
library(processNC)

# Load other packages
library(terra)

# Set file directory
filedir <- "/home/matt/Documents/WFDE5"

# Load bavaria outline
load("data/bavaria.rda")

# Specify variables
var <- c("Rainf", "Snowf", "Tair")

# List files
files1 <- list.files(filedir, pattern=paste0(var[1], "_"), full.names=T)
files2 <- list.files(filedir, pattern=paste0(var[2], "_"), full.names=T)
files3 <- list.files(filedir, pattern=paste0(var[3], "_"), full.names=T)
files <- c(files1, files2, files3)
head(files)

# Check for missing files
df <- expand.grid(var, paste0("_WFDE5_CRU_", 1979:2019), paste0(c("01","02","03","04","05","06","07","08","09","10","11","12"), "_v2.0.nc"))
df <- df %>% tidyr::unite("file", Var1:Var3, sep="", remove = T)
head(df)
filenames <- df$file
filenames[which(!filenames %in% basename(files))]

# Load climate data
dat <- terra::rast(files[1])
dat
time(dat)[1:10]
rm(dat); gc()

# Load files
lapply(var, function(z){
  # List files
  files <- list.files(filedir, pattern=paste0(z, "_"), full.names=T)
  
  if(!file.exists(paste0("inst/extdata/", z, "_bav_yearmon.nc"))){
    # Loop through individual files
    all_dat <- lapply(files, function(x){
      # Load data file
      dat <- terra::rast(x)
      
      # Crop and mask by outline of Bavaria
      dat <- terra::mask(terra::crop(dat, bavaria), vect(sf::st_as_sf(bavaria)))
      
      if(z %in% c("Tair")){
        day_mean <- terra::tapp(dat, index="days", fun="mean"); rm(dat)
        terra::time(day_mean) <- as.Date(sub("X", "", names(day_mean)), format="%Y.%m.%d")
        avg <- terra::tapp(day_mean, index="months", fun="mean"); rm(day_mean); gc()
        terra::time(avg) <- zoo::as.yearmon(strsplit(basename(x), split="_")[[1]][4], format="%Y%m")
      } else if(z %in% c("Rainf", "Snowf")){
        day_mean <- terra::tapp(dat, index="days", fun="sum"); rm(dat)
        terra::time(day_mean) <- as.Date(sub("X", "", names(day_mean)), format="%Y.%m.%d")
        avg <- terra::tapp(day_mean, index="months", fun="sum"); rm(day_mean); gc()
        terra::time(avg) <- zoo::as.yearmon(strsplit(basename(x), split="_")[[1]][4], format="%Y%m")
      }
      return(avg)
    })
    all_dat <- terra::rast(all_dat)
    all_dat
    names(all_dat) <- basename(files)
    plot(all_dat[[1]])
    writeCDF(all_dat ,filename=paste0("inst/extdata/", z, "_bav_yearmon.nc"))
  }
})

# Specify variables
var <- c("Rainf", "Snowf", "Tair")

# Specify dates
dates <- as.Date(paste0(as.numeric(sapply(files1, function(x) strsplit(basename(x), "_")[[1]][4])), "15"), "%Y%m%d")

# Load files
lapply(var, function(z){
  dat <- terra::rast(paste0("inst/extdata/", z, "_bav_yearmon.nc"))
  time(dat) <- dates
  names(dat) <- as.character(zoo::as.yearmon(dates))
  dat_df <- as.data.frame(dat, xy=T)
  assign(paste0("wfde5_", tolower(z), "_bav"), value=dat_df)
  save(list=paste0("wfde5_", tolower(z), "_bav"), 
       file=paste0("data/wfde5_", tolower(z), "_bav.rda"), compress="xz")
})

