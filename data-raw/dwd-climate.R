# DWD Wetterdaten

#' Der CDC OpenData bietet freien Zugang zu vielen Klimadaten des DWD.

#' Im Zug der Zusammenführung aller DWD-Datenangebote wird der CDC-FTP Server 
#' in den DWD OpenData Bereich überführt. 
#' Dort ist ein Zugriff über ftp wie auch über https möglich:

#' Die neuen Zugriffsadressen sind: https://opendata.dwd.de/climate_environment/CDC/
#' bzw. ftp://opendata.dwd.de/climate_environment/CDC/.
#' 

rm(list=ls()); gc()

#' Time series data by state can be found here:

#https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/annual/
#https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/
#https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/seasonal/

if(!file.exists("data/dwd_annual_ts_bav.rda")){
  link <- "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/annual/"
  vars <- c("air_temperature_mean", "frost_days", "hot_days", "ice_days", 
            "precipGE10mm_days", "precipGE20mm_days", "precipitation", "summer_days",
            "sunshine_duration", "tropical_nights_tminGE20")
  vars2 <- c("tm", "tnas", "txbs", "txcs", "rrsfs", "rrsgs", "rr", "txas", "sd", "tnes")
  files <- sapply(1:10, function(x) paste0(link, vars[x], "/", "regional_averages_", vars2[x], "_year.txt"))
  
  library(dplyr)
  dwd_annual_ts_bav <- lapply(1:10, function(x){
    dat <- read.table(files[x], skip=1, header=T, sep=";", dec=".") %>% select(-c("Jahr.1", "X"))
    colnames(dat)[1] <- "Jahr"              
    dat_bav <- dat %>% select(Jahr, Bayern) %>% filter(Jahr > 1950)
    dat_bav$var <- vars[x]
    #dat_bav$var <- vars2[x]
    dat_bav <- dat_bav %>% tidyr::spread(var, Bayern)
    return(dat_bav)
  })
  dwd_annual_ts_bav <- Reduce(function(...) dplyr::left_join(..., by=c("Jahr"), all.x=TRUE), dwd_annual_ts_bav)
  head(dwd_annual_ts_bav)
  
  # Save to file
  save(dwd_annual_ts_bav, file="data/dwd_annual_ts_bav.rda", compress="xz")
}

# Gridded monthly data can be found here:
# https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/

rm(list=ls()); gc()

# Specify file combinations
indir <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/"
var1 <- c("air_temperature_mean/", "precipitation/")
var2 <- c("air_temperature_max/", "air_temperature_min/")
month <- c("01_Jan", "02_Feb", "03_Mar", "04_Apr", "05_May", "06_Jun", 
           "07_Jul", "08_Aug", "09_Sep", "10_Oct", "11_Nov", "12_Dec")
year1 <- paste0(1881:2021)
year2 <- paste0(1901:2021)
df1 <- expand.grid(indir=indir, var=var1, month=month, year=year1)
df2 <- expand.grid(indir=indir, var=var2, month=month, year=year2)
df1$indir <- paste0(df1$indir, df2$var, df2$month, "/")
df2$indir <- paste0(df2$indir, df2$var, df2$month, "/")
filename1 <- c("grids_germany_monthly_air_temp_mean", "grids_germany_monthly_precipitation")
filename2 <- c("grids_germany_monthly_air_temp_max", "grids_germany_monthly_air_temp_min")
df1$file <- paste0(filename1, "_", df1$year, substr(df1$month, 01, 02), ".asc.gz")
df2$file <- paste0(filename2, "_", df2$year, substr(df2$month, 01, 02), ".asc.gz")
df <- dplyr::bind_rows(df1, df2)
df$var2 <- sub("air_temperature_min/", "TADNMM", 
               sub("air_temperature_max/", "TADXMM", 
                   sub("precipitation/", "RSMS", 
                       sub("air_temperature_mean/", "TAMM", df$var))))
df$file2 <- paste0(df$var2, "_", substr(df$month, 1, 2), "_", df$year, "_01.asc")
outdir <- "/home/matt/Documents/DWD/"

#mis <- sapply(df$file, function(z) !file.exists(paste0(outdir, z)))
#missing_df <- df[mis,]

# Download files
#lapply(1:nrow(missing_df), function(z){
#  print(z)
#  ## supply wget command
#  wgetCmd <- paste('wget -P', outdir, paste0(df[z, "indir"], df[z, "file"]), sep=' ')
#  ##run command
#  system(wgetCmd)
#})

###

# Manually extract files

###

# Load terra package
library(terra)

# Load bavaria outline
load("data/bavaria.rda")
bavaria_gk <- sf::st_transform(bavaria, crs="epsg:31467")

mis <- sapply(df$file2, function(z) !file.exists(paste0(outdir, z)))
df[mis,]
avail_df <- df[!mis,]

library(dplyr)
keys <- avail_df %>% group_by(var2) %>% group_keys()
avail_df <- avail_df %>% group_by(var2) %>% group_split()

lapply(1:length(avail_df), function(z){
  if(!file.exists(paste0("extdata/", unique(avail_df[[z]]$var2), "_", first(avail_df[[z]]$year[1:804]), 
                         "_", last(avail_df[[z]]$year[1:804]), "_bav.nc"))){
    dat_all <- lapply(1:804, function(k){
      # Load, crop & mask elevation data
      dat <- terra::rast(paste0(outdir, avail_df[[z]]$file2[k]))
      crs(dat) <- "epsg:31467"
      dat <- terra::mask(terra::crop(dat, bavaria_gk), vect(sf::st_as_sf(bavaria_gk))); gc()
      return(dat)
    }); gc()
    dat_all <- terra::rast(dat_all); gc()
    time(dat_all) <- as.Date(paste0(avail_df[[z]]$year[1:804],"-", 
                                    substr(avail_df[[z]]$month[1:804],1,2), "-15"))
    writeCDF(dat_all,filename=paste0("extdata/", unique(avail_df[[z]]$var2), "_", first(avail_df[[z]]$year[1:804]), 
                                     "_", last(avail_df[[z]]$year[1:804]), "_bav.nc"))
    rm(dat_all); gc()
  }
  if(!file.exists(paste0("extdata/", unique(avail_df[[z]]$var2), "_", 
                         first(avail_df[[z]]$year[805:nrow(avail_df[[z]])]), 
                         "_", last(avail_df[[z]]$year[805:nrow(avail_df[[z]])]), "_bav.nc"))){
    dat_all <- lapply(805:nrow(avail_df[[z]]), function(k){
      # Load, crop & mask elevation data
      dat <- terra::rast(paste0(outdir, avail_df[[z]]$file2[k]))
      crs(dat) <- "epsg:31467"
      dat <- terra::mask(terra::crop(dat, bavaria_gk), vect(sf::st_as_sf(bavaria_gk))); gc()
      return(dat)
    }); gc()
    dat_all <- terra::rast(dat_all); gc()
    time(dat_all) <- as.Date(paste0(avail_df[[z]]$year[805:nrow(avail_df[[z]])],"-", 
                                    substr(avail_df[[z]]$month[805:nrow(avail_df[[z]])],1,2), "-15"))
    writeCDF(dat_all,filename=paste0("extdata/", unique(avail_df[[z]]$var2), "_", 
                                     first(avail_df[[z]]$year[805:nrow(avail_df[[z]])]), 
                                     "_", last(avail_df[[z]]$year[805:nrow(avail_df[[z]])]), "_bav.nc"))
    rm(dat_all); gc()
  }
})

rm(list=ls()); gc()
library(terra)
library(dplyr)

load("data/tk4tel_grid.rda")
tk4tel_r <- terra::rast(tk4tel_grid, type="xyz")

var <- c("RSMS", "TADNMM", "TADXMM", "TAMM")
lapply(var, function(z){
  if(!file.exists(paste0("data/dwd_yearmon_", tolower(z), "_bav_tk4tel.rda"))){
    files <- list.files(path="extdata", pattern=z, full.names = T)
    dat <- terra::rast(files)
    plot(dat[[6]])
    time(dat)[1:10]
    
    # Turn into TK4tel grid
    dat <- terra::aggregate(dat, fact=6, fun="mean", na.rm=T)
    dat <- terra::project(dat, "epsg:31468")
    dat <- terra::resample(dat, tk4tel_r, method="bilinear")
    plot(dat[[6]])
    
    dat_df <- dat %>% as.data.frame(xy=T)
    colnames(dat_df)[3:ncol(dat_df)] <- as.character(zoo::as.yearmon(time(dat)))
    #colnames(dat_df)[1:10]
    #dat_df$var <- var
    
    # Save data
    assign(paste0("dwd_yearmon_", tolower(z), "_bav_tk4tel"), dat_df)
    save(list=paste0("dwd_yearmon_", tolower(z), "_bav_tk4tel"), 
         file=paste0("data/dwd_yearmon_", tolower(z), "_bav_tk4tel.rda"), compress="xz")
    rm(dat, dat_df); gc()
  }
})
