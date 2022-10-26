#' ---
#' title: "Process bird atlas data"
#' ---

dat <- list.files("extdata/EBBA_50-km_grid/", pattern=".shp", full.names=T)

dat2 <- vroom::vroom("extdata/EBBA2_Sub_Dec2021.csv")
head(dat2)

library(sf)
dat <- st_read(dat)
plot(st_geometry(dat))

dat <- as(dat, "Spatial")
dat <- dat %>% as.data.frame() %>% select(x,y, all_area)

r_ebcc <- raster::rasterFromXYZ(dat, digits=0.4, #res=c(50000, 50000), 
                                crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
r_ebcc
plot(r_ebcc)
