rm(list=ls()); gc()

# Load rinat package
library(rinat)

# Load additional packages
library(dplyr)

# Load bavaria shapefile
load("~/Documents/bavDC/data/bavaria.rda")
sf::st_bbox(bavaria)

## Search by area
bounds <- c(47.1,8.8,50.7,13.9)
years <- 1955:2019
inat_bav_1955_2019  <- lapply(years, function(x){
  dat <- try(get_inat_obs(bounds = bounds, year=x, maxresults=10000) %>%
               dplyr::select(scientific_name, datetime, place_guess, longitude, latitude,
                             common_name, species_guess, iconic_taxon_name, taxon_id)); gc()
  Sys.sleep(3)
  return(dat)
})

# Years with no data.frame
years[which(!sapply(inat_bav_1955_2019, is.data.frame))]
inat_bav_1955_2019 <- Filter(is.data.frame, inat_bav_1955_2019)

inat_bav_2020 <- lapply(1:12, function(x){
  dat <- try(get_inat_obs(bounds = bounds, year=2020, month=x, maxresults=10000) %>%
               dplyr::select(scientific_name, datetime, place_guess, longitude, latitude,
                             common_name, species_guess, iconic_taxon_name, taxon_id)); gc()
  Sys.sleep(3)
  return(dat)
}); gc()

# Months with no data.frame
month.abb[which(!sapply(inat_bav_2020, is.data.frame))]

# Filter months with data
inat_bav_2020 <- Filter(is.data.frame, inat_bav_2020)

inat_bav_2021 <- lapply(1:12, function(x){
  dat <- try(get_inat_obs(bounds = bounds, year=2021, month=x, maxresults=10000) %>%
               dplyr::select(scientific_name, datetime, place_guess, longitude, latitude,
                             common_name, species_guess, iconic_taxon_name, taxon_id)); gc()
  Sys.sleep(3)
  return(dat)
}); gc()

# Months with no data.frame
month.abb[which(!sapply(inat_bav_2021, is.data.frame))]

# Filter months with data
inat_bav_2021 <- Filter(is.data.frame, inat_bav_2021)

# Turn list into data.frame
inat_bav_1955_2021 <- c(inat_bav_1955_2019, inat_bav_2020, inat_bav_2021)
inat_bav_1955_2021 <- bind_rows(inat_bav_1955_2021)

# Plot data
plot(inat_bav_1955_2021$longitude, inat_bav_1955_2021$latitude, col="red")
sp::plot(as(bavaria, "Spatial"), add=T)

hist(lubridate::year(inat_bav_1955_2021$datetime))

# Save data to file
save(inat_bav_1955_2021, file="data/inat_bav_1955_2021.rda", compress="xz")
