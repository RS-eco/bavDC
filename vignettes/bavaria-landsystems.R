## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", results="asis", 
  echo=F, warning=F, message=F,
  fig.width=8, fig.height=6, fig.path="../figures/"
)

## ----load_pkgs----------------------------------------------------------------
library(dplyr); library(tidyr); library(ggplot2)

# Define standard colour scheme
whitered <- colorRampPalette(c("white", "#e9e29c","#eeb479","#e88471","#cf597e"))(255)

# Landsystem id
ls_id <- c(21, 22, 23, 41, 42, 43, 51, 52, 53, 31, 32, 61, 62, 63, 71, 72, 74, 75, 731, 732, 733, 11, 12, 13, 80, 90)

# Landsystem class
ls_class  <- c("low-intensity settlement", "medium intensity settlement", "high intensity settlement",
               "low-intensity forest", "medium-intensity forest", "high-intensity forest", "low-intensity grassland",
               "medium-intensity grassland", "high-intensity grassland", "low-intensity cropland", 
               "medium-intensity cropland", "high-intensity cropland", "extensive perm-crops", "intensive perm-crops",
               "forest/shrubs and cropland mosaics", "forest/shrubs and grassland mosaics", 
               "forest/shrubs and bare mosaics", "forest/shrubs and mixed agriculture mosaics",
               "low-intensity agricultural mosaics", "medium-intensity agricultural mosaics", 
               "high-intensity agricultural mosaics", "water body", "wetland", "glacier", "shrub",
               "bare and rocks")

# Colour key as provided by Dou et al. 2021
colours <- c("#FF0000", "#FF7F7F", "#A80000", "#00A600", "#00C800", "#82A600", "#CCF24D", "#96CC64", "#C8CC14", "#E6A600",
             "#E68000", "#FFFFA8", "#E6E600", "#FFFF00", "#66CDAB", "#F5F57A", "#A6FF80", "#F2CCA6", "#FFBEE8", "#D69DBC",
             "#CD6699", "#002673", "#A6A6FF", "#A6E6CC", "#A6E64D", "#CCCCCC")

## -----------------------------------------------------------------------------
# Load shapefile of Bavaria
data("bavaria", package="bavDC")
bavaria <- sf::st_as_sf(bavaria)

## -----------------------------------------------------------------------------
data("landsystem_bav_tk4tel", package="bavDC")
#head(landsystem_bav_tk4tel)

# Plot individual variable from long format
landsystem_bav_tk4tel %>% ggplot() + 
  geom_tile(aes (x=x, y=y, fill=`landsystem`)) +
  scale_fill_manual(name="Landsystem", values=colours) + 
  coord_sf() + theme_bw() + 
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank())
  
data("landsystem_perc_bav_tk4tel", package="bavDC")
#head(landsystem_perc_bav_tk4tel)

#+ fig.width=6, fig.height=10
# Plot perc_cover
landsystem_perc_bav_tk4tel %>% gather(var, value, -c(x,y)) %>%
  ggplot() + geom_tile(aes (x=x, y=y, fill=value)) +
  facet_wrap(.~var) + scale_fill_gradientn(name="% Cover", colours=whitered) + 
  coord_sf() + theme_bw() + 
  theme(axis.text=element_blank(), axis.title=element_blank(),
        axis.ticks=element_blank(), strip.background = element_blank(),
        legend.position = c(0.6,0.1), legend.direction = "horizontal")
rm(list=ls()); invisible(gc())

