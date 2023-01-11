## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo=F, warning=F, message=F, comment = NA, 
  fig.width=10, fig.height=8, eval=F, fig.path="../figures/"
)

## ----load_pkgs----------------------------------------------------------------
#  library(dplyr); library(tidyr)
#  library(lubridate); library(zoo)
#  library(ggplot2); library(patchwork)
#  library(scico)

## -----------------------------------------------------------------------------
#  # Load shapefile of Bavaria
#  data("bavaria", package="bavDC")
#  bavaria_gk <- sf::st_transform(bavaria, sp::CRS("+init=epsg:31468"))
#  bavaria_laea <- sf::st_transform(bavaria, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

## -----------------------------------------------------------------------------
#  # Named vector of clc colours
#  clc_cols <- c("Continuous urban fabric" = "#e6004d", "Discontinuous urban fabric" = "#ff0000", "Industrial or commercial units" = "#cc4df2", "Road and rail networks and associated land" = "#cc0000", "Port areas" = "#e6cccc", "Airports" = "#e6cce6", "Mineral extraction sites" = "#a600cc", "Dump sites" = "#a64d00", "Construction sites" = "#ff4dff", "Green urban areas" = "#ffa6ff", "Sport and leisure facilities" = "#ffe6ff", "Non-irrigated arable land" = "#ffffa8", "Permanently irrigated land" = "#ffff00", "Rice fields" = "#e6e600", "Vineyards" = "#e68000", "Fruit trees and berry plantations" = "#f2a64d", "Olive groves" = "#e6a600", "Pastures" = "#e6e64d", "Annual crops associated with permanent crops" = "#ffe6a6", "Complex cultivation patterns" = "#ffe64d", "Land principally occupied by agriculture with \n significant areas of natural vegetation" = "#e6cc4d", "Agro-forestry areas" = "#f2cca6", "Broad-leaved forest" = "#80ff00", "Coniferous forest" = "#00a600", "Mixed forest" = "#4dff00", "Natural grasslands" = "#ccf24d", "Moors and heathland" = "#a6ff80", "Sclerophyllous vegetation" = "#a6e64d", "Transitional woodland-shrub" = "#a6f200", "Beaches - dunes - sands" = "#e6e6e6", "Bare rocks" = "#cccccc", "Sparsely vegetated areas" = "#ccffcc", "Burnt areas" = "#000000", "Glaciers and perpetual snow" = "#a6e6cc", "Inland marshes" = "#a6a6ff", "Peat bogs" = "#4d4dff", "Salt marshes" = "#ccccff", "Salines" = "#e6e6ff", "Intertidal flats" = "#a6a6e6", "Water courses" = "#00ccf2", "Water bodies" = "#80f2e6", "Coastal lagoons" = "#00ffa6", "Estuaries" = "#a6ffe6", "Sea and ocean" = "#e6f2ff", "NODATA" = "#ffffff")

## -----------------------------------------------------------------------------
#  data("corine_lc_bav_tk4tel", package="bavDC")
#  colnames(corine_lc_bav_tk4tel) <- c("x", "y", "1990", "2000", "2006", "2012", "2018")
#  
#  clc_cols_1990 <- clc_cols[names(clc_cols) %in% corine_lc_bav_tk4tel$`1990`]
#  corine_lc_bav_tk4tel %>% ggplot() + geom_tile(aes (x=x, y=y, fill=`1990`)) +
#    scale_fill_manual(name="CLC 1990", values=clc_cols_1990) +
#    geom_sf(data=bavaria_gk, fill=NA) + coord_sf() +
#    theme_bw() + labs(x="", y="")

## -----------------------------------------------------------------------------
#  clc_all <- corine_lc_bav_tk4tel %>% pivot_longer(names_to="year", values_to="value", -c(x,y))
#  
#  clc_cols_all <- clc_cols[names(clc_cols) %in% clc_all$value]
#  clc_all %>% ggplot() + geom_tile(aes (x=x, y=y, fill=value)) +
#    facet_wrap(.~year) + scale_fill_manual(name="CLC", values=clc_cols_all) +
#    geom_sf(data=bavaria_gk, fill=NA) + coord_sf() +
#    theme_bw() + labs(x="", y="") +
#    theme(legend.position = "bottom", strip.background = element_blank())

## -----------------------------------------------------------------------------
#  data("corine_lc_perc_bav", package="bavDC")
#  
#  # Map of fruit trees and berry plantations
#  corine_lc_perc_bav %>% ggplot() +
#    geom_tile(aes(x=x, y=y, fill=`Fruit trees and berry plantations`)) +
#    scale_fill_scico(name="% Cover", palette="roma") +
#    ggtitle("Fruit trees and berry plantations") +
#    geom_sf(data=bavaria_laea, fill=NA) + coord_sf() +
#    labs(x="", y="") + theme_bw()
#  
#  # Map of mixed forest
#  corine_lc_perc_bav %>% ggplot() +
#    geom_tile(aes (x=x, y=y, fill=`Mixed forest`)) +
#    scale_fill_scico(name="% Cover", palette="roma") +
#    ggtitle("Mixed forest") +
#    geom_sf(data=bavaria_laea, fill=NA) + coord_sf() +
#    labs(x="", y="") + theme_bw()
#  
#  # Map of different forest types across years
#  corine_lc_perc_bav %>%
#    pivot_longer(names_to="var", values_to="value", -c(x,y,year)) %>%
#    filter(var %in% c("Pastures", "Broad-leaved forest", "Coniferous forest",
#                      "Mixed forest", "Natural grasslands")) %>%
#    ggplot() + geom_tile(aes(x=x, y=y, fill=value)) +
#    facet_grid(year~var) + scale_fill_scico(name="% Cover", palette="roma") +
#    geom_sf(data=bavaria_laea, fill=NA) + coord_sf() + theme_bw() +
#    theme(strip.background = element_blank(), axis.title = element_blank())

## -----------------------------------------------------------------------------
#  corine_lc_perc_bav$forest <- corine_lc_perc_bav$`Agro-forestry areas` +
#    corine_lc_perc_bav$`Coniferous forest` + corine_lc_perc_bav$`Mixed forest` +
#    corine_lc_perc_bav$`Broad-leaved forest`
#  
#  # Plot individual land-cover percentage cover
#  corine_lc_perc_bav %>% ggplot() + geom_tile(aes(x=x, y=y, fill=forest)) +
#    facet_wrap(.~year) + scale_fill_scico(name="% Cover", palette="roma") +
#    geom_sf(data=bavaria_laea, fill=NA) + coord_sf() + ggtitle("Forest") +
#    theme_bw() + labs(x="", y="") + theme(legend.text = element_text(size=10))

## -----------------------------------------------------------------------------
#  # Calculate dominant land-cover class for 2018
#  dominant_clc <- corine_lc_perc_bav %>%
#    pivot_longer(names_to="var", values_to="value", -c(x,y,year)) %>%
#    pivot_wider(names_from="year", values_from="value") %>%
#    group_by(x,y) %>% slice(which.max(`2018`)) %>%
#    select(x,y,var) %>% mutate(var = as.factor(var))
#  
#  # Plot map
#  clc_cols_sub <- clc_cols[names(clc_cols) %in% dominant_clc$var]
#  dominant_clc %>% ggplot() + geom_tile(aes(x=x, y=y, fill=var)) +
#    scale_fill_manual(name="CLC 2018", values=clc_cols_sub) +
#    geom_sf(data=bavaria_laea, fill=NA) + coord_sf() +
#    labs(x="", y="") + theme_bw() + theme(legend.position="bottom")

## -----------------------------------------------------------------------------
#  data("corine_cha_bav_tk4tel", package="bavDC")
#  corine_cha_bav <- corine_cha_bav_tk4tel %>%
#    unite("9000", c("9000_90", "9000_00"), sep=" - ", na.rm=T) %>%
#    unite("0006", c("0006_00", "0006_06"), sep=" - ", na.rm=T) %>%
#    unite("0612", c("0612_06", "0612_12"), sep=" - ", na.rm=T) %>%
#    unite("1218", c("1218_12", "1218_18"), sep=" - ", na.rm=T)
#  #head(corine_cha_bav)
#  
#  #clc_cols_1990 <- clc_cols[names(clc_cols) %in% corine_lc_bav_tk4tel$`1990`]
#  corine_cha_bav %>% ggplot() + geom_tile(aes (x=x, y=y, fill=`0612`)) +
#    #scale_fill_manual(name="CLC 1990", values=clc_cols_1990) +
#    geom_sf(data=bavaria_gk, fill=NA) + coord_sf() + theme_bw() + labs(x="", y="") +
#    theme(legend.position="bottom", legend.text = element_text(size=6))

## -----------------------------------------------------------------------------
#  rm(list=ls()); invisible(gc())

