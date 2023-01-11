## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = T, comment = NA, warning=F, message=F, eval=T, 
  echo=T, error=F, comment = "#>", fig.path="../figures/"
)

## ----pa-overview--------------------------------------------------------------
# Load bavDC & ggplot2 package
library(bavDC); library(dplyr); library(tidyr); library(ggplot2); library(patchwork)

# Load e-obs climate data from bavDC package
data("e-obs_rr_ens_mean_0.1deg_bav_yearmon")
data("e-obs_tg_ens_mean_0.1deg_bav_yearmon")
data("e-obs_tn_ens_mean_0.1deg_bav_yearmon")
data("e-obs_tx_ens_mean_0.1deg_bav_yearmon")

# Load bayWRF monthly temperature data
load(system.file("extdata", "baywrf_pr_bav.rda", package = "bavDC"))
load(system.file("extdata", "baywrf_tas_bav.rda", package = "bavDC"))
load(system.file("extdata", "baywrf_tasmin_bav.rda", package = "bavDC"))
load(system.file("extdata", "baywrf_tasmax_bav.rda", package = "bavDC"))

# Load DWD monthly climate data
load(system.file("extdata", "dwd_yearmon_rsms_bav_tk4tel.rda", package = "bavDC")) # Gridded monthly climate data
load(system.file("extdata", "dwd_yearmon_tadnmm_bav_tk4tel.rda", package = "bavDC"))
load(system.file("extdata", "dwd_yearmon_tadxmm_bav_tk4tel.rda", package = "bavDC"))
load(system.file("extdata", "dwd_yearmon_tamm_bav_tk4tel.rda", package = "bavDC"))

# Load EuroCordex climate simulation data
load(system.file("extdata", "cordex_prAdjust_bav.rda", package = "bavDC"))
load(system.file("extdata", "cordex_tasminAdjust_bav.rda", package = "bavDC"))
load(system.file("extdata", "cordex_tasmaxAdjust_bav.rda", package = "bavDC"))

# WFDE5 re-analysis climate data (0.5°)
data("wfde5_rainf_bav")
data("wfde5_tair_bav") # In Kelvin

## ---- fig.width=9, fig.height=6-----------------------------------------------
## Plot of tmean
baywrf_tmean <- baywrf_tas_bav %>% tidyr::pivot_longer(cols=-c(x,y), names_to="date", values_to="tmean") %>%
  group_by(date) %>% summarise(baywrf=mean(tmean, na.rm=T))
#dwd_yearmon_tadnmm_bav_tk4tel %>% head()
eobs_tmean <- `e-obs_tn_ens_mean_0.1deg_bav_yearmon` %>% 
  tidyr::pivot_longer(cols=-c(x,y), names_to="date", values_to="tmean") %>%
  group_by(date) %>% summarise(`e-obs`=mean(tmean, na.rm=T))
wfde5_tair_bav <- wfde5_tair_bav %>% group_by(x,y) %>% 
  mutate_at(vars(-group_cols()), .funs=function(x){x - 273.15})
wfde5_tmean <- wfde5_tair_bav  %>% 
  tidyr::pivot_longer(cols=-c(x,y), names_to="date", values_to="tmean") %>%
  group_by(date) %>% summarise(wfde5=mean(tmean, na.rm=T))
baywrf_tmean$date <- sub(pattern="[.]", replacement=" ", x=baywrf_tmean$date)
wfde5_tmean$date <- sub(pattern="[.]", replacement=" ", x=wfde5_tmean$date)

#head(baywrf_tmean)
#head(eobs_tmean)
#head(wfde5_tmean)

all_tmean <- full_join(baywrf_tmean, eobs_tmean) %>% full_join(wfde5_tmean) %>% 
  tidyr::pivot_longer(cols=-date, names_to="Dataset", values_to="tmean")
all_tmean$date <- zoo::as.yearmon(all_tmean$date, "%B %Y")

p1 <- all_tmean %>% ggplot(aes(x=date, y=tmean, col=Dataset)) +
  geom_line() + theme_bw()

## Plot of precipitation
baywrf_pr <- baywrf_pr_bav %>% tidyr::pivot_longer(cols=-c(x,y), names_to="date", values_to="pr") %>%
  group_by(date) %>% summarise(baywrf=sum(pr, na.rm=T))
#dwd_yearmon_tadnmm_bav_tk4tel %>% head()
eobs_pr <- `e-obs_rr_ens_mean_0.1deg_bav_yearmon` %>% 
  tidyr::pivot_longer(cols=-c(x,y), names_to="date", values_to="pr") %>%
  group_by(date) %>% summarise(`e-obs`=sum(pr, na.rm=T))
#wfde5_pr_bav <- wfde5_rainf_bav %>% group_by(x,y) %>% 
#  mutate_at(vars(-group_cols()), .funs=function(x){x - 273.15})
wfde5_pr <- wfde5_rainf_bav  %>% 
  tidyr::pivot_longer(cols=-c(x,y), names_to="date", values_to="pr") %>%
  group_by(date) %>% summarise(wfde5=sum(pr, na.rm=T))
cordex_pr <- cordex_prAdjust_bav %>% group_by(x,y,time) %>% summarise(pr=mean(value, na.rm=T)) %>%
  group_by(time) %>% summarise(`euro-cordex`=sum(pr))


baywrf_pr$date <- sub(pattern="[.]", replacement=" ", x=baywrf_pr$date)
wfde5_pr$date <- sub(pattern="[.]", replacement=" ", x=wfde5_pr$date)
cordex_pr$date <- as.character(zoo::as.yearmon(cordex_pr$time))
cordex_pr$year <- lubridate::year(cordex_pr$time)
cordex_pr <- cordex_pr %>% filter(year <= "2022") %>% select(-c(time, year))

#head(baywrf_pr)
#head(eobs_pr)
#head(wfde5_pr)
#head(cordex_pr)

all_pr <- full_join(baywrf_pr, eobs_pr) %>% full_join(wfde5_pr) %>% full_join(cordex_pr) %>% 
  tidyr::pivot_longer(cols=-date, names_to="Dataset", values_to="pr") 
all_pr$date <- zoo::as.yearmon(all_pr$date, "%B %Y")
p2 <- all_pr %>% ggplot(aes(x=date, y=pr, col=Dataset)) +
  geom_line() + theme_bw()

p1 / p2

## -----------------------------------------------------------------------------


## ----cormat, fig.width=6, fig.height=6----------------------------------------
# GGally, to assess the distribution and correlation of variables 
#library(GGally)

# Check correlations (as scatterplots), distribution and print correlation coefficient 
#ggpairs(dwd_annual_ts_bav %>% filter(Jahr >= 1980) %>% select(-Jahr)) 

# Check correlation between variables
#cor(dwd_annual_ts_bav %>% filter(Jahr >= 1980) %>% select(-Jahr)) 

# Nice visualization of correlations
#ggcorr(dwd_annual_ts_bav %>% filter(Jahr >= 1980) %>% select(-Jahr), 
#       nbreaks = 9,method = c("everything", "pearson"), label=T)
#rm(list=ls()); invisible(gc())
