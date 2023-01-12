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
load(system.file("extdata", "dwd_yearmon_rsms_bav_tk4tel.rda", package = "bavDC"))
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

