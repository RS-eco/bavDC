bavDC: Bavarian Data Cube
================

## Installation

You can install bavDC from github with:

``` r
# Install remotes if not available
if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install baVDC package from Github
remotes::install_github("RS-eco/bavDC", build_vignettes = T)
```

If package installation with `install_github()` fails, try this as an
alternative:

``` r
tmp_zip <- tempfile(fileext = ".zip")
source_url <- "https://api.github.com/repos/RS-eco/bavDC/zipball/main"
utils::download.file(source_url, destfile=tmp_zip, method="wget")
file.exists(tmp_zip)

remotes::install_local(tmp_zip)
```

After installation, simply load the `bavDC` package:

``` r
library(bavDC)
```

**If you encounter a bug or if you have any problems, please file an
[issue](https://github.com/RS-eco/bavDC/issues) on Github.**

## Shiny app

You can start the baVDC Shiny app, by:

``` r
library(shiny)
runApp()
```

**Please note:** The app currently only supports the `alt_bavtk4tel`
dataset.

## Datasets

The individual datasets can then be loaded, by:

``` r
# Elevation & Terrain (SRTM)
data("alt_bav_tk4tel")

# Outline of Bavaria (GADM)
data("bavaria")

# BayWRF monthly climate data
load(system.file("extdata", "baywrf_pr_bav.rda", package = "bavDC"))
load(system.file("extdata", "baywrf_tas_bav.rda", package = "bavDC"))
load(system.file("extdata", "baywrf_tasmin_bav.rda", package = "bavDC"))
load(system.file("extdata", "baywrf_tasmax_bav.rda", package = "bavDC"))

# BioTime data for Germany
data("biotime_deu")

# BirdLife, IUCN and GARD range data
data("amphibians_bav")
data("bird_bav")
data("gard_reptiles_bav")
load(system.file("extdata", "mammals_bav.rda", package = "bavDC"))
data("odonata_bav")
data("reptiles_bav")

# Bird range data from the LFU
data("bird_bva_shape") # sf-object
data("bird_bva_shape_tk4tel") #gridded

# Bird and Odonata Atlas data
data("aves_tk4tel")
data("odonata_tk4tel")

# Carbon stock data
data("carbon_bav")

# CCI land cover data
data("cci_bav_tk4tel")

# Chelsa climate data
data("chelsa_bav_tk4tel")

# Euro-cordex climate simulations
data("cordex_bioclim_bav_tk4tel")
data("cordex_bioclim_bav")
load(system.file("extdata", "cordex_prAdjust_bav.rda", package = "bavDC"))
load(system.file("extdata", "cordex_tasminAdjust_bav.rda", package = "bavDC"))
load(system.file("extdata", "cordex_tasmaxAdjust_bav.rda", package = "bavDC"))

#The analysis of all monthly Euro-Cordex climate scenarios can be found here: https://raw.githubusercontent.com/RS-eco/bavDC/main/vignettes/bavaria-eurocordex.html

#And the corresponding .Rmd-file is available from here: https://raw.githubusercontent.com/RS-eco/bavDC/main/vignettes/bavaria-eurocordex.Rmd

# Corine land-cover and land cover change
data("corine_cha_bav_tk4tel") # Land-cover change
data("corine_cha_bav")
data("corine_cha_perc_bav")
data("corine_lc_bav_tk4tel") # Land cover
data("corine_lc_bav") # Corine Land cover data at 100 x 100 m resolution
data("corine_lc_perc_bav") # Corine land cover percentage data at 1 x 1 km resolution

# Gridded population data for Bavaria at 1km resolution
data("bav_pop_1km")

# Diva shapefiles for Germany
data("diva_cover_deu") # Land-cover data
data("diva_pop_deu") # Population data
data("diva_rails_deu") # Railway tracks
data("diva_roads_deu") # Roads
data("diva_water_areas_deu") # Lakes
data("diva_water_lines_deu") # Rivers

# DWD climate data
data("dwd_annual_ts_bav") # Annual time series data of whole of Bavaria
load(system.file("extdata", "dwd_yearmon_rsms_bav_tk4tel.rda", package = "bavDC")) # Gridded monthly climate data
load(system.file("extdata", "dwd_yearmon_tadnmm_bav_tk4tel.rda", package = "bavDC"))
load(system.file("extdata", "dwd_yearmon_tadxmm_bav_tk4tel.rda", package = "bavDC"))
load(system.file("extdata", "dwd_yearmon_tamm_bav_tk4tel.rda", package = "bavDC"))

# E-OBS observed climate data
data("e-obs_elev_ens_0.1deg_bav")
data("e-obs_rr_ens_mean_0.1deg_bav_yearmon")
data("e-obs_rr_ens_spread_0.1deg_bav_yearmon")
data("e-obs_tg_ens_mean_0.1deg_bav_yearmon")
data("e-obs_tg_ens_spread_0.1deg_bav_yearmon")
data("e-obs_tn_ens_mean_0.1deg_bav_yearmon")
data("e-obs_tn_ens_spread_0.1deg_bav_yearmon")
data("e-obs_tx_ens_mean_0.1deg_bav_yearmon")
data("e-obs_tx_ens_spread_0.1deg_bav_yearmon")

# EBCC data of Bavaria
data("ebcc_bav")

# EU-DEM elevation & hillshade data
data("eu_dem_bav_500m")
data("hillshade_bav_500m")

# EuroLST Land surface temperature
data("eurolst_bav_tk4tel")

# EWEMBI daily temperature and precipitation
data("ewembi_bav")

# EWEMBI daily temperature and precipitation data for the whole of Germany can be extracted from the processNC package (https://github.com/RS-eco/processNC)

# List daily temperature files for Germany from 1979 till 2016
ewembi_tas_ger <- list.files(paste0(system.file(package="processNC"), "/extdata"), 
                    pattern="tas.*\\.nc", full.names=T)

# List daily precipitation files for Germany from 1979 till 2016
ewembi_pr_ger <- list.files(paste0(system.file(package="processNC"), "/extdata"), 
                    pattern="tas.*\\.nc", full.names=T)

# Forest disturbance data
data("forest_disturbance_bav_300m")

# GIMMS3g NDVI data
data("gimms3g_v0_bav")
data("gimms3g_v1_bav")

# Globcover land cover
data("globcover_bav_tk4tel")

# Human footprint
data("hfp_1993_v3_bav")
data("hfp_2009_v3_bav")

# Important bird areas
data("iba_bav")

# ISIMIP2b climate and land-use
data("isimip_bio_bav_tk4tel")
data("isimip_lu_bav_tk4tel")

# KK09, KK10 data
data("kk09_bav")
data("kk10_bav")

# Lakes & Rivers
data("lakes_points_bav")
data("lakes_poly_bav")
data("rivers_bav")

# Landsystem data
data("landsystem_bav_tk4tel")
data("landsystem_bav")
data("landsystem_perc_bav_tk4tel")
data("landsystem_perc_bav")

# Merraclim climate
data("merraclim_2.5m_bav_tk4tel")
data("merraclim_5m_bav_tk4tel")
data("merraclim_10m_bav_tk4tel")

# MODIS land-cover
data("modis_lc_bav_tk4tel")

# MODIS Land surface temperature
data("modis_lst_bav_tk4tel")

# Naturraeume (from large to small)
data("ng_geo")
data("ng_gross")
data("ng_ssymank")
data("ng_meynen_schmit")
data("ng_unter_absp")
data("ng_abspziele")

# Protected areas
data("dist_pa_bav_tk4tel")
load(system.file("extdata", "pa_bav.rda", package = "bavDC"))
data("pa_bav_tk4tel")

# Plot coordinates
data("plot_coords_bav")

# Potential natural vegetation
data("pnv_bav")

# Predicts data
data("predicts_deu")

# Standardized taxonomy
data("taxonomyStd")

# Tandem-X Forest/Non-forest
data("tdm_fnf_bav")

# TK25 grid
data("tk4tel_db")
data("tk4tel_grid")
data("tk25_grid")

# Worldclim v1.4
data("wc1.4_10m_bav_tk4tel")
data("wc1.4_2.5m_bav_tk4tel")
data("wc1.4_30s_bav_tk4tel")
data("wc1.4_5m_bav_tk4tel")

# Worldclim v2.0
data("wc2.0_10m_bav_tk4tel")
data("wc2.0_2.5m_bav_tk4tel")
data("wc2.0_30s_bav_tk4tel")
data("wc2.0_5m_bav_tk4tel")

# WFDE5 re-analysis climate data (0.5??)
data("wfde5_rainf_bav")
data("wfde5_snowf_bav")
data("wfde5_tair_bav")
```

**Note:** The code of how the datasets were created can be found in the
[data-raw](https://github.com/RS-eco/baVDC/tree/main/data-raw) folder.

**Note:** Climate, land-use and species data for the whole of Europe can
be found in the [edc](https://github.com/RS-eco/edc) package.

<!--
\begin{frame}
\frametitle{Climate data - Current}
\large{
\begin{itemize}
\item Worldclim v1.4, 1960 - 1990
\vspace{1ex}
\item Worldclim v2, 1970 - 2000, climatologies
\vspace{1ex}
\item Chelsa, 1979 - 2013, monthly %What is the time frame of the climatologies?
\vspace{1ex}
\item MerraClim, 1980, 1990, 2000 %10 year time frames? What is the difference between min, max, average?
\vspace{1ex}
\item EWEMBI, 1979 - 2016, daily
\vspace{1ex}
\item Euro-Cordex, 1950 - 2100  , monthly
\vspace{1ex}
\item EuroLST, ..., 
\vspace{1ex}
\item MODIS LST, ..., monthly
\end{itemize}
}
All products provide tmin, tmax, tmean, prec. Some products also provide additional variables, i.e. wind speed, humidity, ...
\end{frame}
% Is there a study that shows how often certain products have been used in the past?

\begin{frame}
\frametitle{Climate data - Future}
\large{
\begin{itemize}
\item Worldclim v1.4
\vspace{1ex}
\item Worldclim v2 - not yet available
\vspace{1ex}
\item Chelsa
\vspace{1ex}
\item ISIMIP2b
\vspace{1ex}
\item Euro-Cordex
\end{itemize}
}
Currently all based on CMIP5, but CMIP6 for most of these products is in progress.
\end{frame}

\begin{frame}
\frametitle{Land-cover data}
\large{ 
\begin{itemize}
\item MODIS Land-cover: 600 m, global,
\vspace{1ex}
\item Corine Land-cover/Change: 500 m, Europe
\vspace{1ex}
\item CCI Land-cover: ... m, global
\vspace{1ex}
\item Globcover: ... m, global
\end{itemize}
}
All land-cover data is based on real observations, as far as I know no projections of future land-cover are currently available.
\end{frame}
% Also mention forest layer, human settlement layer, urban data from DLR!
% Change to table with table headers!!! Spatial resolution, Spatial extent, Temporal resolution, Temporal coverage, ...
%GLCC???/ Product that Italian guys use!!!

%Also talk about land-use (ISIMIP2b, Hyde, ...)
\begin{frame}
\frametitle{Land-use data}
\large{
\begin{itemize}
\item ISIMIP2b
\vspace{1ex}
\item Hyde
\vspace{1ex}
\item 
\vspace{1ex}
\item 
\end{itemize}
}
% Talk about current and future (Scenarios, ...)
\end{frame}

\begin{frame}
\frametitle{Methods}
\large{
\begin{itemize}
\item Compile data and subset by outline of Bavaria
\vspace{1ex}
\item Resample data to resolution of biodiversity data (TK25 & TK4tel)
\vspace{1ex}
\item Store data in R package (see https://github.com/RS-eco/baVDC)
\vspace{1ex}
\item Create Shiny app for on-the-fly visualisation
\vspace{1ex}
\item Visual and statistic comparison of the different data sources
\vspace{1ex}
\item ...
\end{itemize}
}
\end{frame}
-->
