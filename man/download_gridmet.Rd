% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_gridmet.R
\name{download_gridmet}
\alias{download_gridmet}
\title{Download GridMET data
#' List of variable abbreviations:
sph: (Near-Surface Specific Humidity)
vpd: (Mean Vapor Pressure Deficit)
pr: (Precipitation)
rmin: (Minimum Near-Surface Relative Humidity)
rmax: (Maximum Near-Surface Relative Humidity)
srad: (Surface Downwelling Solar Radiation)
tmmn: (Minimum Near-Surface Air Temperature)
tmmx: (Maximum Near-Surface Air Temperature)
vs: (Wind speed at 10 m)
th: (Wind direction at 10 m)
pdsi: (Palmer Drought Severity Index)
pet: (Reference grass evaportranspiration)
etr: (Reference alfalfa evaportranspiration)
erc: (model-G)
bi: (model-G)
fm100: (100-hour dead fuel moisture)
fm1000: (1000-hour dead fuel moisture)}
\usage{
download_gridmet(year, var, storage_path)
}
\arguments{
\item{year}{the year to download}

\item{var}{the variable to download (eg: "tmmn", "tmmx", "pr")}

\item{storage_path}{the path to save the data}
}
\description{
Download GridMET data
#' List of variable abbreviations:
sph: (Near-Surface Specific Humidity)
vpd: (Mean Vapor Pressure Deficit)
pr: (Precipitation)
rmin: (Minimum Near-Surface Relative Humidity)
rmax: (Maximum Near-Surface Relative Humidity)
srad: (Surface Downwelling Solar Radiation)
tmmn: (Minimum Near-Surface Air Temperature)
tmmx: (Maximum Near-Surface Air Temperature)
vs: (Wind speed at 10 m)
th: (Wind direction at 10 m)
pdsi: (Palmer Drought Severity Index)
pet: (Reference grass evaportranspiration)
etr: (Reference alfalfa evaportranspiration)
erc: (model-G)
bi: (model-G)
fm100: (100-hour dead fuel moisture)
fm1000: (1000-hour dead fuel moisture)
}
\author{
Eva Marques
}
