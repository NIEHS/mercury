% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_gridmet.R
\name{load_gridmet}
\alias{load_gridmet}
\title{Load GridMET variable for a day}
\usage{
load_gridmet(dates, area, var, storage_folder)
}
\arguments{
\item{dates}{a date vector}

\item{area}{a polygon representing the area of interest}

\item{var}{character. Variable to load (accepted value: "tmmn", "tmmx")}

\item{storage_folder}{character. Directory where GRIDMET data is stored}
}
\value{
a raster object
}
\description{
Load GridMET variable for a day
}
\author{
Eva Marques
}
