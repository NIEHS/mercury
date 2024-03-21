#' Download GridMET data
#' #' List of variable abbreviations:
#' sph: (Near-Surface Specific Humidity)
#' vpd: (Mean Vapor Pressure Deficit)
#' pr: (Precipitation)
#' rmin: (Minimum Near-Surface Relative Humidity)
#' rmax: (Maximum Near-Surface Relative Humidity)
#' srad: (Surface Downwelling Solar Radiation)
#' tmmn: (Minimum Near-Surface Air Temperature)
#' tmmx: (Maximum Near-Surface Air Temperature)
#' vs: (Wind speed at 10 m)
#' th: (Wind direction at 10 m)
#' pdsi: (Palmer Drought Severity Index)
#' pet: (Reference grass evaportranspiration)
#' etr: (Reference alfalfa evaportranspiration)
#' erc: (model-G)
#' bi: (model-G)
#' fm100: (100-hour dead fuel moisture)
#' fm1000: (1000-hour dead fuel moisture)
#' @param year the year to download
#' @param var the variable to download (eg: "tmmn", "tmmx", "pr")
#' @param storage_path the path to save the data
#' @export
#' @author Eva Marques
download_gridmet <- function(year, var, storage_path) {
  if (!dir.exists(storage_path)) {
    dir.create(storage_path, recursive = TRUE)
  }
  
  # Create the url for the data
  url <- paste0("http://www.northwestknowledge.net/metdata/data/",
                var,
                "_",
                year,
                ".nc")
  fpath <- paste0(storage_path, "/", var, "_", year, ".nc")
  if (!file.exists(fpath)) {
    downloader::download(
      url = url,
      destfile = fpath,
      mode = "wb"
    )
  }
}
