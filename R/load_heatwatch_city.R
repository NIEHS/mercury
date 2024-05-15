#' Create heatwatch dictionary as a data.frame
#' @param storage_dir The path to the heatwatch rasters and traverses shp
#' @return A data frame with the heatwatch dictionary
#' @export
#' @author Eva Marques
create_heatwatch_dictionary <- function(storage_dir) {
  dirs <- list.dirs(storage_dir, full.names = TRUE, recursive = FALSE)
  # select dirs containing "rasters_chw_"
  dirs_rast <- dirs[grep("rasters_chw_", dirs)]
  # select dirs containing "traverses_chw_"
  dirs_trav <- dirs[grep("traverses_chw_", dirs)]
  # extract cities name
  cities_rast <- gsub(".*rasters_chw_(.*)_[0-9]{6,8}.*", "\\1", dirs_rast)
  cities_trav <- gsub(".*traverses_chw_(.*)_[0-9]{6,8}.*", "\\1", dirs_trav)
  stopifnot(all(cities_rast == cities_trav))
  # replace " " with "_"
  cities <- gsub(" ", "_", cities_rast)
  info <- list()
  for (i in 1:length(cities)) {
    c <- cities[i]
    for (t in c("am", "af", "pm")) {
      # raster file name of the city
      # find the file in the folder that contains "t_f" and ".tif"
      files <- list.files(dirs_rast[i])
      file <- files[grep(paste0(t, "_t_f(.*).tif$"), files)]
      if (length(file) != 1) {
        message(paste0("File rast for ",
                       c,
                       " ",
                       t,
                       " not found or several files found"))
        rast_file <- NA
      } else {
        rast_file <- paste0(dirs_rast[i], "/", file)
      }
      # traverse file name of the city
      trav_file <- paste0(dirs_trav[i], "/", t, "_trav.shp")
      if (!file.exists(trav_file)) {
        message(paste0("File traverse for ",
                       c,
                       " ",
                       t,
                       " not found or several files found"))
        trav_file <- NA
        ts <- NA
        te <- NA
      } else {
        # open vect with terra
        v <- terra::vect(trav_file)
        if (!("datetime" %in% names(v) | "datetim" %in% names(v))) {
          message(paste0("No date available in ", trav_file, "\n"))
          ts <- NA
          te <- NA
        } else {
          if ("datetime" %in% names(v)) {
            # store the timestamp in datetime
            ts <- min(v$datetime)
            te <- max(v$datetime)
          } else {
          # store the timestamp in datetim
          ts <- min(v$datetim)
          te <- max(v$datetim)
          }
        }
      }
      # append to info list
      info <- append(
        info,
        list(
          data.frame(
          "city" = c,
          "rast_file" = rast_file,
          "trav_file" = trav_file,
          "moment_of_day" = t,
          "ts" = ts,
          "te" = te
          )
        )
      )
    }
  }
  # append all dataframes in the list
  info <- do.call(rbind, info)
  info$day <- as.Date(info$ts)

  # manually add day for cities where the date is not in traverse files
  info[which(info$city == "iowa_city_cedar_rapids"), ]$day <-
    as.Date("2023-07-22")
  info[which(info$city == "brockton"), ]$day <- as.Date("2023-07-26")
  info[which(info$city == "oklahoma_city"), ]$day <- as.Date("2023-08-12")
  info[which(info$city == "chicago"), ]$day <- as.Date("2023-07-28")
  info[which(info$city == "johnson_wyandotte"), ]$day <- as.Date("2023-08-12")
  info[which(info$city == "scranton"), ]$day <- as.Date("2023-07-26")
  info[which(info$city == "san_francisco"), ]$day <- as.Date("2022-09-02")
  info[which(info$city == "nashville"), ]$day <- as.Date("2022-08-14")
  info[which(info$city == "knoxville"), ]$day <- as.Date("2022-08-27")
  info[which(info$city == "clark_county"), ]$day <- as.Date("2022-08-13")
  info[which(info$city == "columbus"), ]$day <- as.Date("2022-08-12")
  info[which(info$city == "columbia_mo"), ]$day <- as.Date("2022-08-06")
  info[which(info$city == "montgomery_county"), ]$day <- as.Date("2022-08-07")
  info[which(info$city == "spokane"), ]$day <- as.Date("2022-07-16")
  info[which(info$city == "philadelphia"), ]$day <- as.Date("2022-07-30")
  info[which(info$city == "columbia" &
               (info$moment_of_day %in% c("am", "af"))), ]$day <-
    as.Date("2022-08-06")
  info[which(info$city == "columbia" &
               (info$moment_of_day %in% c("pm"))), ]$day <-
    as.Date("2022-08-07")
  info[which(info$city == "brooklyn"), ]$day <- as.Date("2022-07-30")
  info[which(info$city == "boulder"), ]$day <- as.Date("2022-07-22")
  info[which(info$city == "milwaukee"), ]$day <- NA
  # as.Date("2022-07-21 or 22")
  info[which(info$city == "jacksonville"), ]$day <- as.Date("2022-06-18")
  info[which(info$city == "winchester"), ]$day <- as.Date("2021-07-15")
  #write.csv(info,
  #          file = paste0(storage_dir, "heatwatch_dictionary.csv"),
  #          overwrite = TRUE)
  return(info)
}


#' Open heatwatch rasters
load_heatwatch_city <- function(hw_dict, city) {
  if (!(city %in% hw_dict$city)) {
    stop(paste0("City ", city, " not found in heatwatch hw_dictionary."))
  }
  r_path_am <- hw_dict[which(hw_dict$city == city &
    hw_dict$moment_of_day == "am"), ]$rast_file
  r_path_af <- hw_dict[which(hw_dict$city == city &
    hw_dict$moment_of_day == "af"), ]$rast_file
  r_path_pm <- hw_dict[which(hw_dict$city == city &
    hw_dict$moment_of_day == "pm"), ]$rast_file
  t_path_am <- hw_dict[which(hw_dict$city == city &
                               hw_dict$moment_of_day == "am"), ]$trav_file
  t_path_af <- hw_dict[which(hw_dict$city == city &
                               hw_dict$moment_of_day == "af"), ]$trav_file
  t_path_pm <- hw_dict[which(hw_dict$city == city &
                               hw_dict$moment_of_day == "pm"), ]$trav_file
  if (is.na(r_path_am)) {
    message(paste0("No raster file found for ", city, " am."))
    r_am <- NULL
    t_am <- NULL
  } else {
    r_am <- terra::rast(r_path_am)
    if ("temp_f" %in% names(terra::vect(t_path_am))) {
      t_am <- terra::vect(t_path_am)["temp_f"]
    } else if ("t_f" %in% names(terra::vect(t_path_am))) {
      t_am <- terra::vect(t_path_am)["t_f"]
    } else {
      message(paste0("No temperature field found in ", t_path_am))
      t_am <- NULL
    }
  }
  if (is.na(r_path_af)) {
    message(paste0("No raster file found for ", city, " af."))
    r_af <- NULL
    t_af <- NULL
  } else {
    r_af <- terra::rast(r_path_af)
    if ("temp_f" %in% names(terra::vect(t_path_af))) {
      t_af <- terra::vect(t_path_af)["temp_f"]
    } else if ("t_f" %in% names(terra::vect(t_path_af))) {
      t_af <- terra::vect(t_path_af)["t_f"]
    } else {
      message(paste0("No temperature field found in ", t_path_af))
      t_af <- NULL
    }
  }
  if (is.na(r_path_pm)) {
    message(paste0("No raster file found for ", city, " pm."))
    r_pm <- NULL
    t_pm <- NULL
  } else {
    r_pm <- terra::rast(r_path_pm)
    if ("temp_f" %in% names(terra::vect(t_path_pm))) {
      t_pm <- terra::vect(t_path_pm)["temp_f"]
    } else if ("t_f" %in% names(terra::vect(t_path_pm))) {
      t_pm <- terra::vect(t_path_pm)["t_f"]
    } else {
      message(paste0("No temperature field found in ", t_path_pm))
      t_pm <- NULL
    }
  }
  y <- list(
    "city" = city,
    "r_am" = r_am,
    "r_af" = r_af,
    "r_pm" = r_pm,
    "t_am" = t_am,
    "t_af" = t_af,
    "t_pm" = t_pm,
    "day_am" = hw_dict[which(hw_dict$city == city &
      hw_dict$moment_of_day == "am"), ]$day,
    "day_af" = hw_dict[which(hw_dict$city == city &
      hw_dict$moment_of_day == "af"), ]$day,
    "day_pm" = hw_dict[which(hw_dict$city == city &
      hw_dict$moment_of_day == "pm"), ]$day
  )
  return(y)
}
