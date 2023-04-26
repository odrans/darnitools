#' @title Grid DARDAR-Nice data from L2 files
#'
#' @description
#' This function takes a DARDAR-Nice L2 file and creates gridded data:
#' - global distributions of icnc_5um and icnc_100um (stored in dir_out/map/orbits)
#' - zonal distribtuions of icnc_5um and icnc_100um (stored in dir_out/zonal/orbits)
#' - Ni as function of T (stored in dir_out/ni_ta/orbits)
#' File function creates *.rds files that will need to be merged.
#'
#' @param fn Name of the DARDAR-Nice L2 file
#' @param dir_out Directory used to output the gridded data
#' @param overwrite Overwrite existing deata. Default = FALSE
#' @export
grid_orbit <- function(fn, dir_out, overwrite = FALSE) {

  ## File and directory for gridded maps
  dir_orbit_map <- paste0(dir_out, "/map/orbits")
  fn_orbit_map <- paste0(dir_orbit_map, "/", gsub(".nc", ".rds", basename(fn)))

  ## File and directory for gridded zonal
  dir_orbit_zonal <- paste0(dir_out, "/zonal/orbits")
  fn_orbit_zonal <- paste0(dir_orbit_zonal, "/", gsub(".nc", ".rds", basename(fn)))

  ## File and directory for gridded Ni-T
  dir_orbit_ni_ta <- paste0(dir_out, "/ni_ta/orbits")
  fn_orbit_ni_ta <- paste0(dir_orbit_ni_ta, "/", gsub(".nc", ".rds", basename(fn)))

  ## Create directories if they don't exist
  directories <- c(dir_orbit_map, dir_orbit_zonal, dir_orbit_ni_ta)
  null <- plyr::ldply(directories, dir.create, showWarnings = FALSE, recursive = TRUE)

  ## Check if files exist. If they do and overwrite = FALSE, function returns NULL.
  files <- c(fn_orbit_map, fn_orbit_zonal, fn_orbit_ni_ta)
  if(all(file.exists(files)) & !overwrite) return(NULL)

  ## Load the DARDAR-Nice data
  df <- darnitools::l2_read(fn, filter_ice = FALSE, filter_quality = TRUE) %>%
    dplyr::select(c(ta, icnc_5um, icnc_100um, lat, lon, height, flag_ice, season, region))

  ## Run the gridding functions
  null <- grid_orbit_map(df, fn_orbit_map)
  null <- grid_orbit_zonal(df, fn_orbit_zonal)
  null <- grid_orbit_ni_ta(df, fn_orbit_ni_ta, season_region = TRUE)

  return(NULL)

}

#' @title Grid Ni as function of temperature
grid_orbit_ni_ta <- function(df, fn_out,
                             bins_ni = 10^seq(-3, 5, by = 0.05),
                             bins_ta = seq(-90, 0, by = 1),
                             season_region = FALSE) {

  ## If season_region = TRUE, then the data is gridded by season and region
  if(season_region) {
    df <- rbind(df, df %>% dplyr::mutate(region = "Global"))
    df <- rbind(df, df %>% dplyr::mutate(season = "All"))
  } else {
    df <- df %>% dplyr::mutate(region = factor("Global"),
                               season = factor("All"))
  }

  df_grid <- df %>%
    dplyr::filter(flag_ice) %>%
    dplyr::filter(ta >= first(bins_ta) & ta <= last(bins_ta) &
                  icnc_5um >= first(bins_ni) & icnc_5um <= last(bins_ni)) %>%
    plotutils::bin(ta, bins_ta) %>%
    plotutils::bin(icnc_5um, bins_ni) %>%
    dplyr::group_by(ta_bin, region, season) %>%
    dplyr::mutate(n_ta_bin = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ta_bin, icnc_5um_bin, region, season) %>%
    dplyr::summarize(n_tot = n(),
                     n_ta_bin = unique(n_ta_bin),
                     .groups = "keep") %>%
    data.frame() %>%
    dplyr::rename(ta = ta_bin, icnc_5um = icnc_5um_bin)

  saveRDS(df_grid, fn_out)
  rm(df_grid); gc()

  return(NULL)

}

#' @title Grid zonal maps of Ni
grid_orbit_zonal<- function(df, fn_out,
                       bins_lat = seq(-90, 90, by = 2)
                       ) {

  df_grid <- df %>%
    dplyr::filter(lat >= first(bins_lat) & lat <= last(bins_lat)) %>%
    plotutils::bin(lat, bins_lat) %>%
    dplyr::group_by(height, lat_bin) %>%
    dplyr::summarize(n_tot = n(),
                     n_ice = length(which(flag_ice)),
                     icnc_5um_sum = sum(icnc_5um[flag_ice]),
                     icnc_5um_mean_orbit = mean(icnc_5um[flag_ice]),
                     icnc_5um_median_orbit = median(icnc_5um[flag_ice]),
                     icnc_100um_sum = sum(icnc_100um[flag_ice]),
                     icnc_100um_mean_orbit = mean(icnc_100um[flag_ice]),
                     icnc_100um_median_orbit = median(icnc_100um[flag_ice]),
                     .groups = "keep") %>%
    data.frame() %>%
    dplyr::mutate(cf_ice = n_ice / n_tot) %>%
    dplyr::rename(lat = lat_bin)

  saveRDS(df_grid, fn_out)
  rm(df_grid); gc()

  return(NULL)

}

#' @title Grid distributions of Ni
grid_orbit_map <- function(df, fn_out,
                     bins_lat = seq(-90, 90, by = 2),
                     bins_lon = seq(-180, 180, by = 2),
                     bins_ta = seq(-90, 0, by = 10)
                     ) {

  df_grid <- df %>%
    dplyr::filter(lon >= first(bins_lon) & lon <= last(bins_lon) &
                  lat >= first(bins_lat) & lat <= last(bins_lat) &
                  ta >= first(bins_ta) & ta <= last(bins_ta)) %>%
    plotutils::bin(lat, bins_lat) %>%
    plotutils::bin(lon, bins_lon) %>%
    plotutils::bin(ta, bins_ta) %>%
    dplyr::group_by(lon_bin, lat_bin, ta_bin) %>%
    dplyr::summarize(n_tot = n(),
                     n_ice = length(which(flag_ice)),
                     icnc_5um_sum = sum(icnc_5um[flag_ice]),
                     icnc_5um_mean_orbit = mean(icnc_5um[flag_ice]),
                     icnc_5um_median_orbit = median(icnc_5um[flag_ice]),
                     icnc_100um_sum = sum(icnc_100um[flag_ice]),
                     icnc_100um_mean_orbit = mean(icnc_100um[flag_ice]),
                     icnc_100um_median_orbit = median(icnc_100um[flag_ice]),
                     .groups = "keep") %>%
    data.frame() %>%
    dplyr::mutate(cf_ice = n_ice / n_tot) %>%
    dplyr::rename(lat = lat_bin, lon = lon_bin, ta = ta_bin)

  saveRDS(df_grid, fn_out)
  rm(df_grid); gc()

  return(NULL)

}
