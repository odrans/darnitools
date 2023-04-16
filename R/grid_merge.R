
#' @title Create gridded DARDAR-Nice data
#'
#' @description
#' Merge the grids from individual orbital files
#'
#' @param dir Directory used to output the gridded data
#' @export
grid_merge <- function(dir_out) {

  ## File and directory for gridded maps
  dir_orbit_map <- paste0(dir_out, "/map")

  ## File and directory for gridded zonal
  dir_orbit_zonal <- paste0(dir_out, "/zonal")

  ## File and directory for gridded Ni-T
  dir_orbit_ni_ta <- paste0(dir_out, "/ni_ta")

  ## Run the gridding functions
  null <- grid_merge_map(dir_orbit_map)
  null <- grid_merge_zonal(dir_orbit_zonal)
  null <- grid_merge_ni_ta(dir_orbit_ni_ta)

  return(NULL)

}



#' @title Create Ni-Ta database
#'
#' @description
#' Merge the files created by `grid_orbit_ni_ta`
#'
#' @param dir Directory used to output the gridded data
grid_merge_ni_ta <- function(dir) {

  ## List all orbit files
  dir_orbits <- paste0(dir, "/orbits")
  lf <- list.files(dir_orbits, full = TRUE)

  ## Set the output file name
  fn_gridded <- gsub("L2", "L3", basename(lf[1]))
  fn_gridded <- gsub("_\\d+\\.rds$", "_ni_ta.rds", fn_gridded)
  fn_gridded <- paste0(dir, "/", fn_gridded)

  ## Load all orbit files
  df <- plyr::ldply(lf, readRDS)

  ## Merge the data
  df_grid <- df %>%
    dplyr::group_by(ta) %>%
    dplyr::group_by(ta, icnc_5um) %>%
    dplyr::mutate(n_ta_bin = sum(n_ta_bin)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ta, icnc_5um) %>%
    dplyr::summarize(n_tot = sum(n_tot),
                     n_ta_bin = unique(n_ta_bin),
                     .groups = "keep") %>%
    data.frame() %>%
    dplyr::mutate(frac = n_tot / n_ta_bin) %>%
    dplyr::select(-c(n_ta_bin))

  ## Write the filename and delete the data
  saveRDS(df_grid, fn_gridded)
  rm(df_grid); gc()

  return(NULL)

}


#' @title Create spatial distributions from DARDAR-Nice L2
#'
#' @description
#' Merge the files created by `grid_orbit_map`
#'
#' @param dir Directory used to output the gridded data
grid_merge_map <- function(dir) {

  ## List all orbit files
  dir_orbits <- paste0(dir, "/orbits")
  lf <- list.files(dir_orbits, full = TRUE)

  ## Set the output file name
  fn_gridded <- gsub("L2", "L3", basename(lf[1]))
  fn_gridded <- gsub("_\\d+\\.rds$", "_map.rds", fn_gridded)
  fn_gridded <- paste0(dir, "/", fn_gridded)

  ## Load all orbit files
  df <- plyr::ldply(lf, readRDS)

  ## Merge the data
  df_grid <- df %>%
    dplyr::mutate(flag_ice = n_ice > 0) %>%
    dplyr::group_by(lat, lon) %>%
    dplyr::summarize(n_tot = sum(n_tot),
                     n_ice = sum(n_ice),
                     icnc_5um_sum = sum(icnc_5um_sum),
                     icnc_5um_mean = mean(icnc_5um_mean_orbit[flag_ice]),
                     icnc_5um_median = median(icnc_5um_median_orbit[flag_ice]),
                     icnc_100um_sum = sum(icnc_100um_sum),
                     icnc_100um_mean = mean(icnc_100um_mean_orbit[flag_ice]),
                     icnc_100um_median = median(icnc_100um_median_orbit[flag_ice]),
                     .groups = "keep") %>%
    data.frame() %>%
    dplyr::mutate(cf_ice = n_ice / n_tot,
                  icnc_5um = icnc_5um_sum / n_ice) %>%
    dplyr::select(-c(icnc_5um_sum, n_ice, n_tot)) %>%
    dplyr::filter(cf_ice > 0)

  ## Write the filename and delete the data
  saveRDS(df_grid, fn_gridded)
  rm(df_grid); gc()

  return(NULL)

}


#' @title Create zonal gridded means from DARDAR-Nice L2
#'
#' @description
#' Merge the files created by `grid_orbit_zonal`
#'
#' @param dir Directory used to output the gridded data
grid_merge_zonal <- function(dir) {

  ## List all orbit files
  dir_orbits <- paste0(dir, "/orbits")
  lf <- list.files(dir_orbits, full = TRUE)

  ## Set the output file name
  fn_gridded <- gsub("L2", "L3", basename(lf[1]))
  fn_gridded <- gsub("_\\d+\\.rds$", "_zonal.rds", fn_gridded)
  fn_gridded <- paste0(dir, "/", fn_gridded)

  ## Load all orbit files
  df <- plyr::ldply(lf, readRDS)

  ## Merge the data
  df_grid <- df %>%
    dplyr::mutate(flag_ice = n_ice > 0) %>%
    dplyr::group_by(lat, height) %>%
    dplyr::summarize(n_tot = sum(n_tot),
                     n_ice = sum(n_ice),
                     icnc_5um_sum = sum(icnc_5um_sum),
                     icnc_5um_mean = mean(icnc_5um_mean_orbit[flag_ice]),
                     icnc_5um_median = median(icnc_5um_median_orbit[flag_ice]),
                     icnc_100um_sum = sum(icnc_100um_sum),
                     icnc_100um_mean = mean(icnc_100um_mean_orbit[flag_ice]),
                     icnc_100um_median = median(icnc_100um_median_orbit[flag_ice]),
                     .groups = "keep") %>%
    data.frame() %>%
    dplyr::mutate(cf_ice = n_ice / n_tot,
                  icnc_5um = icnc_5um_sum / n_ice) %>%
    dplyr::select(-c(icnc_5um_sum, n_ice, n_tot)) %>%
    dplyr::filter(cf_ice > 0)

  ## Write the filename and delete the data
  saveRDS(df_grid, fn_gridded)
  rm(df_grid); gc()

  return(NULL)

}
