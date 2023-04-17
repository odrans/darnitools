#' @title Read DARDAR-Nice L2 files
#'
#' @description
#' This function extracts DARDAR-Nice L2 files and returns a data frame or saves it as an RDS file.
#' The ice crystal concentration is converted from m-3 to L-1.
#' The temperature is converted from K to C.
#' The ice water content is converted from kg/m3 to mg/m3.
#' The effective radius is converted from m to um.
#'
#' @param fn Name of the DARDAR-Nice L2 file.
#' @param dir_rds Save output as an RDS file instead of returning the data. Default: NULL (return data).
#' @param filter_ice If TRUE, filter data to only include ice. Default: TRUE.
#' @param filter_quality Apply quality filters. Default: TRUE.
#' @param filter_cloudtop If TRUE, filter data to only include cloud top (first 180-m of uppermost ice layer). Default: FALSE.
#' @param keep_index If TRUE, keep index columns (idx_height and idx_time). Default: FALSE.
#' @return A data frame containing the extracted and processed data or NULL if saved as RDS.
#' @export
l2_read <- function(fn, dir_rds = NULL, filter_ice = TRUE, filter_quality = TRUE, filter_cloudtop = FALSE, keep_index = FALSE) {

  darni_version <- as.numeric(stringr::str_extract(fn, "(?<=v)\\d+\\.\\d+"))

  ## Error handling
  if (!is.character(fn) || length(fn) != 1) {
    stop("The 'fn' parameter must be a single character string representing the file name.")
  }

  ## Check if file exists
  if (!file.exists(fn)) {
    stop("The input file does not exist.")
  }

  if(!grepl("DARNI_PRO_L2", basename(fn)))

  ## If return RDS file, check if directory and file exist
  if (!is.null(dir_rds)) {
    fn_out <- paste0(dir_rds, "/", basename(fn), ".rds")
    if (!dir.exists(dir_rds)) dir.create(dir_rds)

    if (file.exists(fn_out)) return(NULL)
  }

  ## Defined regions based on latitudes
  bins_lat_lev <- c(-90, -67.7, -23.3, 23.3, 67.7, 90)
  bins_lat_lab <- c("Antarctica", "Mid-lat South", "Tropics", "Mid-lat North", "Arctic")

  nc <- ncdf4::nc_open(fn)
  nheight <- nc$dim$height$len
  ntime <- nc$dim$time$len

  ## Read the temperature, ice concentration, cloud mask, mixed phase flag, and ice water content
  tmp <- data.frame(expand.grid(idx_height = 1:nheight,
                                idx_time = 1:ntime),
                    ta = nc_read(nc, "ta"),
                    icnc_5um = nc_read(nc, "icnc_5um"),
                    clm = nc_read(nc, "clm"),
                    flag_mixed = nc_read(nc, "mixedphase_flag"),
                    iwc = nc_read(nc, "iwc")
                    ) %>%
    dplyr::mutate(iteration_flag = nc_read(nc, "iteration_flag", idx_time),
                  idx = 1:n()) %>%
    dplyr::filter(!is.na(ta))

  ## Apply quality filtering: number of iterations > 1
  if (filter_quality) {
    tmp <- tmp %>%
      dplyr::filter(iteration_flag == 1) %>%
      dplyr::select(-c(iteration_flag))
  }

  ## Define ice clouds as clouds with ice concentration > 0 m-3, ice water content > 1E-8 kg/m3, simplified cloud mask = 1
  tmp <- tmp %>%
    dplyr::mutate(flag_ice = (icnc_5um > 0 & iwc > 1E-8 & clm == 1 & flag_mixed == 0 & ta < 270),
                  icnc_5um = replace(icnc_5um, !flag_ice, 0)) %>%
    dplyr::filter(!is.na(icnc_5um) & !is.na(clm))

  ## Filter to only include ice clouds if requested
  if (filter_ice) {
    tmp <- tmp %>%
      dplyr::filter(flag_ice) %>%
      dplyr::select(-c(flag_ice))
  }

  ## Read layer and position information; filter to only include cloud top if requested
  tmp <- tmp %>% dplyr::mutate(dz_top = nc_read(nc, "dz_top", idx),
                               layer_index = nc_read(nc, "layer_index", idx))
  if (filter_cloudtop) {
    tmp <- dplyr::filter(tmp, layer_index == 1L & dz_top <= 180) %>%
      dplyr::select(-c(layer_index))
  }

  ## Read other variables
  tmp <- tmp %>%
    dplyr::mutate(lat = nc_read(nc, "lat", idx_time),
                  lon = nc_read(nc, "lon", idx_time),
                  height = nc_read(nc, "height", idx_height),
                  time = as.POSIXct(nc_read(nc, "base_time"), tz = "UTC", origin = "1970-01-01") + nc_read(nc, "dtime", idx_time),
                  precipitation_flag = nc_read(nc, "precipitation_flag", idx_time),
                  nightday_flag = nc_read(nc, "nightday_flag", idx_time),
                  plev = nc_read(nc, "plev", idx),
                  reff = nc_read(nc, "reffcli", idx),
                  icnc_100um = nc_read(nc, "icnc_100um", idx),
                  land_water_mask = nc_read(nc, "land_water_mask", idx_time)
                  )

  ## Read the extra cloud mask depending on the version
  if(darni_version >= 2.0) {
    tmp <- tmp %>% mutate(clm_full = nc_read(nc, "clm_full", idx))
  } else {
    tmp <- tmp %>% mutate(clm_full = nc_read(nc, "clm_v2", idx))
  }

  ## Convert some units
  tmp <- tmp %>%
    dplyr::mutate(ta = ta - 273.15,                 ## K to C
                  icnc_5um = icnc_5um * 1E-3,       ## m-3 to L-1
                  icnc_100um = icnc_100um * 1E-3,   ## m-3 to L-1
                  iwc = iwc * 1E6,                  ## kg/m3 to mg/m3
                  reff = reff * 1E6                 ## m to um
                  )

  ## Remove the indexes unless requested
  if (!keep_index) {
    tmp <- tmp %>% dplyr::select(-c(idx_height, idx_time, idx))
  }

  ## Process some extra information (regions, seasons, night/day)
  df <- tmp %>%
    plotutils::bin(lat, bins_lat_lev) %>%
    plotutils::bin(nightday_flag, c(0, 0.5, 1)) %>%
    dplyr::mutate(region = factor(lat_bin, levels = (dplyr::lead(bins_lat_lev) + bins_lat_lev) / 2, labels = bins_lat_lab),
                  season = baseutils::time2season(time),
                  nightday = factor(nightday_flag_bin, levels = c(0.25, 0.75), labels = c("day", "night"))) %>%
    dplyr::select(-c(lat_bin, lat_width, nightday_flag_bin, nightday_flag_width, nightday_flag))

  ncdf4::nc_close(nc)

  rm(tmp); gc()

  if(!is.null(dir_rds)) {
    saveRDS(df,fn_out)
    rm(df); gc()
    return(NULL)
  } else {
    return(df)
  }

}

#' @title Little tool to read netCDF files consistently
#'
#' @description This function is a wrapper around ncdf4::ncvar_get() to read netCDF files in a short and consistent way.
#' Each variables are transformed into 1D numeric vectors to be used in a data frame.
#' If the variable is 2D, the function can use the index of a specific dimension to fist extrapolate 1D variables into 2D grid before transforming them into 1D vectors.
#' The index can also be used to find specific values after the initial data was filtered.
#'
#' @param nc A netCDF file opened with ncdf4::nc_open()
#' @param varname The name of the variable to read
#' @param idx The specific index to associate. Default: NULL (output data as it is read)
#' @return A numeric vector corresponding to the netCDF variable
nc_read <- function(nc, varname, idx = NULL) {
  if(is.null(idx)) {
    as.numeric(ncdf4::ncvar_get(nc, varname))
  } else {
    as.numeric(ncdf4::ncvar_get(nc, varname)[idx])
  }
}
