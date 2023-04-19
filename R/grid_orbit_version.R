#' @title Grid DARDAR-Nice data from an orbit file between two versions
#'
#' @export
grid_orbit_version <- function(fn_ref, v2, dir_out, bins_ta = seq(-90, 0, by = 10), overwrite = FALSE) {

  ## Identify the filename for both versions
  version_ref <- stringr::str_extract(fn_ref, "(?<=v)\\d+\\.\\d+")
  fn_2 <- gsub(paste0("v", version_ref), paste0("v", v2), fn_ref)
  if(! (file.exists(fn_ref) & file.exists(fn_2)) ) return(NULL)

  ## File and directory for gridded version comparison
  dir_orbit_scat <- paste0(dir_out, "/version_scat", "_", v2, "/orbits")
  fn_orbit_scat <- paste0(dir_orbit_scat, "/", gsub(".nc", ".rds", basename(fn_ref)))

  ## Create directories if they don't exist
  directories <- c(dir_orbit_scat)
  null <- plyr::ldply(directories, dir.create, showWarnings = FALSE, recursive = TRUE)

  ## Check if files exist. If they do and overwrite = FALSE, function returns NULL.
  files <- c(fn_orbit_scat)
  if(all(file.exists(files)) & !overwrite) return(NULL)

  ## Load the DARDAR-Nice data
  df_ref <- darnitools::l2_read(fn_ref, filter_ice = TRUE, filter_quality = TRUE) %>%
    dplyr::select(c(time, height, icnc_5um, icnc_100um, ta))

  df_2 <- darnitools::l2_read(fn_2, filter_ice = TRUE, filter_quality = TRUE) %>%
    dplyr::select(c(time, height, icnc_5um, icnc_100um, ta))

  df_darni <- dplyr::left_join(df_ref, df_2, by = c("time", "height")) %>%
    dplyr::rename(icnc_5um_ref = icnc_5um.x,
                  icnc_5um_2 = icnc_5um.y,
                  ta = ta.x)

  bins_icnc <- 10^seq(-2, 5, by = 0.05)

  df_grid <- df_darni %>%
    plotutils::bin(ta, bins_ta) %>%
    plotutils::bin(icnc_5um_ref, bins_icnc) %>%
    plotutils::bin(icnc_5um_2, bins_icnc) %>%
    dplyr::filter(!is.na(icnc_5um_ref_bin) & !is.na(icnc_5um_2_bin) & !is.na(ta_bin)) %>%
    dplyr::group_by(icnc_5um_ref_bin, icnc_5um_2_bin, ta_bin, .drop = "keep") %>%
    dplyr::summarize(count = n()) %>%
    data.frame() %>%
    dplyr::rename(icnc_5um_ref = icnc_5um_ref_bin,
                  icnc_5um_2 = icnc_5um_2_bin,
                  ta = ta_bin)

  saveRDS(df_grid, fn_orbit_scat)

  rm(df_grid, df_ref, df_2)
  gc()

  return(NULL)

}
