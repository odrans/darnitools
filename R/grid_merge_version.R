#' @title Compare two versions of the DARDAR-Nice L2 data using in grid
#'
#' @description This function reads the gridded data created by
#' `grid_orbit_version` and merges all orbit files.
#' A rds file is created with the merged data.
#'
#' @param dir_gridded Directory where the gridded data is stored
#' @param v2 Version number of the second version
#' @export
grid_merge_version <- function(dir_gridded, v2) {

  ## File and directory for gridded maps
  dir_scat <- paste0(dir_gridded, "/version_scat_", v2)
  lf <- list.files(dir_scat, full = TRUE, recursive = TRUE)

  ## Set the output file name
  fn_gridded <- gsub("L2", "L3", basename(lf[1]))
  fn_gridded <- gsub("_\\d+\\.rds$", "_version_scat.rds", fn_gridded)
  fn_gridded <- paste0(dir_scat, "/", fn_gridded)

  ## Load all orbit files
  df <- plyr::ldply(lf, readRDS)

  ## Merge the data
  df_grid <- df %>%
    dplyr::group_by(icnc_5um_ref, icnc_5um_2, ta) %>%
    dplyr::summarize(count = sum(count),
                     .groups = "keep") %>%
    data.frame()

  ## Write the filename and delete the data
  saveRDS(df_grid, fn_gridded)
  rm(df_grid); gc()

  return(NULL)

}
