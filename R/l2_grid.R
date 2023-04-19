#' @title Grid the DARDAR-Nice data
#'
#' @param dir_darni Directory containing DARDAR-Nice L2 data.
#' @param dir_gridded Directory that will contain the gridded product.
#' @param overwrite Overwrite the data if it exists. Default: FALSE.
#' @param nthreads Number of parallel threads. Default: nthreads = 1.
#' @export
l2_grid <- function(dir_darni, dir_gridded, years_only = NULL, version_2 = NULL, overwrite = FALSE, nthreads = 1) {

  ## Set up parallelization if nthreads > 1
  parallel = FALSE
  if(nthreads > 1) {
    require(doMC)
    parallel = TRUE
    doMC::registerDoMC(cores = nthreads)
  }

  ## List of DARDAR-Nice L2 files
  lf <- list.files(dir_darni, recursive = TRUE, full.names = TRUE, pattern = ".nc")

  ## Filter years if requested
  if(!is.null(years_only)) {
    lf <- lf[which(as.numeric(stringr::str_extract(basename(lf), "\\d{4}")) %in% years_only)]
  }

  ## Create one set of gridded files per orbits
  null <- plyr::ldply(lf, darnitools::grid_orbit, dir_out = dir_gridded, overwrite = overwrite, .parallel = parallel)

  ## Create one set of gridded files per orbit comparing two versions
  if(!is.null(version_2)) {
    null <-  plyr::ldply(lf, darnitools::grid_orbit_version, v2 = version_2, dir_out = dir_gridded, overwrite = overwrite, .parallel = parallel)
  }

  ## Merge all the orbital files
  null <- darnitools::grid_merge(dir_gridded)

  ## Merge all files for version comparisons
  if(!is.null(version_2)) {
    null <- darnitools::grid_merge_version(dir_gridded, v2 = version_2)
  }

  ## Uninitialize the doMC backend
  if(parallel) {
    registerDoSEQ()
  }

  return(NULL)

}
