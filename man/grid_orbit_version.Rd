% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_orbit_version.R
\name{grid_orbit_version}
\alias{grid_orbit_version}
\title{Merge orbit-level data from two versions of the DARDAR-Nice data}
\usage{
grid_orbit_version(
  fn_ref,
  v2,
  dir_out,
  bins_ta = seq(-90, 0, by = 10),
  overwrite = FALSE
)
}
\arguments{
\item{fn_ref}{The filename of the reference version of the DARDAR-Nice data.}

\item{v2}{The version number of the second version of the DARDAR-Nice data.}

\item{dir_out}{The directory where the output files will be saved.}

\item{bins_ta}{The bins for the \code{ta} variable. The default is a vector of 10 values from -90 to 0.}

\item{overwrite}{Logical. If \code{TRUE}, the function will overwrite any existing files. The default is \code{FALSE}.}
}
\description{
This function compares two versions of the DARDAR-Nice data. The
function reads the data from the two versions, and then creates a icnc vs icnc grid of the data.
The function compute counts of the number of data points in each grid cell. This is done
for multiple temperature ranges `bins_ta`.
The grid is saved as an RDS file.
}
