#' @title Plotting DARDAR-Nice L3 global maps
#'
#' @param fn File name of the DARDAR-Nice L3 data
#' @param dir_out Directory to save the plots
#' @param t_bins Temperature bins to plot (must be included in the gridded data)
#' @param ni_lim Limits of the color scale for the ice crystal number concentration (in #/L)
#' @param suffix Suffix to add to the file name
#' @export
grid_plot_map <- function(fn, dir_out, t_bins = c(-65, -45, -25), ni_lim = c(0, 300), suffix = NULL) {

  df_darni <- readRDS(fn)

  if(!is.null(suffix)) suffix <- paste0("_", suffix)
  fn_out <- paste0(dir_out, "/", gsub(".rds", "", basename(fn)), suffix, ".pdf")

  ta_lev <- unique(df_darni$ta)
  ta_lev_new <- as.numeric(ta_lev); dta <- (ta_lev_new[2]-ta_lev_new[1])/2
  ta_lev_new <- paste(ta_lev_new-dta,"~to~",ta_lev_new+dta,"*degree*C",sep="")

  zlabel <- expression(paste(N[i]^{5*mu*m}," (#.",L^{-1},")",sep=""))

  p <- df_darni %>%
    filter(cf_ice > 0.001) %>%
    filter(ta %in% t_bins) %>%
    mutate(ta = factor(ta, levels = ta_lev, labels = ta_lev_new),
           icnc_5um = pmin(icnc_5um, ni_lim[2])) %>%
    ggplot() +
    geom_tile(aes(x = lon, y = lat, fill = icnc_5um)) +
    scale_fill_distiller(zlabel, palette = "Spectral", limits = ni_lim) +
    scale_x_continuous(breaks = seq(-180, 180, 60), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-90, 90, 30), expand = c(0, 0)) +
    plotutils::geom_world_polygon() +
    ## plotutils::scale_x_geo() +
    ## plotutils::scale_y_geo() +
    facet_wrap(~ta, labeller = ggplot2::label_parsed) +
    theme(aspect.ratio = 0.7,
          panel.background = ggplot2::element_rect(fill = "gray"))

  ggplot2::ggsave(fn_out, p, width = 8, height = 3)

  return(NULL)
}


#' @title Plotting DARDAR-Nice L3 zonal maps
#'
#' @param fn File name of the DARDAR-Nice L3 data
#' @param dir_out Directory to save the plots
#' @param ni_lim Limits of the color scale for the ice crystal number concentration (in #/L)
#' @param suffix Suffix to add to the file name
#' @export
grid_plot_zonal <- function(fn, dir_out, ni_lim = c(0, 300), suffix = NULL) {

  df_darni <- readRDS(fn)

  if(!is.null(suffix)) suffix <- paste0("_", suffix)
  fn_out <- paste0(dir_out, "/", gsub(".rds", "", basename(fn)), suffix, ".pdf")

  zlabel <- expression(paste(N[i]^{5*mu*m}," (#.",L^{-1},")",sep=""))

  p <- df_darni %>%
    filter(cf_ice > 0.001) %>%
    mutate(icnc_5um = pmin(icnc_5um, ni_lim[2]),
           height = height * 1E-3) %>%
    ggplot() +
    geom_tile(aes(x = lat, y = height, fill = icnc_5um)) +
    scale_fill_distiller(zlabel, palette = "Spectral", limits = ni_lim) +
    scale_y_continuous(limits = c(0, 25), expand = c(0,0)) +
    scale_x_continuous(breaks = seq(-90, 90, 30), expand = c(0, 0)) +
    theme(aspect.ratio = 1,
          panel.background = ggplot2::element_rect(fill = "gray"))

  ggplot2::ggsave(fn_out, p, width = 8, height = 3)

  return(NULL)
}

#' @title Plotting DARDAR-Nice L3 Ni as function of temperature
#'
#' @param fn File name of the DARDAR-Nice L3 data
#' @param dir_out Directory to save the plots
#' @param suffix Suffix to add to the file name
#' @export
grid_plot_ni_ta <- function(fn, dir_out, suffix = NULL) {

  df_darni <- readRDS(fn) %>%
    filter(season == "All" & region == "Global")

  if(!is.null(suffix)) suffix <- paste0("_", suffix)
  fn_out <- paste0(dir_out, "/", gsub(".rds", "", basename(fn)), suffix, ".pdf")

  df_stat <- df_darni %>%
    dplyr::mutate(icnc_5um = icnc_5um * 1E-3,
                  ta = ta + 273.15) %>%
    dplyr::group_by(ta) %>%
    dplyr::summarize(icnc_5um_10 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.10),
                     icnc_5um_25 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.25),
                     icnc_5um_50 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.50),
                     icnc_5um_75 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.75),
                     icnc_5um_90 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.90)) %>%
    data.frame()

  p <- df_darni %>%
    dplyr::mutate(icnc_5um = icnc_5um * 1E-3,
                  ta = ta + 273.15) %>%
    ggplot2::ggplot() +
    ggplot2::geom_raster(aes(x = ta, y = icnc_5um, fill = n_tot)) +
    ggplot2::scale_fill_distiller("Retrievals (#)", palette = "Spectral", direction = -1, na.value = "white") +
    ggplot2::geom_path(data = df_stat, ggplot2::aes(x = ta, y = icnc_5um_10), color = "red", size= 0.4, linetype = 3) +
    ggplot2::geom_path(data = df_stat, ggplot2::aes(x = ta, y = icnc_5um_25), color = "red", size= 0.5, linetype = 2) +
    ggplot2::geom_path(data = df_stat, ggplot2::aes(x = ta, y = icnc_5um_50), color = "red", size= 0.7, linetype = 1) +
    ggplot2::geom_path(data = df_stat, ggplot2::aes(x = ta, y = icnc_5um_75), color = "red", size= 0.5, linetype = 2) +
    ggplot2::geom_path(data = df_stat, ggplot2::aes(x = ta, y = icnc_5um_90), color = "red", size= 0.4, linetype = 3) +
    ggplot2::geom_hline(yintercept = 0.002, color = "blue", size = 0.4, linetype = 3) +
    ggplot2::geom_hline(yintercept = 0.007, color = "blue", size = 0.5, linetype = 2) +
    ggplot2::geom_hline(yintercept = 0.030, color = "blue", size = 0.7, linetype = 1) +
    ggplot2::geom_hline(yintercept = 0.102, color = "blue", size = 0.5, linetype = 2) +
    ggplot2::geom_hline(yintercept = 0.300, color = "blue", size = 0.4, linetype = 3) +
    ggplot2::scale_x_continuous(limits = c(180, 245), expand = c(0, 0), breaks = seq(180, 240, 10)) +
    ggplot2::scale_y_log10(limits = c(1E-3, 1E0), expand = c(0, 0)) +
    ggplot2::labs(x = "Temperature (K)", y = expression(N[ice]~(cm^{-3}))) +
    ggplot2::theme(aspect.ratio = 0.5,
                   panel.background = ggplot2::element_rect(fill = "#f7f7f7"),
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, size=1))

  ggplot2::ggsave(fn_out, p, width = 6, height = 3)

  return(NULL)
}


#' @title Boxplot figure of Ni median for seasons and regions
#'
#' @param fn File name of the DARDAR-Nice L3 data
#' @param dir_out Directory to save the figure
#' @param suffix Suffix to add to the file name
#' @export
grid_plot_box_median <- function(fn, dir_out, suffix = NULL) {

  df_darni <- readRDS(fn)

  if(!is.null(suffix)) suffix <- paste0("_", suffix)
  fn_out <- paste0(dir_out, "/", gsub(".rds", "", basename(fn)), suffix, "_box-median", ".pdf")

  ## Determmine the cumulative weight of each temperature bin
  ## Variable ta_cumsum is added to the dataframe, it can be used to filter the data
  ## e.g. filter(ta_cumsum < 0.5) for the 50% dominant temperature bins
  ## Not used for now, the filter is set to 100% (ta_cumsum < 1)
  df <- df_darni %>%
    dplyr::mutate(n_tot = as.numeric(n_tot)) %>%
    dplyr::group_by(season, region) %>%
    dplyr::mutate(n_tot_all = sum(n_tot)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season, region, ta) %>%
    dplyr::mutate(n_tot_ta = sum(n_tot)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ta_freq = n_tot_ta / n_tot_all) %>%
    dplyr::arrange(season, region, ta_freq) %>%
    dplyr::group_by(season, region, icnc_5um) %>%
    dplyr::mutate(ta_cumsum = cumsum(ta_freq)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n_tot_all, -n_tot_ta, -ta_freq)

  ## Compute the median of Ni for each season and region, as well as the 10th, 25th, 75th and 90th percentiles
  df <- df %>%
    dplyr::filter(ta_cumsum < 1) %>%
    dplyr::mutate(icnc_5um = icnc_5um * 1E-3,
                         n_tot = as.numeric(n_tot)) %>%
    dplyr::group_by(region, season) %>%
    dplyr::summarize(icnc_5um_10 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.10),
                     icnc_5um_25 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.25),
                     icnc_5um_50 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.50),
                     icnc_5um_75 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.75),
                     icnc_5um_90 = Hmisc::wtd.quantile(icnc_5um, weights = n_tot, 0.90)
                     ) %>%
    data.frame()

  ## Plot the boxplot
  p <- df %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = region, middle = icnc_5um_50, lower = icnc_5um_25, upper = icnc_5um_75,
                                       ymin = icnc_5um_10, ymax = icnc_5um_90, color = season), stat = "identity") +
    ggplot2::labs(x = NULL,
                  y = expression(N[ice]~(cm^{-3}))) +
    ggplot2::scale_y_log10(limits = c(1E-3, 1E0), expand = c(0, 0)) +
    ggplot2::annotation_logticks(base = 10, sides = "lr") +
    ggplot2::scale_color_manual(breaks = c("All", "DJF", "MAM", "JJA", "SON"),
                                values = c("black", "#CC79A7", "#52854C", "#D16103", "#00AFBB")) +
    ggplot2::geom_hline(yintercept = 0.007, color = "blue", size=0.5, linetype = 2) +
    ggplot2::geom_hline(yintercept = 0.030, color = "blue", size=0.7, linetype = 1) +
    ggplot2::geom_hline(yintercept = 0.102, color = "blue", size=0.5, linetype = 2) +
    ggplot2::theme(aspect.ratio = 0.5,
                   panel.background = ggplot2::element_rect(fill = "#f7f7f7"),
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1))

  ggplot2::ggsave(fn_out, p, width = 8, height = 4)

  ## Print that the plot has been created
  print(paste0("Plot created: ", fn_out))

  return(NULL)
}
