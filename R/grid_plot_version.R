#' @export
grid_plot_zonal_versions <- function(fn_v1, fn_v2, dir_out) {

  darni_version_1 <- stringr::str_extract(fn_v1, "(?<=v)\\d+\\.\\d+")
  darni_version_2 <- stringr::str_extract(fn_v2, "(?<=v)\\d+\\.\\d+")

  fn_out <- paste0(dir_out, "/DARNI_PRO_L3_v", darni_version_2, "-v", darni_version_1, "_zonal.pdf")

  df_darni_1 <- readRDS(fn_v1) %>% filter(cf_ice > 0.001) %>% dplyr::rename(icnc_5um_1 = icnc_5um)
  df_darni_2 <- readRDS(fn_v2) %>% filter(cf_ice > 0.001) %>% dplyr::rename(icnc_5um_2 = icnc_5um)

  df_darni <- dplyr::left_join(df_darni_1, df_darni_2, by = c("lat", "height"))

  zlabel <- expression(paste(Delta*N[i]^{5*mu*m}," (%)",sep=""))
  zlim <- c(-100, 100)

  p <- df_darni %>%
    dplyr::mutate(dicnc = (icnc_5um_2 - icnc_5um_1) / icnc_5um_2 * 100,
                  dicnc = pmin(dicnc, zlim[2]),
                  dicnc = pmax(dicnc, zlim[1])) %>%
    mutate(height = height * 1E-3) %>%
    ggplot() +
    geom_tile(aes(x = lat, y = height, fill = dicnc)) +
    scale_fill_distiller(zlabel, palette = "RdBu", limits = zlim) +
    scale_y_continuous(limits = c(0, 25), expand = c(0,0)) +
    scale_x_continuous(breaks = seq(-90, 90, 30), expand = c(0, 0)) +
    theme(aspect.ratio = 1,
          panel.background = ggplot2::element_rect(fill = "gray"))

  ggplot2::ggsave(fn_out, p, width = 8, height = 3)

  return(NULL)

}

#' @export
grid_plot_map_versions <- function(fn_v1, fn_v2, dir_out, t_bins = c(-65, -45, -25)) {

  darni_version_1 <- stringr::str_extract(fn_v1, "(?<=v)\\d+\\.\\d+")
  darni_version_2 <- stringr::str_extract(fn_v2, "(?<=v)\\d+\\.\\d+")

  fn_out <- paste0(dir_out, "/DARNI_PRO_L3_v", darni_version_2, "-v", darni_version_1, "_map.pdf")

  df_darni_1 <- readRDS(fn_v1) %>% filter(cf_ice > 0.001) %>% dplyr::rename(icnc_5um_1 = icnc_5um)
  df_darni_2 <- readRDS(fn_v2) %>% filter(cf_ice > 0.001) %>% dplyr::rename(icnc_5um_2 = icnc_5um)

  df_darni <- dplyr::left_join(df_darni_1, df_darni_2, by = c("lat", "lon", "ta"))

  ta_lev <- unique(df_darni$ta)
  ta_lev_new <- as.numeric(ta_lev); dta <- (ta_lev_new[2]-ta_lev_new[1])/2
  ta_lev_new <- paste(ta_lev_new-dta,"~to~",ta_lev_new+dta,"*degree*C",sep="")

  zlabel <- expression(paste(Delta*N[i]^{5*mu*m}," (%)",sep=""))
  zlim <- c(-100, 100)

  p <- df_darni %>%
    dplyr::mutate(dicnc = (icnc_5um_2 - icnc_5um_1) / icnc_5um_2 * 100,
                  dicnc = pmin(dicnc, zlim[2]),
                  dicnc = pmax(dicnc, zlim[1])) %>%
    filter(ta %in% t_bins) %>%
    mutate(ta = factor(ta, levels = ta_lev, labels = ta_lev_new)) %>%
    ggplot() +
    geom_tile(aes(x = lon, y = lat, fill = dicnc)) +
    scale_fill_distiller(zlabel, palette = "RdBu", limits = zlim) +
    scale_x_continuous(breaks = seq(-180, 180, 60), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-90, 90, 30), expand = c(0, 0)) +
    plotutils::geom_world_polygon() +
    ## plotutils::scale_x_geo() +
    ## plotutils::scale_y_geo() +
    facet_wrap(~ta,labeller = ggplot2::label_parsed) +
    theme(aspect.ratio = 0.7,
          panel.background = ggplot2::element_rect(fill = "gray"))

  ggplot2::ggsave(fn_out, p, width = 8, height = 3)

  return(NULL)
}
