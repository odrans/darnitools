
#' @export
l2.preview <- function(orbit.start, orbit.end, dir.data, return.data = FALSE) {
  Sys.setenv(TZ = "UTC")
  require(dplyr)

  l.config <- l2.preview.config(orbit.start, orbit.end, dir.data)

  l.config$input <- l2.preview.fileinfo(l.config)
  l.darni <- l2.preview.data(l.config)

  if (return.data) {
    return(l.darni)
  }

  l.plot <- l2.preview.preplot(l.darni, l.config)

  gg.rm.xaxis <- ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())
  gg.rm.yaxis <- ggplot2::theme(axis.title.y = ggplot2::element_blank())

  pdf(NULL)
  g.ret <- gridExtra::gtable_rbind(
    ggplot2::ggplotGrob(l.plot$p.map + ggplot2::ggtitle(paste0("DARDAR-Nice from ", format(orbit.start, "%Y/%m/%d %H:%M:%S"), " to ", format(orbit.end, "%Y/%m/%d %H:%M:%S"))) ),
    ggplot2::ggplotGrob(l.plot$p.dz.top + gg.rm.xaxis),
    ggplot2::ggplotGrob(l.plot$p.iwc + gg.rm.xaxis),
    ggplot2::ggplotGrob(l.plot$p.reff + gg.rm.xaxis),
    ggplot2::ggplotGrob(l.plot$p.icnc.5um + gg.rm.xaxis),
    ggplot2::ggplotGrob(l.plot$p.icnc.100um)
  )

  g.aux <- gridExtra::gtable_rbind(
    ggplot2::ggplotGrob(l.plot$p.precip + gg.rm.xaxis + gg.rm.yaxis),
    ggplot2::ggplotGrob(l.plot$p.mpc.flag + gg.rm.xaxis + gg.rm.yaxis),
    ggplot2::ggplotGrob(l.plot$p.lay.idx + gg.rm.xaxis + gg.rm.yaxis),
    ggplot2::ggplotGrob(l.plot$p.inst.flag + gg.rm.xaxis + gg.rm.yaxis),
    ggplot2::ggplotGrob(l.plot$p.clm.v2 + gg.rm.xaxis + gg.rm.yaxis),
    ggplot2::ggplotGrob(l.plot$p.clm + gg.rm.yaxis)
  )

  g <- gridExtra::gtable_cbind(g.ret, g.aux)

  daynight.darni <- levels(l.darni$df.ta$nightday_flag)
  daynight.idx <- "B"
  if (length(daynight.darni) == 1) {
    if (daynight.darni == "0") daynight.idx <- "D"
    if (daynight.darni == "1") daynight.idx <- "N"
  }

  fn.out <- paste0("~", "/DARDAR-Nice_preview_", format(orbit.start, "%Y%m%d-%H%M%S"), "_", format(orbit.end, "%Y%m%d-%H%M%S"), "_", daynight.idx, ".png")
  h <- 5.2
  g <- icnc::set_panel_size(g = g, height = grid::unit(c(h, h, h, h, h, h), "cm"), width = grid::unit(c(h * 3, h * 3), "cm"), file = fn.out)
  dev.off()

  return()
}


l2.preview.preplot <- function(l.darni, l.config) {
  l <- vector("list")

  gg.xyscale <- function() {
    list(
      ggplot2::scale_y_continuous("Height (km)", limits = l.config$height$limits, expand = c(0, 0)),
      ggplot2::scale_x_datetime("Latitude, Longitude, Time (UTC)", limits = range(l.darni$df.xscale$time), breaks = l.darni$df.xscale$time, labels = l.darni$df.xscale$label, expand = c(0, 0))
    )
  }

  gg.xscale <- function() ggplot2::scale_x_datetime("Latitude, Longitude, Time (UTC)", limits = range(l.darni$df.xscale$time), breaks = l.darni$df.xscale$time, labels = l.darni$df.xscale$label, expand = c(0, 0))

  gg.theme <- function() ggplot2::theme(aspect.ratio = 0.4, legend.justification = c(0, 0.5))

  gg.isoth <- function() {
    list(
      ggplot2::geom_path(data = l.darni$df.ta, ggplot2::aes(x = time, y = height.zero), linetype = 1),
      ggplot2::geom_path(data = l.darni$df.ta, ggplot2::aes(x = time, y = height.hom), linetype = 2)
    )
  }

  l.darni$df.data %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = clm)) +
    ggplot2::scale_fill_manual("Cloud mask", values = l.config$clm$col, labels = l.config$clm$lab, limits = l.config$clm$seq, na.translate = TRUE, na.value = "grey30") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol=2)) -> l$p.clm

  l.darni$df.data %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = clm.v2)) +
    ggplot2::scale_fill_manual("Cloud mask v2", values = l.config$clm.v2$col, labels = l.config$clm.v2$lab, limits = l.config$clm.v2$seq, na.translate = TRUE, na.value = "grey30") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol=2)) -> l$p.clm.v2

  l.darni$df.data %>%
    dplyr::filter(icnc_5um > l.config$icnc.5um$limits[1]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = icnc_5um)) +
    ggplot2::scale_fill_distiller(l.config$icnc.5um$name, palette = l.config$icnc.5um$pal, limits = l.config$icnc.5um$limits) -> l$p.icnc.5um

  l.darni$df.data %>%
    dplyr::filter(icnc_5um > l.config$icnc.5um$limits[1]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = icnc_100um)) +
    ggplot2::scale_fill_distiller(l.config$icnc.100um$name, palette = l.config$icnc.100um$pal, limits = l.config$icnc.100um$limits) -> l$p.icnc.100um

  l.darni$df.data %>%
    dplyr::filter(icnc_5um > l.config$icnc.5um$limits[1]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = iwc)) +
    ggplot2::scale_fill_distiller(l.config$iwc$name, palette = l.config$iwc$pal, limits = l.config$iwc$limits) -> l$p.iwc

  l.darni$df.data %>%
    dplyr::filter(icnc_5um > l.config$icnc.5um$limits[1]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = reff)) +
    ggplot2::scale_fill_distiller(l.config$reff$name, palette = l.config$reff$pal, limits = l.config$reff$limits) -> l$p.reff

  l.darni$df.data %>%
    dplyr::filter(icnc_5um > l.config$icnc.5um$limits[1]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = dz_top)) +
    ggplot2::scale_fill_distiller(l.config$dz_top$name, palette = l.config$icnc.5um$pal, limits = l.config$dz_top$limits) -> l$p.dz.top

  l.darni$df.data %>%
    dplyr::filter(icnc_5um > l.config$icnc.5um$limits[1]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = mixedphase_flag)) -> l$p.mpc.flag

  l.darni$df.data %>%
    dplyr::filter(icnc_5um > l.config$icnc.5um$limits[1]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = instrument_flag)) -> l$p.inst.flag

  l.darni$df.data %>%
    dplyr::filter(icnc_5um > l.config$icnc.5um$limits[1]) %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x = time, y = height, fill = layer_index)) -> l$p.lay.idx

  l.darni$df.ta %>%
    dplyr::filter(flag.ret) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = time, y = precipitation_flag, color = precipitation_flag)) -> l$p.precip

  l$p.clm <- l$p.clm + gg.xyscale() + gg.theme() + gg.isoth()
  l$p.clm.v2 <- l$p.clm.v2 + gg.xyscale() + gg.theme() + gg.isoth()
  l$p.dz.top <- l$p.dz.top + gg.xyscale() + gg.theme() + gg.isoth()
  l$p.mpc.flag <- l$p.mpc.flag + gg.xyscale() + gg.theme() + gg.isoth()
  l$p.inst.flag <- l$p.inst.flag + gg.xyscale() + gg.theme() + gg.isoth()
  l$p.lay.idx <- l$p.lay.idx + gg.xyscale() + gg.theme() + gg.isoth()

  l$p.icnc.5um <- l$p.icnc.5um + gg.xyscale() + gg.theme() + gg.isoth()
  l$p.icnc.100um <- l$p.icnc.100um + gg.xyscale() + gg.theme() + gg.isoth()
  l$p.iwc <- l$p.iwc + gg.xyscale() + gg.theme() + gg.isoth()
  l$p.reff <- l$p.reff + gg.xyscale() + gg.theme() + gg.isoth()

  l$p.iter <- l$p.iter + gg.xscale() + gg.theme()
  l$p.precip <- l$p.precip + gg.xscale() + gg.theme()

  l$p.map <- ggplot2::ggplot() +
    plotutils::geom_world_polygon(fill = "grey", col = "transparent") +
    ggplot2::geom_point(data = l.darni$df.ta, ggplot2::aes(x = lon, y = lat, color = nightday_flag), size = 1) +
    ggplot2::scale_x_continuous("", limits = c(-181, 181), expand = c(0, 0), breaks = seq(-180, 180, 45)) +
    ggplot2::scale_y_continuous("", limits = c(-91, 91), expand = c(0, 0), breaks = seq(-90, 90, 30)) +
    ggplot2::scale_color_manual("Night/day", values = c("0" = "red", "1" = "blue"), labels = c("Day", "Night"), limits = c(0, 1)) +
    ggplot2::theme(aspect.ratio = 0.8, legend.justification = c(0, 0.5))

  return(l)
}



l2.preview.config <- function(orbit.start, orbit.end, dir.data) {
  l <- vector("list")

  l$orbit.start <- orbit.start
  l$orbit.end <- orbit.end
  l$dir.data <- dir.data

  ## Cloud Mask
  l$clm <- list(
    lab = c(
      "Ground", "Uncertain", "Clear", "Ice", "Sc", "Ice + Sc", "Liquid",
      "Rain", "Aerosol", "Insects", "Strat. Feature"
    ),
    col = c(
      "-9" = "black", "-1" = "grey30", "0" = "transparent", "1" = "steelblue2", "2" = "goldenrod1", "3" = "goldenrod3",
      "4" = "olivedrab3", "8" = "cyan", "16" = "gold", "32" = "forestgreen", "16" = "mediumorchid1"
    ),
    seq = c("-9", "-1", "0", "1", "2", "3", "4", "8", "16", "32", "64")
  )


  l$clm.v2 <- list(
    lab = c("ground", "unknown", "clear", "ice", "spher. ice", "sc", "sc + ice",
            "cold rain", "aerosol", "warm rain", "strat. cloud", "high concent. ice", "top of conv. towers", "liquid",
            "warm rain + liq.", "cold rain + liq.", "rain maybe + liq.", "multiple scat."),
    col = c(
      "-9" = "black", "-1" = "grey30", "0" = "transparent", "1" = "steelblue2", "2" = "steelblue1", "3" = "goldenrod2",
      "4" = "goldenrod3", "5" = "hotpink", "6" = "gold", "7" = "brown1", "8" = "mediumorchid1",
      "9" = "steelblue3", "10" = "cyan", "11" = "olivedrab3", "12" = "brown4", "13" = "hotpink4",
      "14" = "mediumorchid1", "15" = "mediumorchid1"),
    seq = c("-9", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15")
  )

  ## Ni > 5um, in #.L-1
  l$icnc.5um <- list(
    name = expression(paste("Ni > 5 ", mu, "m (#.", L^{
      -1
    }, ")", sep = "")),
    pal = "Spectral",
    limits = c(0, 1000)
  )

  ## Ni > 100um, in #.L-1
  l$icnc.100um <- list(
    name = expression(paste("Ni > 100 ", mu, "m (#.", L^{
      -1
    }, ")", sep = "")),
    pal = "Spectral",
    limits = c(0, 100)
  )

  ## Ice water content, in mg.m-3
  l$reff <- list(
    name = expression(paste("Re (", mu, "m)", sep = "")),
    pal = "Spectral",
    limits = c(0, 120)
  )

  ## Ice water content, in mg.m-3
  l$iwc <- list(
    name = expression(paste("IWC (mg.", m^{
      -3
    }, ")", sep = "")),
    pal = "Spectral",
    limits = c(0, 500)
  )

  ## height, in km
  l$height <- list(limits = c(0, 15))

  ## dz_top, in km
  l$dz_top <- list(
    name = "Distance from\nlayer top (km)",
    limits = c(0, 15)
  )

  ## iteration flag
  l$iteration_flag <- list(
    name = "Iteration flag",
    lev = c(0, 1),
    lab = c("Failure", "Success")
  )

  ## mixed-phased flag
  l$mpc_flag <- list(
    name = "Mixed-phase flag",
    lev = c(0, 1),
    lab = c("Ice", "Mixed-phase")
  )

  ## instrument flag
  l$inst_flag <- list(
    name = "Instrument flag",
    lev = c(1, 2, 3),
    lab = c("Lidar", "Radar", "Lidar+Radar")
  )

  ## Precipitation flag
  l$precip_flag <- list(
    name = "Precipitation flag",
    lev = c(0, 1, 2, 3),
    lab = c("none", "cold rain", "warm rain", "both")
  )

  return(l)
}


l2.preview.fileinfo <- function(l.config) {
  seq.days <- seq(as.Date(l.config$orbit.start) - 1, as.Date(l.config$orbit.end) + 1, by = "days")

  data.frame(
    fn = sapply(seq.days, function(day) {
      list.files(paste0(l.config$dir.data, format(day, "/%Y/%Y_%m_%d")), full = T)
    }) %>% unlist(),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      fn.time.start = unlist(plyr::llply(strsplit(basename(fn), "_", fixed = TRUE), function(x) gsub(".nc", "", x[5]))),
      fn.time.start = as.POSIXct(fn.time.start, format = "%Y%m%d%H%M%S", tz = "UTC")
    ) %>%
    filter(difftime(l.config$orbit.start, fn.time.start, units = "hours") < 2 &
      difftime(l.config$orbit.end, fn.time.start, units = "hours") > -2)
}



l2.preview.read <- function(fn, l.config, return.orbit = FALSE) {
  nc <- ncdf4::nc_open(fn)
  nheight <- nc$dim$height$len
  ntime <- nc$dim$time$len
  df <- data.frame(expand.grid(
    idx.height = 1:nheight,
    idx.time = 1:ntime
  )) %>%
    dplyr::mutate(
      idx = 1:nrow(.),
      height = as.numeric(ncdf4::ncvar_get(nc, "height")[idx.height]),
      lat = as.numeric(ncdf4::ncvar_get(nc, "lat")[idx.time]),
      lon = as.numeric(ncdf4::ncvar_get(nc, "lon")[idx.time]),
      time = as.numeric(ncdf4::ncvar_get(nc, "dtime")[idx.time]) + ncdf4::ncvar_get(nc, "base_time") + as.POSIXct("1970-01-01", tz = "UTC"),
      nightday_flag = as.numeric(ncdf4::ncvar_get(nc, "nightday_flag"))[idx.time],
      iteration_flag = as.numeric(ncdf4::ncvar_get(nc, "iteration_flag"))[idx.time],
      precipitation_flag = as.numeric(ncdf4::ncvar_get(nc, "precipitation_flag"))[idx.time]
    )

  if (!return.orbit) {
    df <- df %>%
      dplyr::filter(time >= l.config$orbit.start & time <= l.config$orbit.end) %>%
      dplyr::filter(height > l.config$height$limits[1] & height < (l.config$height$limits[2] * 1E3)) %>%
      dplyr::mutate(
        ta = as.numeric(ncdf4::ncvar_get(nc, "ta"))[idx],
        clm = as.numeric(ncdf4::ncvar_get(nc, "clm"))[idx],
        clm.v2 = as.numeric(ncdf4::ncvar_get(nc, "clm_v2"))[idx],
        icnc_5um = as.numeric(ncdf4::ncvar_get(nc, "icnc_5um"))[idx],
        icnc_100um = as.numeric(ncdf4::ncvar_get(nc, "icnc_100um"))[idx],
        iwc = as.numeric(ncdf4::ncvar_get(nc, "iwc"))[idx],
        reff = as.numeric(ncdf4::ncvar_get(nc, "reffcli"))[idx],
        mixedphase_flag = as.numeric(ncdf4::ncvar_get(nc, "mixedphase_flag"))[idx],
        instrument_flag = as.numeric(ncdf4::ncvar_get(nc, "instrument_flag"))[idx],
        layer_index = as.numeric(ncdf4::ncvar_get(nc, "layer_index"))[idx],
        dz_top = as.numeric(ncdf4::ncvar_get(nc, "dz_top"))[idx]
      ) %>%
      dplyr::arrange(time, height) %>%
      dplyr::mutate(
        height = height * 1E-3, ## heights converted from m to km
        dz_top = dz_top * 1E-3, ## heights converted from m to km
        dz_top = pmin(dz_top, l.config$dz_top$limits[2]),
        clm = factor(clm),
        clm.v2 = factor(clm.v2),
        icnc_5um = icnc_5um * 1E-3, ## icnc converted from #.m-3 to #.L-1
        icnc_5um = pmin(icnc_5um, l.config$icnc.5um$limits[2]),
        icnc_100um = icnc_100um * 1E-3, ## icnc converted from #.m-3 to #.L-1
        icnc_100um = pmin(icnc_100um, l.config$icnc.100um$limits[2]),
        iwc = iwc * 1E6, ## iwc converted from kg.m-3 to mg.m-3
        iwc = pmin(iwc, l.config$iwc$limits[2]),
        reff = reff * 1E6, ## reff converted from m to um
        reff = pmin(reff, l.config$reff$limits[2]),
        mixedphase_flag = factor(mixedphase_flag, levels = l.config$mpc_flag$lev, labels = l.config$mpc_flag$lab),
        instrument_flag = factor(instrument_flag, levels = l.config$inst_flag$lev, labels = l.config$inst_flag$lab),
        layer_index = factor(layer_index)
      )
  }

  ncdf4::nc_close(nc)
  return(df)
}


l2.preview.data <- function(l.config) {
  df.data <- plyr::ldply(l.config$input$fn, l2.preview.read, l.config = l.config)
  ## df.orbit <- plyr::ldply(l.config$input$fn,l2.preview.read,l.config=l.config,return.orbit=TRUE)

  df.data %>%
    select(c(time, lat, lon)) %>%
    dplyr::group_by(time) %>%
    slice(1) %>%
    dplyr::ungroup() %>%
    data.frame() %>%
    arrange(time) %>%
    dplyr::slice(ceiling(seq(1, n(), length.out = 6))) %>%
    dplyr::mutate(label = paste("(", sprintf("%5.2f", round(lat, 2)), ",",
      sprintf("%4.2f", round(lon, 2)), ") \n",
      format(round(time, "secs"), "%H:%M:%S"),
      sep = ""
    )) -> df.xscale

  df.data %>%
    dplyr::group_by(time) %>%
    dplyr::summarize(
      lat = unique(lat),
      lon = unique(lon),
      nightday_flag = unique(nightday_flag),
      iteration_flag = unique(iteration_flag),
      precipitation_flag = unique(precipitation_flag),
      flag.ret = any(icnc_5um > 0, na.rm = TRUE),
      height.zero = height[which.min(abs(ta - 273.15))],
      height.hom = height[which.min(abs(ta - 273.15 + 40))]
    ) %>%
    data.frame() %>%
    dplyr::mutate(
      nightday_flag = factor(nightday_flag),
      iteration_flag = factor(iteration_flag, levels = l.config$iteration_flag$lev, labels = l.config$iteration_flag$lab),
      precipitation_flag = factor(precipitation_flag, levels = l.config$precip_flag$lev, labels = l.config$precip_flag$lab)
    ) -> df.ta

  ## df.orbit %>%
  ##     dplyr::group_by(time) %>%
  ##     dplyr::summarize(lat=unique(lat),
  ##                      lon=unique(lon),
  ##                      nightday_flag=unique(nightday_flag)) %>%
  ##     data.frame() %>%
  ##     dplyr::mutate(nightday_flag=factor(nightday_flag)) -> df.orbit


  l <- list(df.data = df.data, df.xscale = df.xscale, df.ta = df.ta)

  return(l)
}
