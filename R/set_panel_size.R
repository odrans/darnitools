#' Useful utility for setting the panel size, margin, width, and height of a ggplot object.
#' It saves the plot as an image if a file path is specified.
#'
#' @param p A ggplot object.
#' @param g A ggplot grob. Default: ggplotGrob(p).
#' @param file File path to save the plot as an image. Default: NULL (do not save).
#' @param margin Margin around the plot. Default: unit(1, "mm").
#' @param width Width of the panel. Default: unit(4, "cm").
#' @param height Height of the panel. Default: unit(4, "cm").
#' @return A ggplot object with specified panel size, margin, width, and height.
set_panel_size <- function(p = NULL, g = ggplot::ggplotGrob(p), file = NULL,
                           margin = grid::unit(1, "mm"), width = grid::unit(4, "cm"),
                           height = grid::unit(4, "cm")) {

  ## Error handling for p
  if (!is.null(p) && !inherits(p, "ggplot")) {
    stop("The 'p' parameter must be a ggplot object.")
  }

  panels <- grep("panel", g$layout$name)
  panel_index_w <- unique(g$layout$l[panels])
  panel_index_h <- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)

  g$widths[panel_index_w] <-  rep(width, nw)
  g$heights[panel_index_h] <- height

  ## Save plot if file is specified
  if(!is.null(file))
    ggplot2::ggsave(file, g,
                    width = grid::convertWidth(sum(g$widths) + margin,
                                               unitTo = "in", valueOnly = TRUE),
                    height = grid::convertHeight(sum(g$heights) + margin,
                                                 unitTo = "in", valueOnly = TRUE))

  ## Return ggplot object instead of grob object
  grid::grid.draw(g)
}
