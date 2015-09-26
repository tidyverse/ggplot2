#' Continuous colour bar guide.
#'
#' Colour bar guide shows continuous color scales mapped onto values.
#' Colour bar is available with \code{scale_fill} and \code{scale_colour}.
#' For more information, see the inspiration for this function:
#' \href{http://www.mathworks.com/help/techdoc/ref/colorbar.html}{Matlab's colorbar function}.
#'
#' Guides can be specified in each \code{scale_*} or in \code{\link{guides}}.
#' \code{guide="legend"} in \code{scale_*} is syntactic sugar for
#' \code{guide=guide_legend()} (e.g. \code{scale_color_manual(guide = "legend")}).
#' As for how to specify the guide for each scale in more detail,
#' see \code{\link{guides}}.
#'
#' @inheritParams guide_legend
#' @param barwidth A numeric or a \code{\link[grid]{unit}} object specifying
#'   the width of the colorbar. Default value is \code{legend.key.width} or
#'   \code{legend.key.size} in \code{\link{theme}} or theme.
#' @param barheight A numeric or a \code{\link[grid]{unit}} object specifying
#'   the height of the colorbar. Default value is \code{legend.key.height} or
#'   \code{legend.key.size} in \code{\link{theme}} or theme.
#' @param nbin A numeric specifying the number of bins for drawing colorbar. A
#'   smoother colorbar for a larger value.
#' @param raster A logical. If \code{TRUE} then the colorbar is rendered as a
#'   raster object. If \code{FALSE} then the colorbar is rendered as a set of
#'   rectangles. Note that not all graphics devices are capable of rendering
#'   raster image.
#' @param ticks A logical specifying if tick marks on colorbar should be
#'   visible.
#' @param draw.ulim A logical specifying if the upper limit tick marks should
#'   be visible.
#' @param draw.llim A logical specifying if the lower limit tick marks should
#'   be visible.
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param default.unit A character string indicating \code{\link[grid]{unit}}
#'   for \code{barwidth} and \code{barheight}.
#' @param reverse logical. If \code{TRUE} the colorbar is reversed. By default,
#'   the highest value is on the top and the lowest value is on the bottom
#' @param ... ignored.
#' @return A guide object
#' @export
#' @family guides
#' @examples
#' df <- reshape2::melt(outer(1:4, 1:4), varnames = c("X1", "X2"))
#'
#' p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
#' p2 <- p1 + geom_point(aes(size = value))
#'
#' # Basic form
#' p1 + scale_fill_continuous(guide = "colorbar")
#' p1 + scale_fill_continuous(guide = guide_colorbar())
#' p1 + guides(fill = guide_colorbar())
#'
#' # Control styles
#'
#' # bar size
#' p1 + guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10))
#'
#' # no label
#' p1 + guides(fill = guide_colorbar(label = FALSE))
#'
#' # no tick marks
#' p1 + guides(fill = guide_colorbar(ticks = FALSE))
#'
#' # label position
#' p1 + guides(fill = guide_colorbar(label.position = "left"))
#'
#' # label theme
#' p1 + guides(fill = guide_colorbar(label.theme = element_text(colour = "blue", angle = 0)))
#'
#' # small number of bins
#' p1 + guides(fill = guide_colorbar(nbin = 3))
#'
#' # large number of bins
#' p1 + guides(fill = guide_colorbar(nbin = 100))
#'
#' # make top- and bottom-most ticks invisible
#' p1 + scale_fill_continuous(limits = c(0,20), breaks=c(0, 5, 10, 15, 20),
#'  guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE))
#'
#' # guides can be controlled independently
#' p2 +
#'   scale_fill_continuous(guide = "colorbar") +
#'   scale_size(guide = "legend")
#' p2 + guides(fill = "colorbar", size = "legend")
#'
#' p2 +
#'   scale_fill_continuous(guide = guide_colorbar(direction = "horizontal")) +
#'   scale_size(guide = guide_legend(direction = "vertical"))
guide_colourbar <- function(

  # title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # label
  label = TRUE,
  label.position = NULL,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,

  # bar
  barwidth = NULL,
  barheight = NULL,
  nbin = 20,
  raster = TRUE,

  # ticks
  ticks = TRUE,
  draw.ulim= TRUE,
  draw.llim = TRUE,

  # general
  direction = NULL,
  default.unit = "line",
  reverse = FALSE,
  order = 0,

  ...) {

  if (!is.null(barwidth) && !is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
  if (!is.null(barheight) && !is.unit(barheight)) barheight <- unit(barheight, default.unit)

  structure(list(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # bar
    barwidth = barwidth,
    barheight = barheight,
    nbin = nbin,
    raster = raster,

    # ticks
    ticks = ticks,
    draw.ulim = draw.ulim,
    draw.llim = draw.llim,

    # general
    direction = direction,
    default.unit = default.unit,
    reverse = reverse,
    order = order,

    # parameter
    available_aes = c("colour", "color", "fill"), ..., name = "colorbar"),
    class = c("guide", "colorbar")
  )
}

#' @export
guide_train.colorbar <- function(guide, scale) {

  # do nothing if scale are inappropriate
  if (length(intersect(scale$aesthetics, c("color", "colour", "fill"))) == 0) {
    warning("colorbar guide needs colour or fill scales.")
    return(NULL)
  }
  if (scale$is_discrete()) {
    warning("colorbar guide needs continuous scales.")
    return(NULL)
  }


  # create data frame for tick display
  breaks <- scale$get_breaks()
  if (length(breaks) == 0 || all(is.na(breaks)))
    return()

  ticks <- as.data.frame(setNames(list(scale$map(breaks)), scale$aesthetics[1]))
  ticks$.value <- breaks
  ticks$.label <- scale$get_labels(breaks)

  guide$key <- ticks

  # bar specification (number of divs etc)
  .limits <- scale$get_limits()
  .bar <- discard(pretty(.limits, n = guide$nbin), scale$get_limits())
  if (length(.bar) == 0) {
    .bar = unique(.limits)
  }
  guide$bar <- data.frame(colour = scale$map(.bar), value = .bar, stringsAsFactors = FALSE)
  if (guide$reverse) {
    guide$key <- guide$key[nrow(guide$key):1, ]
    guide$bar <- guide$bar[nrow(guide$bar):1, ]
  }
  guide$hash <- with(guide, digest::digest(list(title, key$.label, bar, name)))
  guide
}

# simply discards the new guide
#' @export
guide_merge.colorbar <- function(guide, new_guide) {
  guide
}

# this guide is not geom-based.
#' @export
guide_geom.colorbar <- function(guide, ...) {
  guide
}

#' @export
guide_gengrob.colorbar <- function(guide, theme) {

  # settings of location and size
  switch(guide$direction,
    "horizontal" = {
      label.position <- guide$label.position %||% "bottom"
      if (!label.position %in% c("top", "bottom")) stop("label position \"", label.position, "\" is invalid")

      barwidth <- convertWidth(guide$barwidth %||% (theme$legend.key.width * 5), "mm")
      barheight <- convertHeight(guide$barheight %||% theme$legend.key.height, "mm")
    },
    "vertical" = {
      label.position <- guide$label.position %||% "right"
      if (!label.position %in% c("left", "right")) stop("label position \"", label.position, "\" is invalid")

      barwidth <- convertWidth(guide$barwidth %||% theme$legend.key.width, "mm")
      barheight <- convertHeight(guide$barheight %||% (theme$legend.key.height * 5), "mm")
    })

  barwidth.c <- c(barwidth)
  barheight.c <- c(barheight)
  barlength.c <- switch(guide$direction, "horizontal" = barwidth.c, "vertical" = barheight.c)
  nbreak <- nrow(guide$key)

  # gap between keys etc
  hgap <- c(convertWidth(unit(0.3, "lines"), "mm"))
  vgap <- hgap

  grob.bar <-
    if (guide$raster) {
      image <- switch(guide$direction, horizontal = t(guide$bar$colour), vertical = rev(guide$bar$colour))
      rasterGrob(image = image, width = barwidth.c, height = barheight.c, default.units = "mm", gp = gpar(col = NA), interpolate = TRUE)
    } else {
      switch(guide$direction,
             horizontal = {
               bw <- barwidth.c / nrow(guide$bar)
               bx <- (seq(nrow(guide$bar)) - 1) * bw
               rectGrob(x = bx, y = 0, vjust = 0, hjust = 0, width = bw, height = barheight.c, default.units = "mm",
                        gp = gpar(col = NA, fill = guide$bar$colour))
             },
             vertical = {
               bh <- barheight.c / nrow(guide$bar)
               by <- (seq(nrow(guide$bar)) - 1) * bh
               rectGrob(x = 0, y = by, vjust = 0, hjust = 0, width = barwidth.c, height = bh, default.units = "mm",
                        gp = gpar(col = NA, fill = guide$bar$colour))
             })
  }

  # tick and label position
  tic_pos.c <- rescale(guide$key$.value, c(0.5, guide$nbin - 0.5), guide$bar$value[c(1, nrow(guide$bar))]) * barlength.c / guide$nbin
  label_pos <- unit(tic_pos.c, "mm")
  if (!guide$draw.ulim) tic_pos.c <- tic_pos.c[-1]
  if (!guide$draw.llim) tic_pos.c <- tic_pos.c[-length(tic_pos.c)]

  # title
  grob.title <- ggname("guide.title",
    element_grob(
      guide$title.theme %||% calc_element("legend.title", theme),
      label = guide$title,
      hjust = guide$title.hjust %||% theme$legend.title.align %||% 0,
      vjust = guide$title.vjust %||% 0.5
    )
  )


  title_width <- convertWidth(grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- convertHeight(grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)

  # label
  label.theme <- guide$label.theme %||% calc_element("legend.text", theme)
  grob.label <- {
    if (!guide$label)
      zeroGrob()
    else {
      hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||%
        if (any(is.expression(guide$key$.label))) 1 else switch(guide$direction, horizontal = 0.5, vertical = 0)
      vjust <- y <- guide$label.vjust %||% 0.5
      switch(guide$direction, horizontal = {x <- label_pos; y <- vjust}, "vertical" = {x <- hjust; y <- label_pos})

      label <- guide$key$.label

      # If any of the labels are quoted language objects, convert them
      # to expressions. Labels from formatter functions can return these
      if (any(vapply(label, is.call, logical(1)))) {
        label <- lapply(label, function(l) {
          if (is.call(l)) substitute(expression(x), list(x = l))
          else l
        })
        label <- do.call(c, label)
      }
      g <- element_grob(element = label.theme, label = label,
        x = x, y = y, hjust = hjust, vjust = vjust)
      ggname("guide.label", g)
    }
  }

  label_width <- convertWidth(grobWidth(grob.label), "mm")
  label_width.c <- c(label_width)
  label_height <- convertHeight(grobHeight(grob.label), "mm")
  label_height.c <- c(label_height)

  # ticks
  grob.ticks <-
    if (!guide$ticks) zeroGrob()
    else {
      switch(guide$direction,
        "horizontal" = {
          x0 = rep(tic_pos.c, 2)
          y0 = c(rep(0, nbreak), rep(barheight.c * (4/5), nbreak))
          x1 = rep(tic_pos.c, 2)
          y1 = c(rep(barheight.c * (1/5), nbreak), rep(barheight.c, nbreak))
        },
        "vertical" = {
          x0 = c(rep(0, nbreak), rep(barwidth.c * (4/5), nbreak))
          y0 = rep(tic_pos.c, 2)
          x1 = c(rep(barwidth.c * (1/5), nbreak), rep(barwidth.c, nbreak))
          y1 = rep(tic_pos.c, 2)
        })
      segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                   default.units = "mm", gp = gpar(col = "white", lwd = 0.5, lineend = "butt"))
    }

  # layout of bar and label
  switch(guide$direction,
    "horizontal" = {
      switch(label.position,
        "top" = {
          bl_widths <- barwidth.c
          bl_heights <- c(label_height.c, vgap, barheight.c)
          vps <- list(bar.row = 3, bar.col = 1,
                      label.row = 1, label.col = 1)
        },
        "bottom" = {
          bl_widths <- barwidth.c
          bl_heights <- c(barheight.c, vgap, label_height.c)
          vps <- list(bar.row = 1, bar.col = 1,
                      label.row = 3, label.col = 1)
        })
    },
    "vertical" = {
      switch(label.position,
        "left" = {
          bl_widths <- c(label_width.c, vgap, barwidth.c)
          bl_heights <- barheight.c
          vps <- list(bar.row = 1, bar.col = 3,
                      label.row = 1, label.col = 1)
        },
        "right" = {
          bl_widths <- c(barwidth.c, vgap, label_width.c)
          bl_heights <- barheight.c
          vps <- list(bar.row = 1, bar.col = 1,
                      label.row = 1, label.col = 3)
        })
    })

  # layout of title and bar+label
  switch(guide$title.position,
    "top" = {
      widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
      heights <- c(title_height.c, vgap, bl_heights)
      vps <- with(vps,
                  list(bar.row = bar.row + 2, bar.col = bar.col,
                       label.row = label.row + 2, label.col = label.col,
                       title.row = 1, title.col = 1:length(widths)))
    },
    "bottom" = {
      widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
      heights <- c(bl_heights, vgap, title_height.c)
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col,
                       label.row = label.row, label.col = label.col,
                       title.row = length(heights), title.col = 1:length(widths)))
    },
    "left" = {
      widths <- c(title_width.c, hgap, bl_widths)
      heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col + 2,
                       label.row = label.row, label.col = label.col + 2,
                       title.row = 1:length(heights), title.col = 1))
    },
    "right" = {
      widths <- c(bl_widths, hgap, title_width.c)
      heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col,
                       label.row = label.row, label.col = label.col,
                       title.row = 1:length(heights), title.col = length(widths)))
    })

  # background
  grob.background <- element_render(theme, "legend.background")

  # padding
  padding <- unit(1.5, "mm")
  widths <- c(padding, widths, padding)
  heights <- c(padding, heights, padding)

  gt <- gtable(widths = unit(widths, "mm"), heights = unit(heights, "mm"))
  gt <- gtable_add_grob(gt, grob.background, name = "background", clip = "off",
    t = 1, r = -1, b = -1, l = 1)
  gt <- gtable_add_grob(gt, grob.bar, name = "bar", clip = "off",
    t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
    b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))
  gt <- gtable_add_grob(gt, grob.label, name = "label", clip = "off",
    t = 1 + min(vps$label.row), r = 1 + max(vps$label.col),
    b = 1 + max(vps$label.row), l = 1 + min(vps$label.col))
  gt <- gtable_add_grob(gt, grob.title, name = "title", clip = "off",
    t = 1 + min(vps$title.row), r = 1 + max(vps$title.col),
    b = 1 + max(vps$title.row), l = 1 + min(vps$title.col))
  gt <- gtable_add_grob(gt, grob.ticks, name = "ticks", clip = "off",
    t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
    b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))

  gt
}

#' @export
#' @rdname guide_colourbar
guide_colorbar <- guide_colourbar
