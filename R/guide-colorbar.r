#' Continuous colour bar guide
#'
#' Colour bar guide shows continuous color scales mapped onto values.
#' Colour bar is available with `scale_fill` and `scale_colour`.
#' For more information, see the inspiration for this function:
#' \href{http://www.mathworks.com/help/techdoc/ref/colorbar.html}{Matlab's colorbar function}.
#'
#' Guides can be specified in each `scale_*` or in [guides()].
#' `guide="legend"` in `scale_*` is syntactic sugar for
#' `guide=guide_legend()` (e.g. `scale_color_manual(guide = "legend")`).
#' As for how to specify the guide for each scale in more detail,
#' see [guides()].
#'
#' @inheritParams guide_legend
#' @param barwidth A numeric or a [grid::unit()] object specifying
#'   the width of the colorbar. Default value is `legend.key.width` or
#'   `legend.key.size` in [theme()] or theme.
#' @param barheight A numeric or a [grid::unit()] object specifying
#'   the height of the colorbar. Default value is `legend.key.height` or
#'   `legend.key.size` in [theme()] or theme.
#' @param frame.colour A string specifying the colour of the frame
#'   drawn around the bar. If `NULL` (the default), no frame is drawn.
#' @param frame.linewidth A numeric specifying the width of the frame
#'   drawn around the bar.
#' @param frame.linetype A numeric specifying the linetype of the frame
#'   drawn around the bar.
#' @param nbin A numeric specifying the number of bins for drawing colorbar. A
#'   smoother colorbar for a larger value.
#' @param raster A logical. If `TRUE` then the colorbar is rendered as a
#'   raster object. If `FALSE` then the colorbar is rendered as a set of
#'   rectangles. Note that not all graphics devices are capable of rendering
#'   raster image.
#' @param ticks A logical specifying if tick marks on colorbar should be
#'   visible.
#' @param ticks.colour A string specifying the color of the tick marks.
#' @param ticks.linewidth A numeric specifying the width of the tick marks.
#' @param draw.ulim A logical specifying if the upper limit tick marks should
#'   be visible.
#' @param draw.llim A logical specifying if the lower limit tick marks should
#'   be visible.
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param default.unit A character string indicating [grid::unit()]
#'   for `barwidth` and `barheight`.
#' @param reverse logical. If `TRUE` the colorbar is reversed. By default,
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
#' p1 + scale_fill_continuous(limits = c(0,20), breaks = c(0, 5, 10, 15, 20),
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

  # frame
  frame.colour = NULL,
  frame.linewidth = 0.5,
  frame.linetype = 1,

  # ticks
  ticks = TRUE,
  ticks.colour = "white",
  ticks.linewidth = 0.5,
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

    # frame
    frame.colour = frame.colour,
    frame.linewidth = frame.linewidth,
    frame.linetype = frame.linetype,

    # ticks
    ticks = ticks,
    ticks.colour = ticks.colour,
    ticks.linewidth = ticks.linewidth,
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
  .bar <- seq(.limits[1], .limits[2], length = guide$nbin)
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
guide_geom.colorbar <- function(guide, layers, default_mapping) {
  # Layers that use this guide
  guide_layers <- plyr::llply(layers, function(layer) {
    matched <- matched_aes(layer, guide, default_mapping)

    if (length(matched) && ((is.na(layer$show.legend) || layer$show.legend))) {
      layer
    } else {
      # This layer does not use this guide
      NULL
    }
  })

  # Remove this guide if no layer uses it
  if (length(compact(guide_layers)) == 0) guide <- NULL

  guide
}

#' @export
guide_gengrob.colorbar <- function(guide, theme) {

  # settings of location and size
  switch(guide$direction,
    "horizontal" = {
      label.position <- guide$label.position %||% "bottom"
      if (!label.position %in% c("top", "bottom")) stop("label position \"", label.position, "\" is invalid")

      barwidth <- width_cm(guide$barwidth %||% (theme$legend.key.width * 5))
      barheight <- height_cm(guide$barheight %||% theme$legend.key.height)
    },
    "vertical" = {
      label.position <- guide$label.position %||% "right"
      if (!label.position %in% c("left", "right")) stop("label position \"", label.position, "\" is invalid")

      barwidth <- width_cm(guide$barwidth %||% theme$legend.key.width)
      barheight <- height_cm(guide$barheight %||% (theme$legend.key.height * 5))
    })

  barlength <- switch(guide$direction, "horizontal" = barwidth, "vertical" = barheight)
  nbreak <- nrow(guide$key)

  grob.bar <-
    if (guide$raster) {
      image <- switch(guide$direction, horizontal = t(guide$bar$colour), vertical = rev(guide$bar$colour))
      rasterGrob(image = image, width = barwidth, height = barheight, default.units = "cm", gp = gpar(col = NA), interpolate = TRUE)
    } else {
      switch(guide$direction,
             horizontal = {
               bw <- barwidth / nrow(guide$bar)
               bx <- (seq(nrow(guide$bar)) - 1) * bw
               rectGrob(x = bx, y = 0, vjust = 0, hjust = 0, width = bw, height = barheight, default.units = "cm",
                        gp = gpar(col = NA, fill = guide$bar$colour))
             },
             vertical = {
               bh <- barheight / nrow(guide$bar)
               by <- (seq(nrow(guide$bar)) - 1) * bh
               rectGrob(x = 0, y = by, vjust = 0, hjust = 0, width = barwidth, height = bh, default.units = "cm",
                        gp = gpar(col = NA, fill = guide$bar$colour))
             })
    }

  # make frame around color bar if requested (colour is not NULL)
  if (!is.null(guide$frame.colour)) {
    grob.bar <- grobTree(
                  grob.bar,
                  rectGrob(
                    width = barwidth,
                    height = barheight,
                    default.units = "cm",
                    gp = gpar(
                      col = guide$frame.colour,
                      lwd = guide$frame.linewidth,
                      lty = guide$frame.linetype,
                      fill = NA)
                    )
                  )
  }

  # tick and label position
  tick_pos <- rescale(guide$key$.value, c(0.5, guide$nbin - 0.5), guide$bar$value[c(1, nrow(guide$bar))]) * barlength / guide$nbin
  label_pos <- unit(tick_pos, "cm")
  if (!guide$draw.ulim) tick_pos <- tick_pos[-1]
  if (!guide$draw.llim) tick_pos <- tick_pos[-length(tick_pos)]

  # title

  # obtain the theme for the legend title. We need this both for the title grob
  # and to obtain the title fontsize.
  title.theme <- guide$title.theme %||% calc_element("legend.title", theme)

  grob.title <- ggname("guide.title",
    element_grob(
      title.theme,
      label = guide$title,
      hjust = guide$title.hjust %||% theme$legend.title.align %||% 0,
      vjust = guide$title.vjust %||% 0.5
    )
  )

  title_width <- width_cm(grob.title)
  title_height <- height_cm(grob.title)
  title_fontsize <- title.theme$size
  if (is.null(title_fontsize)) title_fontsize <- 0

  # gap between keys etc
  # the default horizontal and vertical gap need to be the same to avoid strange
  # effects for certain guide layouts
  hgap <- width_cm(theme$legend.spacing.x  %||% (0.5 * unit(title_fontsize, "pt")))
  vgap <- height_cm(theme$legend.spacing.y %||% (0.5 * unit(title_fontsize, "pt")))

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

  label_width <- width_cm(grob.label)
  label_height <- height_cm(grob.label)

  # ticks
  grob.ticks <-
    if (!guide$ticks) zeroGrob()
    else {
      switch(guide$direction,
        "horizontal" = {
          x0 = rep(tick_pos, 2)
          y0 = c(rep(0, nbreak), rep(barheight * (4/5), nbreak))
          x1 = rep(tick_pos, 2)
          y1 = c(rep(barheight * (1/5), nbreak), rep(barheight, nbreak))
        },
        "vertical" = {
          x0 = c(rep(0, nbreak), rep(barwidth * (4/5), nbreak))
          y0 = rep(tick_pos, 2)
          x1 = c(rep(barwidth * (1/5), nbreak), rep(barwidth, nbreak))
          y1 = rep(tick_pos, 2)
        })
      segmentsGrob(
        x0 = x0, y0 = y0, x1 = x1, y1 = y1,
        default.units = "cm",
        gp = gpar(
          col = guide$ticks.colour,
          lwd = guide$ticks.linewidth,
          lineend = "butt")
        )
    }

  # layout of bar and label
  switch(guide$direction,
    "horizontal" = {
      switch(label.position,
        "top" = {
          bl_widths <- barwidth
          bl_heights <- c(label_height, vgap, barheight)
          vps <- list(bar.row = 3, bar.col = 1,
                      label.row = 1, label.col = 1)
        },
        "bottom" = {
          bl_widths <- barwidth
          bl_heights <- c(barheight, vgap, label_height)
          vps <- list(bar.row = 1, bar.col = 1,
                      label.row = 3, label.col = 1)
        })
    },
    "vertical" = {
      switch(label.position,
        "left" = {
          bl_widths <- c(label_width, hgap, barwidth)
          bl_heights <- barheight
          vps <- list(bar.row = 1, bar.col = 3,
                      label.row = 1, label.col = 1)
        },
        "right" = {
          bl_widths <- c(barwidth, hgap, label_width)
          bl_heights <- barheight
          vps <- list(bar.row = 1, bar.col = 1,
                      label.row = 1, label.col = 3)
        })
    })

  # layout of title and bar+label
  switch(guide$title.position,
    "top" = {
      widths <- c(bl_widths, max(0, title_width - sum(bl_widths)))
      heights <- c(title_height, vgap, bl_heights)
      vps <- with(vps,
                  list(bar.row = bar.row + 2, bar.col = bar.col,
                       label.row = label.row + 2, label.col = label.col,
                       title.row = 1, title.col = 1:length(widths)))
    },
    "bottom" = {
      widths <- c(bl_widths, max(0, title_width - sum(bl_widths)))
      heights <- c(bl_heights, vgap, title_height)
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col,
                       label.row = label.row, label.col = label.col,
                       title.row = length(heights), title.col = 1:length(widths)))
    },
    "left" = {
      widths <- c(title_width, hgap, bl_widths)
      heights <- c(bl_heights, max(0, title_height - sum(bl_heights)))
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col + 2,
                       label.row = label.row, label.col = label.col + 2,
                       title.row = 1:length(heights), title.col = 1))
    },
    "right" = {
      widths <- c(bl_widths, hgap, title_width)
      heights <- c(bl_heights, max(0, title_height - sum(bl_heights)))
      vps <- with(vps,
                  list(bar.row = bar.row, bar.col = bar.col,
                       label.row = label.row, label.col = label.col,
                       title.row = 1:length(heights), title.col = length(widths)))
    })

  # background
  grob.background <- element_render(theme, "legend.background")

  # padding
  padding <- convertUnit(theme$legend.margin %||% margin(), "cm")
  widths <- c(padding[4], widths, padding[2])
  heights <- c(padding[1], heights, padding[3])

  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
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
