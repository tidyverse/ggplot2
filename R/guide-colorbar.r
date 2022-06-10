#' Continuous colour bar guide
#'
#' Colour bar guide shows continuous colour scales mapped onto values.
#' Colour bar is available with `scale_fill` and `scale_colour`.
#' For more information, see the inspiration for this function:
#' \href{http://www.mathworks.com/help/techdoc/ref/colorbar.html}{Matlab's colorbar function}.
#'
#' Guides can be specified in each `scale_*` or in [guides()].
#' `guide="legend"` in `scale_*` is syntactic sugar for
#' `guide=guide_legend()` (e.g. `scale_colour_manual(guide = "legend")`).
#' As for how to specify the guide for each scale in more detail,
#' see [guides()].
#'
#' @inheritParams guide_legend
#' @param barwidth A numeric or a [grid::unit()] object specifying
#'   the width of the colourbar. Default value is `legend.key.width` or
#'   `legend.key.size` in [theme()] or theme.
#' @param barheight A numeric or a [grid::unit()] object specifying
#'   the height of the colourbar. Default value is `legend.key.height` or
#'   `legend.key.size` in [theme()] or theme.
#' @param frame.colour A string specifying the colour of the frame
#'   drawn around the bar. If `NULL` (the default), no frame is drawn.
#' @param frame.linewidth A numeric specifying the width of the frame
#'   drawn around the bar in millimetres.
#' @param frame.linetype A numeric specifying the linetype of the frame
#'   drawn around the bar.
#' @param nbin A numeric specifying the number of bins for drawing the
#'   colourbar. A smoother colourbar results from a larger value.
#' @param raster A logical. If `TRUE` then the colourbar is rendered as a
#'   raster object. If `FALSE` then the colourbar is rendered as a set of
#'   rectangles. Note that not all graphics devices are capable of rendering
#'   raster image.
#' @param ticks A logical specifying if tick marks on the colourbar should be
#'   visible.
#' @param ticks.colour A string specifying the colour of the tick marks.
#' @param ticks.linewidth A numeric specifying the width of the tick marks in
#'   millimetres.
#' @param draw.ulim A logical specifying if the upper limit tick marks should
#'   be visible.
#' @param draw.llim A logical specifying if the lower limit tick marks should
#'   be visible.
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param default.unit A character string indicating [grid::unit()]
#'   for `barwidth` and `barheight`.
#' @param reverse logical. If `TRUE` the colourbar is reversed. By default,
#'   the highest value is on the top and the lowest value is on the bottom
#' @param available_aes A vector of character strings listing the aesthetics
#'   for which a colourbar can be drawn.
#' @param ... ignored.
#' @return A guide object
#' @export
#' @family guides
#' @examples
#' df <- expand.grid(X1 = 1:10, X2 = 1:10)
#' df$value <- df$X1 * df$X2
#'
#' p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
#' p2 <- p1 + geom_point(aes(size = value))
#'
#' # Basic form
#' p1 + scale_fill_continuous(guide = "colourbar")
#' p1 + scale_fill_continuous(guide = guide_colourbar())
#' p1 + guides(fill = guide_colourbar())
#'
#' # Control styles
#'
#' # bar size
#' p1 + guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
#'
#' # no label
#' p1 + guides(fill = guide_colourbar(label = FALSE))
#'
#' # no tick marks
#' p1 + guides(fill = guide_colourbar(ticks = FALSE))
#'
#' # label position
#' p1 + guides(fill = guide_colourbar(label.position = "left"))
#'
#' # label theme
#' p1 + guides(fill = guide_colourbar(label.theme = element_text(colour = "blue", angle = 0)))
#'
#' # small number of bins
#' p1 + guides(fill = guide_colourbar(nbin = 3))
#'
#' # large number of bins
#' p1 + guides(fill = guide_colourbar(nbin = 100))
#'
#' # make top- and bottom-most ticks invisible
#' p1 +
#'   scale_fill_continuous(
#'     limits = c(0,20), breaks = c(0, 5, 10, 15, 20),
#'     guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)
#'    )
#'
#' # guides can be controlled independently
#' p2 +
#'   scale_fill_continuous(guide = "colourbar") +
#'   scale_size(guide = "legend")
#' p2 + guides(fill = "colourbar", size = "legend")
#'
#' p2 +
#'   scale_fill_continuous(guide = guide_colourbar(direction = "horizontal")) +
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
  nbin = 300,
  raster = TRUE,

  # frame
  frame.colour = NULL,
  frame.linewidth = 0.5 / .pt,
  frame.linetype = 1,

  # ticks
  ticks = TRUE,
  ticks.colour = "white",
  ticks.linewidth = 0.5 / .pt,
  draw.ulim= TRUE,
  draw.llim = TRUE,

  # general
  direction = NULL,
  default.unit = "line",
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),

  ...) {

  if (!is.null(barwidth) && !is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
  if (!is.null(barheight) && !is.unit(barheight)) barheight <- unit(barheight, default.unit)

  structure(list2(
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
    available_aes = available_aes,
    ...,
    name = "colorbar"),
    class = c("guide", "colorbar")
  )
}

#' @export
guide_train.colorbar <- function(guide, scale, aesthetic = NULL) {

  # do nothing if scale are inappropriate
  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    cli::cli_warn("colourbar guide needs appropriate scales: {.or {.field {guide$available_aes}}}")
    return(NULL)
  }
  if (scale$is_discrete()) {
    cli::cli_warn("colourbar guide needs continuous scales.")
    return(NULL)
  }


  # create data frame for tick display
  breaks <- scale$get_breaks()
  if (length(breaks) == 0 || all(is.na(breaks)))
    return()

  ticks <- new_data_frame(setNames(list(scale$map(breaks)), aesthetic %||% scale$aesthetics[1]))
  ticks$.value <- breaks
  ticks$.label <- scale$get_labels(breaks)

  guide$key <- ticks

  # bar specification (number of divs etc)
  .limits <- scale$get_limits()
  .bar <- seq(.limits[1], .limits[2], length.out = guide$nbin)
  if (length(.bar) == 0) {
    .bar = unique(.limits)
  }
  guide$bar <- new_data_frame(list(colour = scale$map(.bar), value = .bar), n = length(.bar))
  if (guide$reverse) {
    guide$key <- guide$key[nrow(guide$key):1, ]
    guide$bar <- guide$bar[nrow(guide$bar):1, ]
  }
  guide$hash <- with(guide, hash(list(title, key$.label, bar, name)))
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
  guide_layers <- lapply(layers, function(layer) {
    matched <- matched_aes(layer, guide)

    if (length(matched) == 0) {
      # This layer does not use this guide
      return(NULL)
    }

    # check if this layer should be included
    if (include_layer_in_guide(layer, matched)) {
      layer
    } else {
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
  if (guide$direction == "horizontal") {
    label.position <- guide$label.position %||% "bottom"
    if (!label.position %in% c("top", "bottom")) {
      cli::cli_abort(c(
        "label position {.val {label.position}} is invalid",
        "i" = "use either {.val 'top'} or {.val 'bottom'}"
      ))
    }

    barwidth <- width_cm(guide$barwidth %||% (theme$legend.key.width * 5))
    barheight <- height_cm(guide$barheight %||% theme$legend.key.height)
  } else { # guide$direction == "vertical"
    label.position <- guide$label.position %||% "right"
    if (!label.position %in% c("left", "right")) {
      cli::cli_abort(c(
        "label position {.val {label.position}} is invalid",
        "i" = "use either {.val 'left'} or {.val 'right'}"
      ))
    }

    barwidth <- width_cm(guide$barwidth %||% theme$legend.key.width)
    barheight <- height_cm(guide$barheight %||% (theme$legend.key.height * 5))
  }

  barlength <- switch(guide$direction, "horizontal" = barwidth, "vertical" = barheight)
  nbreak <- nrow(guide$key)

  # make the bar grob (`grob.bar`)
  if (guide$raster) {
    image <- switch(guide$direction, horizontal = t(guide$bar$colour), vertical = rev(guide$bar$colour))
    grob.bar <-rasterGrob(image = image, width = barwidth, height = barheight, default.units = "cm", gp = gpar(col = NA), interpolate = TRUE)
  } else {
    if (guide$direction == "horizontal") {
      bw <- barwidth / nrow(guide$bar)
      bx <- (seq(nrow(guide$bar)) - 1) * bw
      grob.bar <-rectGrob(x = bx, y = 0, vjust = 0, hjust = 0, width = bw, height = barheight, default.units = "cm",
                          gp = gpar(col = NA, fill = guide$bar$colour))
    } else { # guide$direction == "vertical"
      bh <- barheight / nrow(guide$bar)
      by <- (seq(nrow(guide$bar)) - 1) * bh
      grob.bar <-rectGrob(x = 0, y = by, vjust = 0, hjust = 0, width = barwidth, height = bh, default.units = "cm",
                          gp = gpar(col = NA, fill = guide$bar$colour))
    }
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
                      lwd = guide$frame.linewidth * .pt,
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

  title.hjust <- guide$title.hjust %||% theme$legend.title.align %||% title.theme$hjust %||% 0
  title.vjust <- guide$title.vjust %||% title.theme$vjust %||% 0.5

  grob.title <- ggname("guide.title",
    element_grob(
      title.theme,
      label = guide$title,
      hjust = title.hjust,
      vjust = title.vjust,
      margin_x = TRUE,
      margin_y = TRUE
    )
  )

  title_width <- width_cm(grob.title)
  title_height <- height_cm(grob.title)
  title_fontsize <- title.theme$size %||% calc_element("legend.title", theme)$size %||%
    calc_element("text", theme)$size %||% 11

  # gap between keys etc
  # the default horizontal and vertical gap need to be the same to avoid strange
  # effects for certain guide layouts
  hgap <- width_cm(theme$legend.spacing.x  %||% (0.5 * unit(title_fontsize, "pt")))
  vgap <- height_cm(theme$legend.spacing.y %||% (0.5 * unit(title_fontsize, "pt")))

  # Labels

  # get the defaults for label justification. The defaults are complicated and depend
  # on the direction of the legend and on label placement
  just_defaults <- label_just_defaults.colorbar(guide$direction, label.position)
  # don't set expressions left-justified
  if (just_defaults$hjust == 0 && any(is.expression(guide$key$.label))) just_defaults$hjust <- 1

  # get the label theme
  label.theme <- guide$label.theme %||% calc_element("legend.text", theme)

  # We break inheritance for hjust and vjust, because that's more intuitive here; it still allows manual
  # setting of hjust and vjust if desired. The alternative is to ignore hjust and vjust altogether, which
  # seems worse
  if (is.null(guide$label.theme$hjust) && is.null(theme$legend.text$hjust)) label.theme$hjust <- NULL
  if (is.null(guide$label.theme$vjust) && is.null(theme$legend.text$vjust)) label.theme$vjust <- NULL

  # label.theme in param of guide_legend() > theme$legend.text.align > default
  hjust <- guide$label.hjust %||% theme$legend.text.align %||% label.theme$hjust %||%
    just_defaults$hjust
  vjust <- guide$label.vjust %||% label.theme$vjust %||%
    just_defaults$vjust

  # make the label grob (`grob.label`)
  if (!guide$label)
    grob.label <- zeroGrob()
  else {
    if (guide$direction == "horizontal") {
      x <- label_pos
      y <- rep(vjust, length(label_pos))
      margin_x <- FALSE
      margin_y <- TRUE
    } else { # guide$direction == "vertical"
      x <- rep(hjust, length(label_pos))
      y <- label_pos
      margin_x <- TRUE
      margin_y <- FALSE
    }
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
    grob.label <- element_grob(
      element = label.theme,
      label = label,
      x = x,
      y = y,
      hjust = hjust,
      vjust = vjust,
      margin_x = margin_x,
      margin_y = margin_y
    )
    grob.label <- ggname("guide.label", grob.label)
  }

  label_width <- width_cm(grob.label)
  label_height <- height_cm(grob.label)

  # make the ticks grob (`grob.ticks`)
  if (!guide$ticks)
    grob.ticks <-zeroGrob()
  else {
    if (guide$direction == "horizontal") {
      x0 <- rep(tick_pos, 2)
      y0 <- c(rep(0, nbreak), rep(barheight * (4/5), nbreak))
      x1 <- rep(tick_pos, 2)
      y1 <- c(rep(barheight * (1/5), nbreak), rep(barheight, nbreak))
    } else { # guide$direction == "vertical"
      x0 <- c(rep(0, nbreak), rep(barwidth * (4/5), nbreak))
      y0 <- rep(tick_pos, 2)
      x1 <- c(rep(barwidth * (1/5), nbreak), rep(barwidth, nbreak))
      y1 <- rep(tick_pos, 2)
    }
    grob.ticks <- segmentsGrob(
      x0 = x0, y0 = y0, x1 = x1, y1 = y1,
      default.units = "cm",
      gp = gpar(
        col = guide$ticks.colour,
        lwd = guide$ticks.linewidth * .pt,
        lineend = "butt"
      )
    )
  }

  # layout of bar and label
  if (guide$direction == "horizontal") {
    if (label.position == "top") {
      bl_widths <- barwidth
      bl_heights <- c(label_height, vgap, barheight)
      vps <- list(bar.row = 3, bar.col = 1,
                  label.row = 1, label.col = 1)
    } else { # label.position == "bottom" or other
      bl_widths <- barwidth
      bl_heights <- c(barheight, vgap, label_height)
      vps <- list(bar.row = 1, bar.col = 1,
                  label.row = 3, label.col = 1)
    }
  } else { # guide$direction == "vertical"
    if (label.position == "left") {
      bl_widths <- c(label_width, hgap, barwidth)
      bl_heights <- barheight
      vps <- list(bar.row = 1, bar.col = 3,
                  label.row = 1, label.col = 1)
    } else { # label.position == "right" or other
      bl_widths <- c(barwidth, hgap, label_width)
      bl_heights <- barheight
      vps <- list(bar.row = 1, bar.col = 1,
                  label.row = 1, label.col = 3)
    }
  }

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
  padding <- convertUnit(theme$legend.margin %||% margin(), "cm", valueOnly = TRUE)
  widths <- c(padding[4], widths, padding[2])
  heights <- c(padding[1], heights, padding[3])

  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(gt, grob.background, name = "background", clip = "off",
    t = 1, r = -1, b = -1, l = 1)
  gt <- gtable_add_grob(gt, grob.bar, name = "bar", clip = "off",
    t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
    b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))
  gt <- gtable_add_grob(
    gt,
    grob.label,
    name = "label",
    clip = "off",
    t = 1 + min(vps$label.row), r = 1 + max(vps$label.col),
    b = 1 + max(vps$label.row), l = 1 + min(vps$label.col)
  )
  gt <- gtable_add_grob(
    gt,
    justify_grobs(
      grob.title,
      hjust = title.hjust,
      vjust = title.vjust,
      int_angle = title.theme$angle,
      debug = title.theme$debug
    ),
    name = "title",
    clip = "off",
    t = 1 + min(vps$title.row), r = 1 + max(vps$title.col),
    b = 1 + max(vps$title.row), l = 1 + min(vps$title.col)
  )

  gt <- gtable_add_grob(gt, grob.ticks, name = "ticks", clip = "off",
    t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
    b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col)
  )

  gt
}

#' @export
#' @rdname guide_colourbar
guide_colorbar <- guide_colourbar
