guide_bins <- function(
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

  # frame
  frame.colour = NULL,
  frame.linewidth = 0.5,
  frame.linetype = 1,

  # divider
  divider.colour = NULL,
  divider.linewidth = 0.5,
  divider.linetype = 1,

  # general
  direction = NULL,
  default.unit = "line",
  override.aes = list(),
  reverse = FALSE,
  order = 0,
  ...) {

  if (!is.null(barwidth) && !is.unit(barwidth)) {
    barwidth <- unit(barwidth, default.unit)
  }
  if (!is.null(barheight) && !is.unit(barheight)) {
    barheight <- unit(barheight, default.unit)
  }

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

    # frame
    frame.colour = frame.colour,
    frame.linewidth = frame.linewidth,
    frame.linetype = frame.linetype,

    # divider
    divider.colour = divider.colour,
    divider.linewidth = divider.linewidth,
    divider.linetype = divider.linetype,

    # general
    direction = direction,
    default.unit = default.unit,
    reverse = reverse,
    order = order,

    # parameter
    available_aes = c("any"),
    ...,
    name = "bins"),
    class = c("guide", "bins")
  )
}

#' @export
guide_train.bins <- function(guide, scale, aesthetic = NULL) {
  breaks <- scale$break_info()
  if (length(breaks$major_source) == 0 || all(is.na(breaks$major_source))) {
    return()
  }

  # in the key data frame, use either the aesthetic provided as
  # argument to this function or, as a fall back, the first in the vector
  # of possible aesthetics handled by the scale
  aes_column_name <- aesthetic %||% scale$aesthetics[1]
  key <- new_data_frame(setNames(list(breaks$major), aes_column_name))

  label <- breaks$labels
  position <- breaks$major_source
  if (length(position) != nrow(key) + 1) {
    label <- c(NA, label, NA)
    position <- c(breaks$range[1], position, breaks$range[2])
  }
  labels <- new_data_frame(list(label = label, position = position))

  if (guide$reverse) {
    key <- key[nrow(key):1, ]
    labels <- labels[nrow(labels):1, ]
  }

  guide$key <- key
  guide$labels <- labels
  guide$hash <- with(
    guide,
    digest::digest(list(title, labels$label, direction, name))
  )
  guide
}

#' @export
guide_merge.bins <- function(guide, new_guide) {
  guide$key <- merge(guide$key, new_guide$key, sort = FALSE)
  guide$override.aes <- c(guide$override.aes, new_guide$override.aes)
  if (any(duplicated(names(guide$override.aes)))) {
    warning("Duplicated override.aes is ignored.")
  }
  guide$override.aes <- guide$override.aes[!duplicated(names(guide$override.aes))]
  guide
}

#' @export
guide_geom.bins <- function(guide, layers, default_mapping) {
  # arrange common data for vertical and horizontal guide
  guide$geoms <- lapply(layers, function(layer) {
    matched <- matched_aes(layer, guide, default_mapping)

    if (length(matched) > 0) {
      # This layer contributes to the legend

      # check if this layer should be included, different behaviour depending on
      # if show.legend is a logical or a named logical vector
      if (!is.null(names(layer$show.legend))) {
        layer$show.legend <- rename_aes(layer$show.legend)
        include <- is.na(layer$show.legend[matched]) ||
          layer$show.legend[matched]
      } else {
        include <- is.na(layer$show.legend) || layer$show.legend
      }

      if (include) {
        # Default is to include it

        # Filter out set aesthetics that can't be applied to the legend
        n <- vapply(layer$aes_params, length, integer(1))
        params <- layer$aes_params[n == 1]

        data <- layer$geom$use_defaults(guide$key[matched], params)
      } else {
        return(NULL)
      }
    } else {
      # This layer does not contribute to the legend
      if (is.na(layer$show.legend) || !layer$show.legend) {
        # Default is to exclude it
        return(NULL)
      } else {
        data <- layer$geom$use_defaults(NULL, layer$aes_params)[rep(1, nrow(guide$key)), ]
      }
    }

    # override.aes in guide_legend manually changes the geom
    data <- modify_list(data, guide$override.aes)

    list(
      draw_key = layer$geom$draw_key,
      data = data,
      params = c(layer$geom_params, layer$stat_params)
    )
  })

  # remove null geom
  guide$geoms <- compact(guide$geoms)

  # Finally, remove this guide if no layer is drawn
  if (length(guide$geoms) == 0) guide <- NULL
  guide
}

#' @export
guide_gengrob.bins <- function(guide, theme) {
  # settings of location and size
  if (guide$direction == "horizontal") {
    label.position <- guide$label.position %||% "bottom"
    if (!label.position %in% c("top", "bottom")) stop("label position \"", label.position, "\" is invalid")

    barwidth <- width_cm(guide$barwidth %||% (theme$legend.key.width * 5))
    barheight <- height_cm(guide$barheight %||% theme$legend.key.height)
  } else { # guide$direction == "vertical"
    label.position <- guide$label.position %||% "right"
    if (!label.position %in% c("left", "right")) stop("label position \"", label.position, "\" is invalid")

    barwidth <- width_cm(guide$barwidth %||% theme$legend.key.width)
    barheight <- height_cm(guide$barheight %||% (theme$legend.key.height * 5))
  }

  barlength <- switch(guide$direction, "horizontal" = barwidth, "vertical" = barheight)
  nbreak <- nrow(guide$key)

  # Keys
  key_width <- width_cm(
    guide$keywidth %||% theme$legend.key.width %||% theme$legend.key.size
  )
  key_height <- height_cm(
    guide$keyheight %||% theme$legend.key.height %||% theme$legend.key.size
  )

  key_size_mat <- do.call("cbind", lapply(guide$geoms, function(g) g$data$size / 10))
  if (nrow(key_size_mat) == 0 || ncol(key_size_mat) == 0) {
    key_size_mat <- matrix(0, ncol = 1, nrow = nbreak)
  }
  key_sizes <- apply(key_size_mat, 1, max)

  if (!is.null(guide$nrow) && !is.null(guide$ncol) &&
      guide$nrow * guide$ncol < nbreak) {
    stop(
      "`nrow` * `ncol` needs to be larger than the number of breaks",
      call. = FALSE
    )
  }

  # If neither nrow/ncol specified, guess with "reasonable" values
  if (is.null(guide$nrow) && is.null(guide$ncol)) {
    if (guide$direction == "horizontal") {
      guide$nrow <- ceiling(nbreak / 5)
    } else {
      guide$ncol <- ceiling(nbreak / 20)
    }
  }
  legend.nrow <- guide$nrow %||% ceiling(nbreak / guide$ncol)
  legend.ncol <- guide$ncol %||% ceiling(nbreak / guide$nrow)

  key_sizes <- matrix(
    c(key_sizes, rep(0, legend.nrow * legend.ncol - nbreak)),
    legend.nrow,
    legend.ncol,
    byrow = guide$byrow
  )

  key_widths <- pmax(key_width, apply(key_sizes, 2, max))
  key_heights <- pmax(key_height, apply(key_sizes, 1, max))

  bg <- element_render(theme, "legend.key")
  # grob for key
  key_size <- c(key_width, key_height) * 10
  draw_key <- function(i) {
    lapply(guide$geoms, function(g) {
      g$draw_key(g$data[i, ], g$params, key_size)
    })
  }
  grob.keys <- unlist(lapply(seq_len(nbreak), draw_key), recursive = FALSE)

  # make the bar grob (`grob.bar`)
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
  title_fontsize <- title.theme$size %||% calc_element("legend.title", theme)$size %||% 0

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
        lwd = guide$ticks.linewidth,
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
    b = 1 + max(vps$label.row), l = 1 + min(vps$label.col))
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
    b = 1 + max(vps$title.row), l = 1 + min(vps$title.col))
  gt <- gtable_add_grob(gt, grob.ticks, name = "ticks", clip = "off",
                        t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
                        b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))

  gt
}
