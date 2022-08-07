#' A binned version of guide_legend
#'
#' This guide is a version of the [guide_legend()] guide for binned scales. It
#' differs in that it places ticks correctly between the keys, and sports a
#' small axis to better show the binning. Like [guide_legend()] it can be used
#' for all non-position aesthetics though colour and fill defaults to
#' [guide_coloursteps()], and it will merge aesthetics together into the same
#' guide if they are mapped in the same way.
#'
#' @inheritParams guide_legend
#' @param axis Logical. Should a small axis be drawn along the guide
#' @param axis.colour,axis.linewidth Graphic specifications for the look of the
#'   axis.
#' @param axis.arrow A call to `arrow()` to specify arrows at the end of the
#'   axis line, thus showing an open interval.
#' @param show.limits Logical. Should the limits of the scale be shown with
#'   labels and ticks. Default is `NULL` meaning it will take the value from the
#'   scale. This argument is ignored if `labels` is given as a vector of
#'   values. If one or both of the limits is also given in `breaks` it will be
#'   shown irrespective of the value of `show.limits`.
#'
#' @section Use with discrete scale:
#' This guide is intended to show binned data and work together with ggplot2's
#' binning scales. However, it is sometimes desirable to perform the binning in
#' a separate step, either as part of a stat (e.g. [stat_contour_filled()]) or
#' prior to the visualisation. If you want to use this guide for discrete data
#' the levels must follow the naming scheme implemented by [base::cut()]. This
#' means that a bin must be encoded as `"(<lower>, <upper>]"` with `<lower>`
#' giving the lower bound of the bin and `<upper>` giving the upper bound
#' (`"[<lower>, <upper>)"` is also accepted). If you use [base::cut()] to
#' perform the binning everything should work as expected, if not, some recoding
#' may be needed.
#'
#' @return A guide object
#' @family guides
#' @export
#'
#' @examples
#' p <- ggplot(mtcars) +
#'   geom_point(aes(disp, mpg, size = hp)) +
#'   scale_size_binned()
#'
#' # Standard look
#' p
#'
#' # Remove the axis or style it
#' p + guides(size = guide_bins(axis = FALSE))
#'
#' p + guides(size = guide_bins(show.limits = TRUE))
#'
#' p + guides(size = guide_bins(
#'   axis.arrow = arrow(length = unit(1.5, 'mm'), ends = 'both')
#' ))
#'
#' # Guides are merged together if possible
#' ggplot(mtcars) +
#'   geom_point(aes(disp, mpg, size = hp, colour = hp)) +
#'   scale_size_binned() +
#'   scale_colour_binned(guide = "bins")
#'
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

  # key
  keywidth = NULL,
  keyheight = NULL,

  # ticks
  axis = TRUE,
  axis.colour = "black",
  axis.linewidth = 0.5,
  axis.arrow = NULL,

  # general
  direction = NULL,
  default.unit = "line",
  override.aes = list(),
  reverse = FALSE,
  order = 0,
  show.limits = NULL,
  ...) {

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

    # key
    keywidth = keywidth,
    keyheight = keyheight,

    # ticks
    axis = axis,
    axis.colour = axis.colour,
    axis.linewidth = axis.linewidth,
    axis.arrow = axis.arrow,

    # general
    direction = direction,
    override.aes = rename_aes(override.aes),
    default.unit = default.unit,
    reverse = reverse,
    order = order,
    show.limits = show.limits,

    # parameter
    available_aes = c("any"),
    ...,
    name = "bins"),
    class = c("guide", "bins")
  )
}

#' @export
guide_train.bins <- function(guide, scale, aesthetic = NULL) {
  breaks <- scale$get_breaks()
  breaks <- breaks[!is.na(breaks)]
  if (length(breaks) == 0 || all(is.na(breaks))) {
    return()
  }
  show_limits <- guide$show.limits %||% scale$show.limits %||% FALSE
  if (show_limits && (is.character(scale$labels) || is.numeric(scale$labels))) {
    cli::cli_warn(c(
      "{.arg show.limits} is ignored when {.arg labels} are given as a character vector",
      "i" = "Either add the limits to {.arg breaks} or provide a function for {.arg labels}"
    ))
    show_limits <- FALSE
  }
  # in the key data frame, use either the aesthetic provided as
  # argument to this function or, as a fall back, the first in the vector
  # of possible aesthetics handled by the scale
  aes_column_name <- aesthetic %||% scale$aesthetics[1]

  if (is.numeric(breaks)) {
    limits <- scale$get_limits()
    if (!is.numeric(scale$breaks)) {
      breaks <- breaks[!breaks %in% limits]
    }
    all_breaks <- unique0(c(limits[1], breaks, limits[2]))
    bin_at <- all_breaks[-1] - diff(all_breaks) / 2
  } else {
    # If the breaks are not numeric it is used with a discrete scale. We check
    # if the breaks follow the allowed format "(<lower>, <upper>]", and if it
    # does we convert it into bin specs
    bin_at <- breaks
    breaks <- as.character(breaks)
    breaks <- strsplit(gsub("\\(|\\)|\\[|\\]", "", breaks), ",\\s?")
    breaks <- as.numeric(unlist(breaks))
    if (anyNA(breaks)) {
      cli::cli_abort(c(
        "Breaks not formatted correctly for a bin legend.",
        "i" = "Use {.code (<lower>, <upper>]} format to indicate bins"
      ))
    }
    all_breaks <- breaks[c(1, seq_along(bin_at) * 2)]
    limits <- all_breaks[c(1, length(all_breaks))]
    breaks <- all_breaks[-c(1, length(all_breaks))]
  }
  key <- data_frame(c(scale$map(bin_at), NA), .name_repair = ~ aes_column_name)
  labels <- scale$get_labels(breaks)
  show_limits <- rep(show_limits, 2)
  if (is.character(scale$labels) || is.numeric(scale$labels)) {
    limit_lab <- c(NA, NA)
  } else {
    limit_lab <- scale$get_labels(limits)
  }
  if (!breaks[1] %in% limits) {
    labels <- c(limit_lab[1], labels)
  } else {
    show_limits[1] <- TRUE
  }
  if (!breaks[length(breaks)] %in% limits) {
    labels <- c(labels, limit_lab[2])
  } else {
    show_limits[2] <- TRUE
  }
  key$.label <- labels
  guide$show.limits <- show_limits

  if (guide$reverse) {
    key <- key[rev(seq_len(nrow(key))), ]
    # Move last row back to last
    aesthetics <- setdiff(names(key), ".label")
    key[, aesthetics] <- key[c(seq_len(nrow(key))[-1], 1), aesthetics]
  }

  guide$key <- key
  guide$hash <- with(
    guide,
    hash(list(title, key$.label, direction, name))
  )
  guide
}

#' @export
guide_merge.bins <- function(guide, new_guide) {
  guide$key <- merge(guide$key, new_guide$key, sort = FALSE)
  guide$override.aes <- c(guide$override.aes, new_guide$override.aes)
  if (any(duplicated(names(guide$override.aes)))) {
    cli::cli_warn("Duplicated {.arg override.aes} is ignored.")
  }
  guide$override.aes <- guide$override.aes[!duplicated(names(guide$override.aes))]
  guide
}

#' @export
guide_geom.bins <- function(guide, layers, default_mapping) {
  # arrange common data for vertical and horizontal guide
  guide$geoms <- lapply(layers, function(layer) {
    matched <- matched_aes(layer, guide)

    # check if this layer should be included
    include <- include_layer_in_guide(layer, matched)

    if (!include) {
      return(NULL)
    }

    if (length(matched) > 0) {
      # Filter out set aesthetics that can't be applied to the legend
      n <- vapply(layer$aes_params, length, integer(1))
      params <- layer$aes_params[n == 1]

      aesthetics <- layer$computed_mapping
      modifiers <- aesthetics[is_scaled_aes(aesthetics) | is_staged_aes(aesthetics)]

      data <- try_fetch(
        layer$geom$use_defaults(guide$key[matched], params, modifiers),
        error = function(cnd) {
          cli::cli_warn("Failed to apply {.fn after_scale} modifications to legend", parent = cnd)
          layer$geom$use_defaults(guide$key[matched], params, list())
        }
      )
    } else {
      data <- layer$geom$use_defaults(NULL, layer$aes_params)[rep(1, nrow(guide$key)), ]
    }

    # override.aes in guide_legend manually changes the geom
    data <- modify_list(data, guide$override.aes)

    list(
      draw_key = layer$geom$draw_key,
      data = data,
      params = c(layer$computed_geom_params, layer$computed_stat_params)
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
  guide$key$.label[c(1, nrow(guide$key))[!guide$show.limits]] <- NA

  # default setting
  if (guide$direction == "horizontal") {
    label.position <- guide$label.position %||% "bottom"
    if (!label.position %in% c("top", "bottom")) {
      cli::cli_warn("Ignoring invalid {.arg label.position}")
      label.position <- "bottom"
    }
  } else {
    label.position <- guide$label.position %||% "right"
    if (!label.position %in% c("left", "right")) {
      cli::cli_warn("Ignoring invalid {.arg label.position}")
      label.position <- "right"
    }
  }

  n_keys <- nrow(guide$key) - 1

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

  # first get the label theme, we need it below even when there are no labels
  label.theme <- guide$label.theme %||% calc_element("legend.text", theme)

  if (!guide$label || is.null(guide$key$.label)) {
    grob.labels <- rep(list(zeroGrob()), nrow(guide$key))
  } else {
    # get the defaults for label justification. The defaults are complicated and depend
    # on the direction of the legend and on label placement
    just_defaults <- label_just_defaults.bins(guide$direction, label.position)
    # don't set expressions left-justified
    if (just_defaults$hjust == 0 && any(is.expression(guide$key$.label))) just_defaults$hjust <- 1

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

    grob.labels <- lapply(guide$key$.label, function(label, ...) {
      g <- element_grob(
        element = label.theme,
        label = label,
        hjust = hjust,
        vjust = vjust,
        margin_x = TRUE,
        margin_y = TRUE
      )
      ggname("guide.label", g)
    })
    grob.labels[c(1, length(grob.labels))[!guide$show.limits]] <- list(zeroGrob())
  }

  label_widths <- width_cm(grob.labels)
  label_heights <- height_cm(grob.labels)

  # Keys
  key_width <- width_cm(
    guide$keywidth %||% theme$legend.key.width %||% theme$legend.key.size
  )
  key_height <- height_cm(
    guide$keyheight %||% theme$legend.key.height %||% theme$legend.key.size
  )

  key_size <- lapply(guide$geoms, function(g) g$data$size / 10)
  key_size_mat <- inject(cbind(!!!key_size))

  # key_size_mat can be an empty matrix (e.g. the data doesn't contain size
  # column), so subset it only when it has any rows and columns.
  if (nrow(key_size_mat) == 0 || ncol(key_size_mat) == 0) {
    key_size_mat <- matrix(0, ncol = 1, nrow = n_keys)
  } else {
    key_size_mat <- key_size_mat[seq_len(n_keys), , drop = FALSE]
  }
  key_sizes <- apply(key_size_mat, 1, max)

  if (guide$direction == "horizontal") {
    key.nrow <- 1
    key.ncol <- n_keys
    label.nrow <- 1
    label.ncol <- n_keys + 1
  } else {
    key.nrow <- n_keys
    key.ncol <- 1
    label.nrow <- n_keys + 1
    label.ncol <- 1
  }

  key_sizes <- matrix(key_sizes, key.nrow, key.ncol)
  label_sizes <- matrix(label_widths, label.nrow, label.ncol)

  key_widths <- max(key_width, apply(key_sizes, 2, max))
  key_heights <- max(key_height, apply(key_sizes, 1, max))

  label_widths <- max(apply(label_sizes, 2, max))
  label_heights <- max(apply(label_sizes, 1, max))

  key_loc <- data_frame0(
    R = seq(2, by = 2, length.out = n_keys),
    C = if (label.position %in% c("right", "bottom")) 1 else 3
  )
  label_loc <- data_frame0(
    R = seq(1, by = 2, length.out = n_keys + 1),
    C = if (label.position %in% c("right", "bottom")) 3 else 1
  )
  tick_loc <- label_loc
  tick_loc$C <- if (label.position %in% c("right", "bottom")) 1 else 3

  widths <- c(key_widths, hgap, label_widths)
  if (label.position != "right") widths <- rev(widths)
  heights <- c(interleave(rep(0, n_keys), key_heights), 0)
  if (guide$direction == "horizontal") {
    names(key_loc) <- c("C", "R")
    names(label_loc) <- c("C", "R")
    names(tick_loc) <- c("C", "R")
    heights <- c(key_heights, vgap, label_heights)
    if (label.position != "bottom") heights <- rev(heights)
    widths <- c(interleave(rep(0, n_keys), key_widths), 0)
  }

  # layout the title over key-label
  switch(guide$title.position,
    "top" = {
      widths <- c(widths, max(0, title_width - sum(widths)))
      heights <- c(title_height, vgap, heights)
      key_loc$R <- key_loc$R + 2
      label_loc$R <- label_loc$R + 2
      tick_loc$R <- tick_loc$R + 2
      title_row = 1
      title_col = seq_along(widths)
    },
    "bottom" = {
      widths <- c(widths, max(0, title_width - sum(widths)))
      heights <- c(heights, vgap, title_height)
      title_row = length(heights)
      title_col = seq_along(widths)
    },
    "left" = {
      widths <- c(title_width, hgap, widths)
      heights <- c(heights, max(0, title_height - sum(heights)))
      key_loc$C <- key_loc$C + 2
      label_loc$C <- label_loc$C + 2
      tick_loc$C <- tick_loc$C + 2
      title_row = seq_along(heights)
      title_col = 1
    },
    "right" = {
      widths <- c(widths, hgap, title_width)
      heights <- c(heights, max(0, title_height - sum(heights)))
      title_row = seq_along(heights)
      title_col = length(widths)
    }
  )

  # grob for key
  key_size <- c(key_width, key_height) * 10

  draw_key <- function(i) {
    bg <- element_render(theme, "legend.key")
    keys <- lapply(guide$geoms, function(g) {
      g$draw_key(g$data[i, ], g$params, key_size)
    })
    c(list(bg), keys)
  }
  grob.keys <- unlist(lapply(seq_len(n_keys), draw_key), recursive = FALSE)

  # background
  grob.background <- element_render(theme, "legend.background")

  ngeom <- length(guide$geoms) + 1
  kcols <- rep(key_loc$C, each = ngeom)
  krows <- rep(key_loc$R, each = ngeom)

  # padding
  padding <- convertUnit(theme$legend.margin %||% margin(), "cm", valueOnly = TRUE)
  widths <- c(padding[4], widths, padding[2])
  heights <- c(padding[1], heights, padding[3])

  # make the ticks grob (`grob.ticks`)
  if (!guide$axis) {
    grob.ticks <- zeroGrob()
    grob.axis <- zeroGrob()
  } else {
    if (guide$direction == "horizontal") {
      x0 <- 0.5
      y0 <- 0
      x1 <- 0.5
      y1 <- 1/5
      axis_x <- c(0, 1)
      axis_y <- c(0, 0)
      if (label.position == "top") {
        y0 <- 4/5
        y1 <- 1
        axis_y <- c(1, 1)
      }
    } else { # guide$direction == "vertical"
      y0 <- 0.5
      x0 <- 4/5
      y1 <- 0.5
      x1 <- 1
      axis_x <- c(1, 1)
      axis_y <- c(0, 1)
      if (label.position == "left") {
        x0 <- 0
        x1 <- 1/5
        axis_x <- c(0, 0)
      }
    }
    grob.ticks <- segmentsGrob(
      x0 = x0, y0 = y0, x1 = x1, y1 = y1,
      default.units = "npc",
      gp = gpar(
        col = guide$axis.colour,
        lwd = guide$axis.linewidth,
        lineend = "butt"
      )
    )
    grob.axis <- segmentsGrob(
      x0 = axis_x[1], y0 = axis_y[1], x1 = axis_x[2], y1 = axis_y[2],
      default.units = "npc",
      arrow = guide$axis.arrow,
      gp = gpar(
        col = guide$axis.colour,
        lwd = guide$axis.linewidth,
        lineend = if (is.null(guide$axis.arrow)) "square" else "round"
      )
    )
  }
  grob.ticks <- rep_len(list(grob.ticks), length(grob.labels))
  grob.ticks[c(1, length(grob.ticks))[!guide$show.limits]] <- list(zeroGrob())

  # Create the gtable for the legend
  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(
    gt,
    grob.background,
    name = "background",
    clip = "off",
    t = 1,
    r = -1,
    b = -1,
    l = 1
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
    t = 1 + min(title_row),
    r = 1 + max(title_col),
    b = 1 + max(title_row),
    l = 1 + min(title_col)
  )
  gt <- gtable_add_grob(
    gt,
    grob.keys,
    name = paste("key", krows, kcols, c("bg", seq(ngeom - 1)), sep = "-"),
    clip = "off",
    t = 1 + krows,
    r = 1 + kcols,
    b = 1 + krows,
    l = 1 + kcols
  )
  gt <- gtable_add_grob(
    gt,
    grob.ticks,
    name = paste("tick", tick_loc$R, tick_loc$C, sep = "-"),
    clip = "off",
    t = 1 + tick_loc$R,
    r = 1 + tick_loc$C,
    b = 1 + tick_loc$R,
    l = 1 + tick_loc$C
  )
  gt <- gtable_add_grob(
    gt,
    grob.axis,
    name = "axis",
    clip = "off",
    t = min(1 + tick_loc$R),
    r = min(1 + tick_loc$C),
    b = max(1 + tick_loc$R),
    l = max(1 + tick_loc$C)
  )
  gt <- gtable_add_grob(
    gt,
    justify_grobs(
      grob.labels,
      hjust = hjust,
      vjust = vjust,
      int_angle = label.theme$angle,
      debug = label.theme$debug
    ),
    name = paste("label", label_loc$R, label_loc$C, sep = "-"),
    clip = "off",
    t = 1 + label_loc$R,
    r = 1 + label_loc$C,
    b = 1 + label_loc$R,
    l = 1 + label_loc$C
  )
  gt
}

#' Calculate the default hjust and vjust settings depending on legend
#' direction and position.
#'
#' @noRd
label_just_defaults.bins <- function(direction, position) {
  if (direction == "horizontal") {
    switch(
      position,
      "top" = list(hjust = 0.5, vjust = 0),
      "bottom" = list(hjust = 0.5, vjust = 1),
      "left" = list(hjust = 1, vjust = 0.5),
      list(hjust = 0.5, vjust = 0.5)
    )
  }
  else {
    switch(
      position,
      "top" = list(hjust = 0.5, vjust = 0),
      "bottom" = list(hjust = 0.5, vjust = 1),
      "left" = list(hjust = 1, vjust = 0.5),
      list(hjust = 0, vjust = 0.5)
    )

  }
}
