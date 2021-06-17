#' Legend guide
#'
#' Legend type guide shows key (i.e., geoms) mapped onto values.
#' Legend guides for various scales are integrated if possible.
#'
#' Guides can be specified in each `scale_*` or in [guides()].
#' `guide = "legend"` in `scale_*` is syntactic sugar for
#' `guide = guide_legend()` (e.g. `scale_color_manual(guide = "legend")`).
#' As for how to specify the guide for each scale in more detail,
#' see [guides()].
#'
#' @param title A character string or expression indicating a title of guide.
#'   If `NULL`, the title is not shown. By default
#'   ([waiver()]), the name of the scale object or the name
#'   specified in [labs()] is used for the title.
#' @param title.position A character string indicating the position of a
#'   title. One of "top" (default for a vertical guide), "bottom", "left"
#'  (default for a horizontal guide), or "right."
#' @param title.theme A theme object for rendering the title text. Usually the
#'   object of [element_text()] is expected. By default, the theme is
#'   specified by `legend.title` in [theme()] or theme.
#' @param title.hjust A number specifying horizontal justification of the
#'   title text.
#' @param title.vjust A number specifying vertical justification of the title
#'   text.
#' @param label logical. If `TRUE` then the labels are drawn. If
#'   `FALSE` then the labels are invisible.
#' @param label.position A character string indicating the position of a
#'   label. One of "top", "bottom" (default for horizontal guide), "left", or
#'   "right" (default for vertical guide).
#' @param label.theme A theme object for rendering the label text. Usually the
#'   object of [element_text()] is expected. By default, the theme is
#'   specified by `legend.text` in [theme()].
#' @param label.hjust A numeric specifying horizontal justification of the
#'   label text.
#' @param label.vjust A numeric specifying vertical justification of the label
#'   text.
#' @param keywidth A numeric or a [grid::unit()] object specifying
#'   the width of the legend key. Default value is `legend.key.width` or
#'   `legend.key.size` in [theme()].
#' @param keyheight A numeric or a [grid::unit()] object specifying
#'   the height of the legend key. Default value is `legend.key.height` or
#'   `legend.key.size` in [theme()].
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param default.unit A character string indicating [grid::unit()]
#'   for `keywidth` and `keyheight`.
#' @param override.aes A list specifying aesthetic parameters of legend key.
#'   See details and examples.
#' @param nrow The desired number of rows of legends.
#' @param ncol The desired number of column of legends.
#' @param byrow logical. If `FALSE` (the default) the legend-matrix is
#'   filled by columns, otherwise the legend-matrix is filled by rows.
#' @param reverse logical. If `TRUE` the order of legends is reversed.
#' @param order positive integer less than 99 that specifies the order of
#'   this guide among multiple guides. This controls the order in which
#'   multiple guides are displayed, not the contents of the guide itself.
#'   If 0 (default), the order is determined by a secret algorithm.
#' @param ... ignored.
#' @export
#' @family guides
#' @examples
#' \donttest{
#' df <- expand.grid(X1 = 1:10, X2 = 1:10)
#' df$value <- df$X1 * df$X2
#'
#' p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
#' p2 <- p1 + geom_point(aes(size = value))
#'
#' # Basic form
#' p1 + scale_fill_continuous(guide = guide_legend())
#'
#' # Control styles
#'
#' # title position
#' p1 + guides(fill = guide_legend(title = "LEFT", title.position = "left"))
#'
#' # title text styles via element_text
#' p1 + guides(fill =
#'   guide_legend(
#'     title.theme = element_text(
#'       size = 15,
#'       face = "italic",
#'       colour = "red",
#'       angle = 0
#'     )
#'   )
#' )
#'
#' # label position
#' p1 + guides(fill = guide_legend(label.position = "left", label.hjust = 1))
#'
#' # label styles
#' p1 +
#'   scale_fill_continuous(
#'     breaks = c(5, 10, 15),
#'     labels = paste("long", c(5, 10, 15)),
#'     guide = guide_legend(
#'       direction = "horizontal",
#'       title.position = "top",
#'       label.position = "bottom",
#'       label.hjust = 0.5,
#'       label.vjust = 1,
#'       label.theme = element_text(angle = 90)
#'     )
#'   )
#'
#' # Set aesthetic of legend key
#' # very low alpha value make it difficult to see legend key
#' p3 <- ggplot(mtcars, aes(vs, am, colour = factor(cyl))) +
#'   geom_jitter(alpha = 1/5, width = 0.01, height = 0.01)
#' p3
#' # override.aes overwrites the alpha
#' p3 + guides(colour = guide_legend(override.aes = list(alpha = 1)))
#'
#' # multiple row/col legends
#' df <- data.frame(x = 1:20, y = 1:20, color = letters[1:20])
#' p <- ggplot(df, aes(x, y)) +
#'   geom_point(aes(colour = color))
#' p + guides(col = guide_legend(nrow = 8))
#' p + guides(col = guide_legend(ncol = 8))
#' p + guides(col = guide_legend(nrow = 8, byrow = TRUE))
#'
#' # reversed order legend
#' p + guides(col = guide_legend(reverse = TRUE))
#' }
guide_legend <- function(# title
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

                         # general
                         direction = NULL,
                         default.unit = "line",
                         override.aes = list(),
                         nrow = NULL,
                         ncol = NULL,
                         byrow = FALSE,
                         reverse = FALSE,
                         order = 0,
                         ...) {

  if (!is.null(keywidth) && !is.unit(keywidth)) {
    keywidth <- unit(keywidth, default.unit)
  }
  if (!is.null(keyheight) && !is.unit(keyheight)) {
    keyheight <- unit(keyheight, default.unit)
  }

  structure(
    list(
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

      # size of key
      keywidth = keywidth,
      keyheight = keyheight,

      # general
      direction = direction,
      override.aes = rename_aes(override.aes),
      nrow = nrow,
      ncol = ncol,
      byrow = byrow,
      reverse = reverse,
      order = order,

      # parameter
      available_aes = c("any"),
      ...,
      name = "legend"
    ),
    class = c("guide", "legend")
  )
}

#' @export
guide_train.legend <- function(guide, scale, aesthetic = NULL) {
  breaks <- scale$get_breaks()
  if (length(breaks) == 0 || all(is.na(breaks))) {
    return()
  }

  # in the key data frame, use either the aesthetic provided as
  # argument to this function or, as a fall back, the first in the vector
  # of possible aesthetics handled by the scale
  aes_column_name <- aesthetic %||% scale$aesthetics[1]
  key <- new_data_frame(setNames(list(scale$map(breaks)), aes_column_name))
  key$.label <- scale$get_labels(breaks)

  # Drop out-of-range values for continuous scale
  # (should use scale$oob?)
  if (!scale$is_discrete()) {
    limits <- scale$get_limits()
    noob <- !is.na(breaks) & limits[1] <= breaks & breaks <= limits[2]
    key <- key[noob, , drop = FALSE]
  }


  if (guide$reverse) key <- key[nrow(key):1, ]

  guide$key <- key
  guide$hash <- with(
    guide,
    digest::digest(list(title, key$.label, direction, name))
  )
  guide
}

#' @export
guide_merge.legend <- function(guide, new_guide) {
  new_guide$key$.label <- NULL
  guide$key <- cbind(guide$key, new_guide$key)
  guide$override.aes <- c(guide$override.aes, new_guide$override.aes)
  if (any(duplicated(names(guide$override.aes)))) {
    warn("Duplicated override.aes is ignored.")
  }
  guide$override.aes <- guide$override.aes[!duplicated(names(guide$override.aes))]
  guide
}

#' @export
guide_geom.legend <- function(guide, layers, default_mapping) {
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

      data <- tryCatch(
        layer$geom$use_defaults(guide$key[matched], params, modifiers),
        error = function(...) {
          warn("Failed to apply `after_scale()` modifications to legend")
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
guide_gengrob.legend <- function(guide, theme) {

  # default setting
  label.position <- guide$label.position %||% "right"
  if (!label.position %in% c("top", "bottom", "left", "right"))
    abort(glue("label position `{label.position}` is invalid"))

  nbreak <- nrow(guide$key)

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
    just_defaults <- label_just_defaults.legend(guide$direction, label.position)
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

  key_size_mat <- do.call("cbind", lapply(guide$geoms, function(g) g$data$size / 10))
  if (nrow(key_size_mat) == 0 || ncol(key_size_mat) == 0) {
    key_size_mat <- matrix(0, ncol = 1, nrow = nbreak)
  }
  key_sizes <- apply(key_size_mat, 1, max)

  if (!is.null(guide$nrow) && !is.null(guide$ncol) &&
      guide$nrow * guide$ncol < nbreak) {
    abort("`nrow` * `ncol` needs to be larger than the number of breaks")
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

  label_widths <- apply(
    matrix(
      c(label_widths, rep(0, legend.nrow * legend.ncol - nbreak)),
      legend.nrow,
      legend.ncol,
      byrow = guide$byrow
    ),
    2,
    max
  )
  label_heights <- apply(
    matrix(
      c(label_heights, rep(0, legend.nrow * legend.ncol - nbreak)),
      legend.nrow,
      legend.ncol,
      byrow = guide$byrow
    ),
    1,
    max
  )

  if (guide$byrow) {
    vps <- new_data_frame(list(
      R = ceiling(seq(nbreak) / legend.ncol),
      C = (seq(nbreak) - 1) %% legend.ncol + 1
    ))
  } else {
    vps <- mat_2_df(arrayInd(seq(nbreak), dim(key_sizes)), c("R", "C"))
  }

  # layout of key-label depends on the direction of the guide
  if (guide$byrow == TRUE) {
    switch(
      label.position,
      "top" = {
        kl_widths <- pmax(label_widths, key_widths)
        kl_heights <- utils::head(
          interleave(label_heights, vgap, key_heights, vgap),
          -1
        )
        vps <- transform(
          vps,
          key.row = R * 4 - 1,
          key.col = C,
          label.row = R * 4 - 3,
          label.col = C
        )
      },
      "bottom" = {
        kl_widths <- pmax(label_widths, key_widths)
        kl_heights <- utils::head(
          interleave(key_heights, vgap, label_heights, vgap),
          -1
        )
        vps <- transform(
          vps,
          key.row = R * 4 - 3,
          key.col = C,
          label.row = R * 4 - 1,
          label.col = C
        )
      },
      "left" = {
        kl_widths <- utils::head(
          interleave(label_widths, hgap, key_widths, hgap),
          -1
        )
        kl_heights <- utils::head(
          interleave(pmax(label_heights, key_heights), vgap),
          -1
        )
        vps <- transform(
          vps,
          key.row = R * 2 - 1,
          key.col = C * 4 - 1,
          label.row = R * 2 - 1,
          label.col = C * 4 - 3
        )
      },
      "right" = {
        kl_widths <- utils::head(
          interleave(key_widths, hgap, label_widths, hgap),
          -1
        )
        kl_heights <- utils::head(
          interleave(pmax(label_heights, key_heights), vgap),
          -1
        )
        vps <- transform(
          vps,
          key.row = R * 2 - 1,
          key.col = C * 4 - 3,
          label.row = R * 2 - 1,
          label.col = C * 4 - 1
        )
      })
  } else {
    switch(
      label.position,
      "top" = {
        kl_widths <- utils::head(
          interleave(pmax(label_widths, key_widths), hgap),
          -1
        )
        kl_heights <- utils::head(
          interleave(label_heights, vgap, key_heights, vgap),
          -1
        )
        vps <- transform(
          vps,
          key.row = R * 4 - 1,
          key.col = C * 2 - 1,
          label.row = R * 4 - 3,
          label.col = C * 2 - 1
        )
      },
      "bottom" = {
        kl_widths <- utils::head(
          interleave(pmax(label_widths, key_widths), hgap),
          -1
        )
        kl_heights <- utils::head(
          interleave(key_heights, vgap, label_heights, vgap),
          -1
        )
        vps <- transform(
          vps,
          key.row = R * 4 - 3,
          key.col = C * 2 - 1,
          label.row = R * 4 - 1,
          label.col = C * 2 - 1
        )
      },
      "left" = {
        kl_widths <- utils::head(
          interleave(label_widths, hgap, key_widths, hgap),
          -1
        )
        kl_heights <- pmax(key_heights, label_heights)
        vps <- transform(
          vps,
          key.row = R,
          key.col = C * 4 - 1,
          label.row = R,
          label.col = C * 4 - 3
        )
      },
      "right" = {
        kl_widths <- utils::head(
          interleave(key_widths, hgap, label_widths, hgap),
          -1
        )
        kl_heights <- pmax(key_heights, label_heights)
        vps <- transform(
          vps,
          key.row = R,
          key.col = C * 4 - 3,
          label.row = R,
          label.col = C * 4 - 1
        )
      })
  }

  # layout the title over key-label
  switch(guide$title.position,
         "top" = {
           widths <- c(kl_widths, max(0, title_width - sum(kl_widths)))
           heights <- c(title_height, vgap, kl_heights)
           vps <- transform(
             vps,
             key.row = key.row + 2,
             key.col = key.col,
             label.row = label.row + 2,
             label.col = label.col
           )
           vps.title.row = 1; vps.title.col = 1:length(widths)
         },
         "bottom" = {
           widths <- c(kl_widths, max(0, title_width - sum(kl_widths)))
           heights <- c(kl_heights, vgap, title_height)
           vps <- transform(
             vps,
             key.row = key.row,
             key.col = key.col,
             label.row = label.row,
             label.col = label.col
           )
           vps.title.row = length(heights); vps.title.col = 1:length(widths)
         },
         "left" = {
           widths <- c(title_width, hgap, kl_widths)
           heights <- c(kl_heights, max(0, title_height - sum(kl_heights)))
           vps <- transform(
             vps,
             key.row = key.row,
             key.col = key.col + 2,
             label.row = label.row,
             label.col = label.col + 2
           )
           vps.title.row = 1:length(heights); vps.title.col = 1
         },
         "right" = {
           widths <- c(kl_widths, hgap, title_width)
           heights <- c(kl_heights, max(0, title_height - sum(kl_heights)))
           vps <- transform(
             vps,
             key.row = key.row,
             key.col = key.col,
             label.row = label.row,
             label.col = label.col
           )
           vps.title.row = 1:length(heights); vps.title.col = length(widths)
         })

  # grob for key
  key_size <- c(key_width, key_height) * 10

  draw_key <- function(i) {
    bg <- element_render(theme, "legend.key")
    keys <- lapply(guide$geoms, function(g) {
      g$draw_key(g$data[i, ], g$params, key_size)
    })
    c(list(bg), keys)
  }
  grob.keys <- unlist(lapply(seq_len(nbreak), draw_key), recursive = FALSE)

  # background
  grob.background <- element_render(theme, "legend.background")

  ngeom <- length(guide$geoms) + 1
  kcols <- rep(vps$key.col, each = ngeom)
  krows <- rep(vps$key.row, each = ngeom)

  # padding
  padding <- convertUnit(theme$legend.margin %||% margin(), "cm", valueOnly = TRUE)
  widths <- c(padding[4], widths, padding[2])
  heights <- c(padding[1], heights, padding[3])

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
    t = 1 + min(vps.title.row),
    r = 1 + max(vps.title.col),
    b = 1 + max(vps.title.row),
    l = 1 + min(vps.title.col)
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
    justify_grobs(
      grob.labels,
      hjust = hjust,
      vjust = vjust,
      int_angle = label.theme$angle,
      debug = label.theme$debug
    ),
    name = paste("label", vps$label.row, vps$label.col, sep = "-"),
    clip = "off",
    t = 1 + vps$label.row,
    r = 1 + vps$label.col,
    b = 1 + vps$label.row,
    l = 1 + vps$label.col
  )
  gt
}


#' Calculate the default hjust and vjust settings depending on legend
#' direction and position.
#'
#' @noRd
label_just_defaults.legend <- function(direction, position) {
  if (direction == "horizontal") {
    switch(
      position,
      "top" = list(hjust = 0.5, vjust = 0),
      "bottom" = list(hjust = 0.5, vjust = 1),
      "left" = list(hjust = 1, vjust = 0.5),
      list(hjust = 0, vjust = 0.5)
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


utils::globalVariables(c("C", "R", "key.row", "key.col", "label.row", "label.col"))
