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
#'   label text. The default for standard text is 0 (left-aligned) and 1
#'   (right-aligned) for expressions.
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
guide_legend <- function(
  # Title
  title          = waiver(),
  title.position = NULL,
  title.theme    = NULL,
  title.hjust    = NULL,
  title.vjust    = NULL,

  # Label
  label          = TRUE,
  label.position = NULL,
  label.theme    = NULL,
  label.hjust    = NULL,
  label.vjust    = NULL,

  # Key size
  keywidth  = NULL,
  keyheight = NULL,

  # General
  direction    = NULL,
  default.unit = "line",
  override.aes = list(),
  nrow         = NULL,
  ncol         = NULL,
  byrow        = FALSE,
  reverse      = FALSE,
  order        = 0,
  ...
) {
  # Resolve key sizes
  if (!inherits(keywidth, c("NULL", "unit"))) {
    keywidth <- unit(keywidth, default.unit)
  }
  if (!inherits(keyheight, c("NULL", "unit"))) {
    keyheight <- unit(keyheight, default.unit)
  }
  if (!is.null(title.position)) {
    title.position <- arg_match0(title.position, .trbl)
  }
  if (!is.null(label.position)) {
    label.position <- arg_match0(label.position, .trbl)
  }

  new_guide(
    # Title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # Label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # Key size
    keywidth  = keywidth,
    keyheight = keyheight,

    # General
    direction = direction,
    override.aes = rename_aes(override.aes),
    nrow = nrow,
    ncol = ncol,
    byrow = byrow,
    reverse = reverse,
    order = order,

    # Fixed parameters
    available_aes = "any",
    name  = "legend",
    super = GuideLegend
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideLegend <- ggproto(
  "GuideLegend", Guide,

  params = list(
    title = waiver(),
    title.position = NULL,
    title.theme = NULL,
    title.hjust = NULL,
    title.vjust = NULL,

    label = TRUE,
    label.position = NULL,
    label.theme = NULL,
    label.hjust = NULL,
    label.vjust = NULL,

    keywidth  = NULL,
    keyheight = NULL,

    # General
    direction = NULL,
    override.aes = list(),
    nrow = NULL,
    ncol = NULL,
    byrow = FALSE,
    reverse = FALSE,
    order = 0,

    name  = "legend",
    hash  = character(),
    position = NULL,
    direction = NULL
  ),

  available_aes = "any",

  hashables = exprs(title, key$.label, direction, name),

  elements = list(
    background  = "legend.background",
    margin      = "legend.margin",
    spacing     = "legend.spacing",
    spacing.x   = "legend.spacing.x",
    spacing.y   = "legend.spacing.y",
    key         = "legend.key",
    key.height  = "legend.key.height",
    key.width   = "legend.key.width",
    text        = "legend.text",
    theme.title = "legend.title"
  ),

  extract_params = function(scale, params,
                            title = waiver(), direction = NULL, ...) {
    params$title <- scale$make_title(
      params$title %|W|% scale$name %|W|% title
    )
    params$direction <- arg_match0(
      params$direction %||% direction,
      c("horizontal", "vertical"), arg_nm = "direction"
    )
    if (isTRUE(params$reverse %||% FALSE)) {
      params$key <- params$key[nrow(params$key):1, , drop = FALSE]
    }
    params
  },

  merge = function(self, params, new_guide, new_params) {
    # Combine keys
    new_params$key$.label <- new_params$key$.value <- NULL
    params$key <- vec_cbind(params$key, new_params$key)

    # Combine override.aes
    params$override.aes <- c(params$override.aes, new_params$override.aes)
    nms <- names(params$override.aes)
    if (anyDuplicated(nms)) {
      cli::cli_warn("Duplicated {.arg override.aes} is ignored.")
    }
    params$override.aes <- params$override.aes[!duplicated(nms)]

    list(guide = self, params = params)
  },

  # Arrange common data for vertical and horizontal legends
  get_layer_key = function(params, layers) {

    decor <- lapply(layers, function(layer) {

      matched_aes <- matched_aes(layer, params)

      # Check if this layer should be included
      if (!include_layer_in_guide(layer, matched_aes)) {
        return(NULL)
      }

      if (length(matched_aes) > 0) {
        # Filter out aesthetics that can't be applied to the legend
        n <- lengths(layer$aes_params, use.names = FALSE)
        layer_params <- layer$aes_params[n == 1]

        aesthetics  <- layer$computed_mapping
        is_modified <- is_scaled_aes(aesthetics) | is_staged_aes(aesthetics)
        modifiers   <- aesthetics[is_modified]

        data <- try_fetch(
          layer$geom$use_defaults(params$key[matched_aes],
                                  layer_params, modifiers),
          error = function(cnd) {
            cli::cli_warn(
              "Failed to apply {.fn after_scale} modifications to legend",
              parent = cnd
            )
            layer$geom$use_defaults(params$key[matched], layer_params, list())
          }
        )
      } else {
        reps <- rep(1, nrow(params$key))
        data <- layer$geom$use_defaults(NULL, layer$aes_params)[reps, ]
      }

      data <- modify_list(data, params$override.aes)

      list(
        draw_key = layer$geom$draw_key,
        data     = data,
        params   = c(layer$computed_geom_params, layer$computed_stat_params)
      )
    })

    # Remove NULL geoms
    params$decor <- compact(decor)

    if (length(params$decor) == 0) {
      return(NULL)
    }
    return(params)
  },

  setup_params = function(params) {
    if ("title.position" %in% names(params)) {
      params$title.position <- arg_match0(
        params$title.position %||%
          switch(params$direction, vertical = "top", horizontal = "left"),
        .trbl, arg_nm = "title.position"
      )
    }
    if ("label.position" %in% names(params)) {
      params$label.position <- arg_match0(
        params$label.position %||% "right",
        .trbl, arg_nm = "label.position"
      )
      params$rejust_labels <- TRUE
    }

    params$n_breaks <- n_breaks <- nrow(params$key)
    params$n_key_layers <- length(params$decor) + 1 # +1 is key background

    # Resolve shape
    if (!is.null(params$nrow) && !is.null(params$ncol) &&
        params$nrow * params$ncol < n_breaks) {
      cli::cli_abort(paste0(
        "{.arg nrow} * {.arg ncol} needs to be larger than the number of ",
        "breaks ({n_breaks})"
      ))
    }
    if (is.null(params$nrow) && is.null(params$ncol)) {
      if (params$direction == "horizontal") {
        params$nrow <- ceiling(n_breaks / 5)
      } else {
        params$ncol <- ceiling(n_breaks / 20)
      }
    }
    params$nrow <- params$nrow %||% ceiling(n_breaks / params$ncol)
    params$ncol <- params$ncol %||% ceiling(n_breaks / params$nrow)
    params
  },

  override_elements = function(params, elements, theme) {

    # Title
    title <- combine_elements(params$title.theme, elements$theme.title)
    title$hjust <- params$title.hjust %||% title$hjust %||% 0
    title$vjust <- params$title.vjust %||% title$vjust %||% 0.5
    elements$title <- title

    # Labels
    if (!is.null(elements$text)) {
      label <- combine_elements(params$label.theme, elements$text)
      if (!params$label || is.null(params$key$.label)) {
        label <- element_blank()
      } else {
        hjust <- unname(label_hjust_defaults[params$label.position])
        vjust <- unname(label_vjust_defaults[params$label.position])
        # Expressions default to right-justified
        if (hjust == 0 && any(is.expression(params$key$.label))) {
          hjust <- 1
        }
        # Breaking justification inheritance for intuition purposes.
        if (is.null(params$label.theme$hjust) &&
            is.null(theme$legend.text$hjust)) {
          label$hjust <- NULL
        }
        if (is.null(params$label.theme$vjust) &&
            is.null(theme$legend.text$vjust)) {
          label$vjust <- NULL
        }
        label$hjust <- params$label.hjust %||% label$hjust %||% hjust
        label$vjust <- params$label.vjust %||% label$vjust %||% vjust
      }
      elements$text <- label
    }

    # Keys
    if (any(c("key.width", "key.height") %in% names(elements))) {
      elements$key.width  <- width_cm( params$keywidth  %||% elements$key.width)
      elements$key.height <- height_cm(params$keyheight %||% elements$key.height)
    }

    # Spacing
    gap <- title$size %||% elements$theme.title$size %||%
      elements$text$size %||% 11
    gap <- unit(gap * 0.5, "pt")
    # Should maybe be elements$spacing.{x/y} instead of the theme's spacing?
    elements$hgap <- width_cm( theme$legend.spacing.x %||% gap)
    elements$vgap <- height_cm(theme$legend.spacing.y %||% gap)
    elements$padding <- convertUnit(
      elements$margin %||% margin(),
      "cm", valueOnly = TRUE
    )

    # Evaluate backgrounds early
    if (!is.null(elements$background)) {
      elements$background <- ggname(
        "legend.background", element_grob(elements$background)
      )
    }
    if (!is.null(elements$key)) {
      elements$key <- ggname(
        "legend.key", element_grob(elements$key)
      )
    }

    elements
  },

  build_ticks = function(...) {
    zeroGrob()
  },

  build_decor = function(decor, grobs, elements, params) {

    key_size <- c(elements$key.width, elements$key.height) * 10

    draw <- function(i) {
      bg <- elements$key
      keys <- lapply(decor, function(g) {
        g$draw_key(vec_slice(g$data, i), g$params, key_size)
      })
      c(list(bg), keys)
    }
    unlist(lapply(seq_len(params$n_breaks), draw), FALSE)
  },

  build_labels = function(key, elements, params) {
    n_labels <- length(key$.label)
    if (n_labels < 1) {
      out <- rep(list(zeroGrob()), nrow(key))
      return(out)
    }
    lapply(key$.label, function(lab) {
      ggname(
        "guide.label",
        element_grob(
          elements$text,
          label    = lab,
          margin_x = TRUE,
          margin_y = TRUE
        )
      )
    })
  },

  measure_grobs = function(grobs, params, elements) {
    byrow    <- params$byrow    %||% FALSE
    n_breaks <- params$n_breaks %||% 1L
    dim      <- c(params$nrow %||% 1L, params$ncol %||% 1L)

    # A guide may have already specified the size of the decoration, only
    # measure when it hasn't already.
    sizes <- params$sizes %||% measure_legend_keys(
      params$decor, n = n_breaks, dim = dim, byrow = byrow,
      default_width  = elements$key.width,
      default_height = elements$key.height
    )
    widths  <- sizes$widths
    heights <- sizes$heights

    # Measure label sizes
    zeroes   <- rep(0, prod(dim) - n_breaks) # size vector padding
    label_widths  <- apply(matrix(
      c(width_cm(grobs$labels), zeroes),
      nrow = dim[1], ncol = dim[2], byrow = byrow
    ), 2, max)
    label_heights <- apply(matrix(
      c(height_cm(grobs$labels), zeroes),
      nrow = dim[1], ncol = dim[2], byrow = byrow
    ), 1, max)

    # Interleave gaps between keys and labels, which depends on the label
    # position. For unclear reasons, we need to adjust some gaps based on the
    # `byrow` parameter (see also #4352).
    hgap <- elements$hgap %||% 0
    widths <- switch(
      params$label.position,
      "left"   = list(label_widths, hgap, widths, hgap),
      "right"  = list(widths, hgap, label_widths, hgap),
      list(pmax(label_widths, widths), hgap * (!byrow))
    )
    widths  <- head(vec_interleave(!!!widths),  -1)

    vgap <- elements$vgap %||% 0
    heights <- switch(
      params$label.position,
      "top"    = list(label_heights, vgap, heights, vgap),
      "bottom" = list(heights, vgap, label_heights, vgap),
      list(pmax(label_heights, heights), vgap * (byrow))
    )
    heights <- head(vec_interleave(!!!heights), -1)

    # Measure title
    title_width  <- width_cm(grobs$title)
    title_height <- height_cm(grobs$title)

    # Combine title with rest of the sizes based on its position
    widths <- switch(
      params$title.position,
      "left"  = c(title_width, hgap, widths),
      "right" = c(widths, hgap, title_width),
      c(widths, max(0, title_width - sum(widths)))
    )
    heights <- switch(
      params$title.position,
      "top"    = c(title_height, vgap, heights),
      "bottom" = c(heights, vgap, title_height),
      c(heights, max(0, title_height - sum(heights)))
    )

    list(
      widths  = widths,
      heights = heights,
      padding = elements$padding
    )
  },

  arrange_layout = function(key, sizes, params) {

    break_seq <- seq_len(params$n_breaks %||% 1L)
    dim <- c(params$nrow %||% 1L, params$ncol %||% 1L)

    # Find rows / columns of legend items
    if (params$byrow %||% FALSE) {
      df <- data_frame0(
        R = ceiling(break_seq / dim[2]),
        C = (break_seq - 1) %% dim[2] + 1
      )
    } else {
      df <- mat_2_df(arrayInd(break_seq, dim), c("R", "C"))
    }
    # Make spacing for padding / gaps. For example: because first gtable cell
    # will be padding, first item will be at [2, 2] position. Then the
    # second item-row will be [4, 2] because [3, 2] will be a gap cell.
    key_row <- label_row <- df$R * 2
    key_col <- label_col <- df$C * 2

    # Make gaps for key-label spacing depending on label position
    switch(
      params$label.position,
      "top" = {
        key_row   <- key_row   * 2
        label_row <- label_row * 2 - 2
      },
      "bottom" = {
        key_row   <- key_row   * 2 - 2
        label_row <- label_row * 2
      },
      "left" = {
        key_col   <- key_col   * 2
        label_col <- label_col * 2 - 2
      },
      "right" = {
        key_col   <- key_col   * 2 - 2
        label_col <- label_col * 2
      }
    )

    # Offset layout based on title position
    switch(
      params$title.position,
      "top" = {
        key_row   <- key_row   + 2
        label_row <- label_row + 2
        title_row <- 2
        title_col <- seq_along(sizes$widths) + 1
      },
      "bottom" = {
        title_row <- length(sizes$heights)   + 1
        title_col <- seq_along(sizes$widths) + 1
      },
      "left" = {
        key_col   <- key_col   + 2
        label_col <- label_col + 2
        title_row <- seq_along(sizes$heights) + 1
        title_col <- 2
      },
      "right" = {
        title_row <- seq_along(sizes$heights) + 1
        title_col <- length(sizes$widths)     + 1
      }
    )

    df <- cbind(df, key_row, key_col, label_row, label_col)

    list(layout = df, title_row = title_row, title_col = title_col)
  },

  assemble_drawing = function(grobs, layout, sizes, params, elements) {

    gt <- gtable(
      widths  = unit(c(sizes$padding[4], sizes$widths, sizes$padding[2]), "cm"),
      heights = unit(c(sizes$padding[1], sizes$heights, sizes$padding[3]), "cm")
    )

    # Add background
    if (!is.zero(elements$background)) {
      gt <- gtable_add_grob(
        gt, elements$background,
        name = "background", clip = "off",
        t = 1, r = -1, b = -1, l =1
      )
    }

    # Add title
    if (!is.zero(grobs$title)) {
      gt <- gtable_add_grob(
        gt,
        justify_grobs(
          grobs$title,
          hjust = elements$title$hjust,
          vjust = elements$title$vjust,
          int_angle = elements$title$angle,
          debug = elements$title$debug
        ),
        name = "title", clip = "off",
        t = min(layout$title_row), r = max(layout$title_col),
        b = max(layout$title_row), l = min(layout$title_col)
      )
    }

    # Extract appropriate part of layout
    layout   <- layout$layout

    # Add keys
    if (!is.zero(grobs$decor)) {
      n_key_layers <- params$n_key_layers %||% 1L
      key_cols <- rep(layout$key_col, each = n_key_layers)
      key_rows <- rep(layout$key_row, each = n_key_layers)

      # Add keys
      gt <- gtable_add_grob(
        gt, grobs$decor,
        name = names(grobs$decor) %||%
          paste("key", key_rows, key_cols, c("bg", seq_len(n_key_layers - 1)),
                sep = "-"),
        clip = "off",
        t = key_rows, r = key_cols, b = key_rows, l = key_cols
      )
    }

    if (!is.zero(grobs$labels)) {
      labels <- if (params$rejust_labels %||% TRUE) {
        justify_grobs(
          grobs$labels,
          hjust = elements$text$hjust, vjust = elements$text$vjust,
          int_angle = elements$text$angle, debug = elements$text$debug
        )
      } else {
        grobs$labels
      }

      gt <- gtable_add_grob(
        gt, labels,
        name = names(labels) %||%
          paste("label", layout$label_row, layout$label_col, sep = "-"),
        clip = "off",
        t = layout$label_row, r = layout$label_col,
        b = layout$label_row, l = layout$label_col
      )
    }

    gt
  }
)

label_hjust_defaults <- c(top = 0.5, bottom = 0.5, left = 1,   right = 0)
label_vjust_defaults <- c(top = 0,   bottom = 1,   left = 0.5, right = 0.5)

measure_legend_keys <- function(decor, n, dim, byrow = FALSE,
                                default_width = 1, default_height = 1) {
  if (is.null(decor)) {
    ans <- list(widths = NULL, heights = NULL)
    return(ans)
  }

  # Vector padding in case rows * cols > keys
  zeroes <- rep(0, prod(dim) - n)

  # For every layer, extract the size in cm
  size <- lapply(decor, function(g) {
    lwd <- g$data$linewidth %||% 0
    lwd[is.na(lwd)] <- 0
    size <- g$data$size %||% 0
    size[is.na(size)] <- 0
    vec_recycle((size + lwd) / 10, size = nrow(g$data))
  })
  size <- inject(cbind(!!!size))

  # Binned legends may have `n + 1` breaks, but we need to display `n` keys.
  size <- vec_slice(size, seq_len(n))

  # For every key, find maximum across all layers
  size <- apply(size, 1, max)

  # Apply legend layout
  size <- matrix(c(size, zeroes), nrow = dim[1], ncol = dim[2], byrow = byrow)

  list(
    widths  = pmax(default_width,  apply(size, 2, max)),
    heights = pmax(default_height, apply(size, 1, max))
  )
}
