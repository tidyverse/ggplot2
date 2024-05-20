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
#' @param theme A [`theme`][theme()] object to style the guide individually or
#'   differently from the plot's theme settings. The `theme` argument in the
#'   guide overrides, and is combined with, the plot's theme.
#' @param position A character string indicating where the legend should be
#'   placed relative to the plot panels.
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param override.aes A list specifying aesthetic parameters of legend key.
#'   See details and examples.
#' @param nrow,ncol The desired number of rows and column of legends
#'   respectively.
#' @param reverse logical. If `TRUE` the order of legends is reversed.
#' @param order positive integer less than 99 that specifies the order of
#'   this guide among multiple guides. This controls the order in which
#'   multiple guides are displayed, not the contents of the guide itself.
#'   If 0 (default), the order is determined by a secret algorithm.
#' @param ... ignored.
#' @export
#' @family guides
#' @seealso
#' The `r link_book("legends section", "scales-colour#sec-guide-legend")`
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
#' p1 + guides(fill = guide_legend(
#'   title = "LEFT", theme(legend.title.position = "left")
#' ))
#'
#' # title text styles via element_text
#' p1 + guides(fill = guide_legend(theme = theme(
#'   legend.title = element_text(size = 15, face = "italic", colour = "red")
#' )))
#'
#' # label position
#' p1 + guides(fill = guide_legend(theme = theme(
#'   legend.text.position = "left",
#'   legend.text = element_text(hjust = 1)
#' )))
#'
#' # label styles
#' p1 +
#'   scale_fill_continuous(
#'     breaks = c(5, 10, 15),
#'     labels = paste("long", c(5, 10, 15)),
#'     guide = guide_legend(theme = theme(
#'       legend.direction = "horizontal",
#'       legend.title.position = "top",
#'       legend.text.position = "bottom",
#'       legend.text = element_text(hjust = 0.5, vjust = 1, angle = 90)
#'     ))
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
#' p + guides(col = guide_legend(nrow = 8, theme = theme(legend.byrow = TRUE)))
#'
#' # reversed order legend
#' p + guides(col = guide_legend(reverse = TRUE))
#' }
guide_legend <- function(
  # Title
  title = waiver(),

  # Theme
  theme = NULL,

  # General
  position     = NULL,
  direction    = NULL,
  override.aes = list(),
  nrow         = NULL,
  ncol         = NULL,
  reverse      = FALSE,
  order        = 0,
  ...
) {

  theme <- deprecated_guide_args(theme, ...)

  if (!is.null(position)) {
    position <- arg_match0(position, c(.trbl, "inside"))
  }

  new_guide(
    # Title
    title = title,
    theme = theme,

    # General
    direction = direction,
    override.aes = rename_aes(override.aes),
    nrow = nrow,
    ncol = ncol,
    reverse = reverse,
    order = order,
    position = position,

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
    theme = NULL,

    # General
    override.aes = list(),
    nrow = NULL,
    ncol = NULL,
    reverse = FALSE,
    order = 0,

    name  = "legend",
    hash  = character(),
    position = NULL,
    direction = NULL
  ),

  available_aes = "any",

  hashables = exprs(title, key$.label, name),

  elements = list(
    background     = "legend.background",
    margin         = "legend.margin",
    key            = "legend.key",
    key_height     = "legend.key.height",
    key_width      = "legend.key.width",
    text           = "legend.text",
    theme.title    = "legend.title",
    spacing_x      = "legend.key.spacing.x",
    spacing_y      = "legend.key.spacing.y",
    text_position  = "legend.text.position",
    title_position = "legend.title.position",
    byrow          = "legend.byrow"
  ),

  extract_params = function(scale, params,
                            title = waiver(), ...) {
    params$title <- scale$make_title(params$title %|W|% scale$name %|W|% title)
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
  process_layers = function(self, params, layers, data = NULL) {

    include <- vapply(layers, function(layer) {
      aes <- matched_aes(layer, params)
      include_layer_in_guide(layer, aes)
    }, logical(1))

    if (!any(include)) {
      return(NULL)
    }

    self$get_layer_key(params, layers[include], data[include])
  },

  get_layer_key = function(params, layers, data) {

    decor <- Map(layer = layers, df = data, f = function(layer, df) {

      matched_aes <- matched_aes(layer, params)

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
            layer$geom$use_defaults(params$key[matched_aes], layer_params, list())
          }
        )
        data$.draw <- keep_key_data(params$key, df, matched_aes, layer$show.legend)
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
    return(params)
  },

  setup_params = function(params) {
    params$direction <- arg_match0(
      params$direction,
      c("horizontal", "vertical"), arg_nm = "direction"
    )
    params$n_breaks <- n_breaks <- nrow(params$key)
    params$n_key_layers <- length(params$decor) + 1 # +1 is key background

    # Resolve shape
    if (!is.null(params$nrow) && !is.null(params$ncol) &&
        params$nrow * params$ncol < n_breaks) {
      cli::cli_abort(paste0(
        "{.arg nrow} * {.arg ncol} needs to be larger than the number of ",
        "breaks ({n_breaks})."
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

  setup_elements = function(params, elements, theme) {
    theme <- add_theme(theme, params$theme)
    params$theme <- NULL

    # Resolve text positions
    text_position  <- theme$legend.text.position  %||% "right"
    title_position <- theme$legend.title.position %||% switch(
      params$direction,
      vertical = "top", horizontal = "left"
    )
    theme$legend.text.position <-
      arg_match0(text_position, .trbl, arg_nm = "legend.text.position")
    theme$legend.title.position <-
      arg_match0(title_position, .trbl, arg_nm = "legend.title.position")

    # Set default spacing
    theme$legend.key.spacing <- calc_element("legend.key.spacing", theme)
    gap <- theme$legend.key.spacing

    # For backward compatibility, default vertical spacing is no spacing
    if (params$direction == "vertical") {
      theme$legend.key.spacing.y <- theme$legend.key.spacing.y %||%
        unit(0, "pt")
    }

    # Resolve title. The trick here is to override the main text element, so
    # that any settings declared in `legend.title` will be honoured but we have
    # custom defaults for the guide.
    margin <- calc_element("text", theme)$margin
    title <- theme(text = element_text(
      hjust = 0, vjust = 0.5,
      margin = position_margin(title_position, margin, gap)
    ))
    elements$title <- calc_element("legend.title", add_theme(theme, title))

    # Resolve text, setting default justification and margins. Again, the
    # trick here is to set the main text element to propagate defaults while
    # honouring the `legend.text` settings.
    margin <- position_margin(text_position, margin, gap)
    text  <- theme(
      text = switch(
        text_position,
        top    = element_text(hjust = 0.5, vjust = 0.0, margin = margin),
        bottom = element_text(hjust = 0.5, vjust = 1.0, margin = margin),
        left   = element_text(hjust = 1.0, vjust = 0.5, margin = margin),
        right  = element_text(hjust = 0.0, vjust = 0.5, margin = margin)
      )
    )
    elements$text <- calc_element("legend.text", add_theme(theme, text))
    Guide$setup_elements(params, elements, theme)
  },

  override_elements = function(params, elements, theme) {

    if (any(c("key_width", "key_height") %in% names(elements))) {
      # Determine if the key is stretched
      elements$stretch_x <- unitType(elements$key_width) == "null"
      elements$stretch_y <- unitType(elements$key_height) == "null"
      # Convert key sizes to cm
      elements$width_cm  <- width_cm(elements$key_width)
      elements$height_cm <- height_cm(elements$key_height)
    }

    # Convert padding and spacing to cm
    if (any(c("spacing_x", "spacing_y") %in% names(elements))) {
      elements$spacing_x <- width_cm(elements$spacing_x)
      elements$spacing_y <- height_cm(elements$spacing_y)
    }

    elements$padding <-
      convertUnit(elements$margin %||% margin(), "cm", valueOnly = TRUE)

    # Evaluate backgrounds early
    if (!is.null(elements$background)) {
      elements$background <-
        ggname("legend.background", element_grob(elements$background))
    }
    if (!is.null(elements$key)) {
      elements$key <-
        ggname("legend.key", element_grob(elements$key))
    }

    elements
  },

  build_ticks = function(...) {
    zeroGrob()
  },

  build_decor = function(decor, grobs, elements, params) {

    key_size <- c(elements$width_cm, elements$height_cm) * 10

    draw <- function(i) {
      bg <- elements$key
      keys <- lapply(decor, function(g) {
        data <- vec_slice(g$data, i)
        if (data$.draw %||% TRUE) {
          key <- g$draw_key(data, g$params, key_size)
          set_key_size(key, data$linewidth, data$size, key_size / 10)
        } else {
          zeroGrob()
        }
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

    byrow    <- elements$byrow  %||% FALSE
    n_breaks <- params$n_breaks %||% 1L
    dim      <- c(params$nrow   %||% 1L, params$ncol %||% 1L)

    # A guide may have already specified the size of the decoration, only
    # measure when it hasn't already.
    sizes <- params$sizes %||% measure_legend_keys(
      grobs$decor, n = n_breaks, dim = dim, byrow = byrow,
      default_width  = elements$width_cm,
      default_height = elements$height_cm
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
    hgap <- elements$spacing_x %||% 0
    widths <- switch(
      elements$text_position,
      "left"   = list(label_widths, widths, hgap),
      "right"  = list(widths, label_widths, hgap),
      list(pmax(label_widths, widths), hgap)
    )
    widths  <- head(vec_interleave(!!!widths),  -1)

    vgap <- elements$spacing_y %||% 0
    heights <- switch(
      elements$text_position,
      "top"    = list(label_heights, heights, vgap),
      "bottom" = list(heights, label_heights, vgap),
      list(pmax(label_heights, heights), vgap)
    )
    heights <- head(vec_interleave(!!!heights), -1)

    list(widths = widths, heights = heights)
  },

  arrange_layout = function(key, sizes, params, elements) {

    break_seq <- seq_len(params$n_breaks %||% 1L)
    dim <- c(params$nrow %||% 1L, params$ncol %||% 1L)

    # Find rows / columns of legend items
    if (elements$byrow %||% FALSE) {
      row <- ceiling(break_seq / dim[2L])
      col <- (break_seq - 1L) %% dim[2L] + 1L
    } else {
      row <- (break_seq - 1L) %% dim[1L] + 1L
      col <- ceiling(break_seq / dim[1L])
    }
    # Account for spacing in between keys
    key_row <- row * 2 - 1
    key_col <- col * 2 - 1

    # Make gaps for key-label spacing depending on label position
    position <- elements$text_position
    key_row <- key_row + switch(position, top  = row, bottom = row - 1, 0)
    lab_row <- key_row + switch(position, top  = -1,  bottom = 1,       0)
    key_col <- key_col + switch(position, left = col, right  = col - 1, 0)
    lab_col <- key_col + switch(position, left = -1,  right  = 1,       0)

    data_frame0(
      key_row = key_row, key_col = key_col,
      label_row = lab_row, label_col = lab_col
    )
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {

    widths <- unit(sizes$widths, "cm")
    if (isTRUE(elements$stretch_x)) {
      widths[unique0(layout$key_col)] <- elements$key_width
    }

    heights <- unit(sizes$heights, "cm")
    if (isTRUE(elements$stretch_y)) {
      heights[unique0(layout$key_row)] <- elements$key_height
    }

    gt <- gtable(widths = widths, heights = heights)

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
      gt <- gtable_add_grob(
        gt, grobs$labels,
        name = names(labels) %||%
          paste("label", layout$label_row, layout$label_col, sep = "-"),
        clip = "off",
        t = layout$label_row, r = layout$label_col,
        b = layout$label_row, l = layout$label_col
      )
    }

    gt <- self$add_title(
      gt, grobs$title, elements$title_position,
      with(elements$title, rotate_just(angle, hjust, vjust))
    )

    gt <- gtable_add_padding(gt, unit(elements$padding, "cm"))

    # Add background
    if (!is.zero(elements$background)) {
      gt <- gtable_add_grob(
        gt, elements$background,
        name = "background", clip = "off",
        t = 1, r = -1, b = -1, l =1, z = -Inf
      )
    }
    gt
  }
)


measure_legend_keys <- function(keys, n, dim, byrow = FALSE,
                                default_width = 1, default_height = 1) {
  if (is.null(keys)) {
    ans <- list(widths = NULL, heights = NULL)
    return(ans)
  }

  # Vector padding in case rows * cols > keys
  padding_zeroes <- rep(0, prod(dim) - n)

  # For every layer, extract the size in cm
  widths  <- c(get_key_size(keys, "width", n),  padding_zeroes)
  heights <- c(get_key_size(keys, "height", n), padding_zeroes)

  # Apply legend layout
  widths  <- matrix(widths,  nrow = dim[1], ncol = dim[2], byrow = byrow)
  heights <- matrix(heights, nrow = dim[1], ncol = dim[2], byrow = byrow)

  list(
    widths  = pmax(default_width,  apply(widths,  2, max)),
    heights = pmax(default_height, apply(heights, 1, max))
  )
}

get_key_size <- function(keys, which = "width", n) {
  size <- lapply(keys, attr, which = which)
  size[lengths(size) != 1] <- 0
  size <- matrix(unlist(size), ncol = n)
  apply(size, 2, max)
}

set_key_size <- function(key, linewidth = NULL, size = NULL, default = NULL) {
  if (!is.null(attr(key, "width")) && !is.null(attr(key, 'height'))) {
    return(key)
  }
  if (!is.null(size) || !is.null(linewidth)) {
    size      <- size %||% 0
    linewidth <- linewidth %||% 0
    size      <- if (is.na(size)[1]) 0 else size[1]
    linewidth <- if (is.na(linewidth)[1]) 0 else linewidth[1]
    size <- (size + linewidth) / 10 # From mm to cm
  } else {
    size <- NULL
  }
  attr(key, "width")  <- attr(key, "width",  TRUE) %||% size %||% default[1]
  attr(key, "height") <- attr(key, "height", TRUE) %||% size %||% default[2]
  key
}

# For legend keys, check if the guide key's `.value` also occurs in the layer
# data when `show.legend = NA` and data is discrete. Note that `show.legend`
# besides TRUE (always show), FALSE (never show) and NA (show in relevant legend),
# can also take *named* logical vector to set this behaviour per aesthetic.
keep_key_data <- function(key, data, aes, show) {
  # First, can we exclude based on anything else than actually checking the
  # data that we should include or drop the key?
  if (!is.discrete(key$.value)) {
    return(TRUE)
  }
  if (is_named(show)) {
    aes  <- intersect(aes, names(show))
    show <- show[aes]
  } else {
    show <- show[rep(1L, length(aes))]
  }
  if (isTRUE(any(show)) || length(show) == 0) {
    return(TRUE)
  }
  if (isTRUE(all(!show))) {
    return(FALSE)
  }
  # Second, we go find if the value is actually present in the data.
  aes <- aes[is.na(show)]
  match <- which(names(data) %in% aes)
  if (length(match) == 0) {
    return(TRUE)
  }
  keep <- rep(FALSE, nrow(key))
  for (column in match) {
    keep <- keep | key$.value %in% data[[column]]
  }

  # NA might be included in breaks but originate from non-missing values that
  # map to NA instead of *being* NA. We double-check if there are values
  # outside the non-missing conventional limits.
  is_na <- which(is.na(key$.value) & !keep)
  if (length(is_na) > 0) {
    na_keep <- FALSE
    for (column in match) {
      na_keep <- na_keep || !all(data[[column]] %in% key$.value)
    }
    keep[is_na] <- na_keep
  }

  keep
}

position_margin <- function(position, margin = NULL, gap = unit(0, "pt")) {
  margin <- margin %||% margin()
  switch(
    position,
    top    = replace(margin, 3, margin[3] + gap),
    bottom = replace(margin, 1, margin[1] + gap),
    left   = replace(margin, 2, margin[2] + gap),
    right  = replace(margin, 4, margin[4] + gap)
  )
}

# Function implementing backward compatibility with the old way of specifying
# guide styling
deprecated_guide_args <- function(
  theme = NULL,
  title.position = NULL,
  title.theme = NULL, title.hjust = NULL, title.vjust = NULL,
  label = NULL,
  label.position = NULL,
  label.theme = NULL, label.hjust = NULL, label.vjust = NULL,
  keywidth  = NULL, keyheight = NULL, barwidth  = NULL, barheight = NULL,
  byrow = NULL,
  frame.colour = NULL, frame.linewidth = NULL, frame.linetype = NULL,
  ticks = NULL, ticks.colour = NULL, ticks.linewidth = NULL,
  axis = NULL, axis.colour = NULL, axis.linewidth = NULL, axis.arrow = NULL,
  default.unit = "line",
  ...,
  .call = caller_call()) {

  args <- names(formals(deprecated_guide_args))
  args <- setdiff(args, c("theme", "default.unit", "...", ".call"))
  vals <- compact(mget(args, current_env()))

  # Early exit when no old arguments have been supplied
  if (length(vals) == 0) {
    return(theme)
  }
  fun_name <- call_name(.call)
  replacement <- paste0(fun_name, "(theme)")
  for (arg_name in names(vals)) {
    deprecate_soft0(
      when = "3.5.0",
      what = paste0(fun_name, "(", arg_name, ")"),
      with = replacement
    )
  }
  def_unit <- function(x) {
    if (is.null(x) || is.unit(x)) {
      return(x)
    }
    unit(x, default.unit)
  }

  theme <- theme %||% theme()

  # Resolve straightforward arguments
  theme <- replace_null(
    theme,
    legend.title.position = title.position,
    legend.text.position  = label.position,
    legend.byrow          = byrow,
    legend.key.width      = def_unit(keywidth  %||% barwidth),
    legend.key.height     = def_unit(keyheight %||% barheight)
  )

  # Set legend.text
  if (isFALSE(label)) {
    label.theme <- element_blank()
  } else if (!is.null(label.theme %||% label.hjust %||% label.vjust)) {
    label.theme <- label.theme %||% element_text()
    label.theme <- replace_null(
      label.theme,
      hjust = label.hjust %||% label.theme$hjust,
      vjust = label.vjust %||% label.theme$vjust
    )
  }
  theme$legend.text <- theme$legend.text %||% label.theme

  # Set legend.title
  if (!is.null(title.hjust %||% title.vjust)) {
    title.theme <- title.theme %||% element_text()
    title.theme <- replace_null(
      title.theme,
      hjust = title.hjust %||% title.theme$hjust,
      vjust = title.vjust %||% title.theme$vjust
    )
  }
  theme$legend.title <- theme$legend.title %||% title.theme

  # Set legend.frame
  if (!is.null(frame.colour %||% frame.linewidth %||% frame.linetype)) {
    frame <- theme$legend.frame %||% element_rect(
      colour    = frame.colour,
      linewidth = frame.linewidth,
      linetype  = frame.linetype
    )
    theme$legend.frame <- theme$legend.frame %||% frame
  }

  # Set legend.ticks
  if (isFALSE(ticks)) {
    ticks <- element_blank()
  } else if (!is.null(ticks.colour %||% ticks.linewidth)) {
    ticks <- element_line(colour = ticks.colour, linewidth = ticks.linewidth)
    theme$legend.ticks <- theme$legend.ticks %||% ticks
  }

  # Set legend.axis
  if (isFALSE(axis)) {
    axis <- element_blank()
  } else if (!is.null(axis.colour %||% axis.linewidth %||% axis.arrow)) {
    axis <- element_line(
      colour = axis.colour,
      linewidth = axis.linewidth,
      arrow = axis.arrow
    )
    theme$legend.axis.line <- theme$legend.axis.line %||% axis
  }

  # Set as theme
  theme <- compact(theme)
  if (!is.theme(theme)) {
    theme <- inject(theme(!!!theme))
  }
  theme
}
