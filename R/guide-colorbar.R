#' @include guide-legend.R
NULL

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
#' @param nbin A numeric specifying the number of bins for drawing the
#'   colourbar. A smoother colourbar results from a larger value.
#' @param display A string indicating a method to display the colourbar. Can be
#'   one of the following:
#'
#'   * `"raster"` to display as a bitmap image.
#'   * `"rectangles"` to display as a series of rectangles.
#'   * `"gradient"` to display as a linear gradient.
#'
#'   Note that not all devices are able to render rasters and gradients.
#' @param raster `r lifecycle::badge("deprecated")`  A logical. If `TRUE` then
#'   the colourbar is rendered as a raster object. If `FALSE` then the colourbar
#'   is rendered as a set of rectangles. Note that not all graphics devices are
#'   capable of rendering raster image.
#' @param alpha A numeric between 0 and 1 setting the colour transparency of
#'   the bar. Use `NA` to preserve the alpha encoded in the colour itself
#'   (default).
#' @param draw.ulim A logical specifying if the upper limit tick marks should
#'   be visible.
#' @param draw.llim A logical specifying if the lower limit tick marks should
#'   be visible.
#' @param direction  A character string indicating the direction of the guide.
#'   One of "horizontal" or "vertical."
#' @param reverse logical. If `TRUE` the colourbar is reversed. By default,
#'   the highest value is on the top and the lowest value is on the bottom
#' @param available_aes A vector of character strings listing the aesthetics
#'   for which a colourbar can be drawn.
#' @param ... ignored.
#' @return A guide object
#' @export
#' @family guides
#' @seealso
#' The `r link_book("continuous legend section", "scales-colour#sec-guide-colourbar")`
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
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.key.width  = unit(0.5, "lines"),
#'   legend.key.height = unit(10, "lines")
#' )))
#'
#'
#' # no label
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.text = element_blank()
#' )))
#'
#' # no tick marks
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.ticks = element_blank()
#' )))
#'
#' # label position
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.text.position = "left"
#' )))
#'
#' # label theme
#' p1 + guides(fill = guide_colourbar(theme = theme(
#'   legend.text = element_text(colour = "blue", angle = 0)
#' )))
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
#'   )
#'
#' # guides can be controlled independently
#' p2 +
#'   scale_fill_continuous(guide = "colourbar") +
#'   scale_size(guide = "legend")
#' p2 + guides(fill = "colourbar", size = "legend")
#'
#' p2 +
#'   scale_fill_continuous(guide = guide_colourbar(theme = theme(
#'     legend.direction = "horizontal"
#'   ))) +
#'   scale_size(guide = guide_legend(theme = theme(
#'     legend.direction = "vertical"
#'   )))
guide_colourbar <- function(
  title = waiver(),
  theme = NULL,
  nbin = NULL,
  display = "raster",
  raster = deprecated(),
  alpha = NA,
  draw.ulim = TRUE,
  draw.llim = TRUE,
  position = NULL,
  direction = NULL,
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),
  ...
) {
  if (lifecycle::is_present(raster)) {
    deprecate_soft0("3.5.0", "guide_colourbar(raster)", "guide_colourbar(display)")
    check_bool(raster)
    display <- if (raster) "raster" else "rectangles"
  }
  display <- arg_match0(display, c("raster", "rectangles", "gradient"))
  nbin <- nbin %||% switch(display, gradient = 15, 300)

  theme <- deprecated_guide_args(theme, ...)
  if (!is.null(position)) {
    position <- arg_match0(position, c(.trbl, "inside"))
  }
  check_number_decimal(alpha, min = 0, max = 1, allow_na = TRUE)

  new_guide(
    title = title,
    theme = theme,
    nbin = nbin,
    display = display,
    alpha = alpha,
    draw_lim = c(isTRUE(draw.llim), isTRUE(draw.ulim)),
    position = position,
    direction = direction,
    reverse = reverse,
    order = order,
    available_aes = available_aes,
    name = "colourbar",
    super = GuideColourbar
  )
}

#' @export
#' @rdname guide_colourbar
guide_colorbar <- guide_colourbar

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideColourbar <- ggproto(
  "GuideColourbar", GuideLegend,

  params = list(
    # title
    title = waiver(),

    # theming
    theme = NULL,
    default_ticks = element_line(colour = "white", linewidth = 0.5 / .pt),
    default_frame = element_blank(),

    # bar
    nbin = 300,
    display = "raster",
    alpha = NA,

    draw_lim = c(TRUE, TRUE),

    # general
    direction = NULL,
    reverse = FALSE,
    order = 0,

    # parameter
    name = "colourbar",
    hash = character(),
    position = NULL
  ),

  available_aes = c("colour", "color", "fill"),

  hashables = exprs(title, key$.label, decor, name),

  elements = list(
    background     = "legend.background",
    margin         = "legend.margin",
    key            = "legend.key",
    key_height     = "legend.key.height",
    key_width      = "legend.key.width",
    text           = "legend.text",
    theme.title    = "legend.title",
    text_position  = "legend.text.position",
    title_position = "legend.title.position",
    axis_line      = "legend.axis.line",
    ticks          = "legend.ticks",
    ticks_length   = "legend.ticks.length",
    frame          = "legend.frame"
  ),

  extract_key = function(scale, aesthetic, ...) {
    if (scale$is_discrete()) {
      cli::cli_warn("{.fn guide_colourbar} needs continuous scales.")
      return(NULL)
    }
    key <- Guide$extract_key(scale, aesthetic, ...)
    if (NROW(key) == 0) {
      return(NULL)
    }
    key
  },

  extract_decor = function(scale, aesthetic, nbin = 300, reverse = FALSE, alpha = NA, ...) {

    limits <- scale$get_limits()
    bar <- seq(limits[1], limits[2], length.out = nbin)
    if (length(bar) == 0) {
      bar <- unique0(limits)
    }
    bar <- data_frame0(
      colour = alpha(scale$map(bar), alpha),
      value  = bar,
      .size  = length(bar)
    )
    if (reverse) {
      bar <- bar[nrow(bar):1, , drop = FALSE]
    }
    return(bar)
  },

  extract_params = function(scale, params,
                            title  = waiver(), ...) {
    params$title <- scale$make_title(params$title %|W|% scale$name %|W|% title)
    limits <- params$decor$value[c(1L, nrow(params$decor))]
    to <- switch(
      params$display,
      gradient = c(0, 1),
      c(0.5, params$nbin - 0.5) / params$nbin
    )
    params$key$.value <- rescale(params$key$.value, to = to, from = limits)
    params
  },

  merge = function(self, params, new_guide, new_params) {
    new_params$key$.label <- new_params$key$.value <- NULL
    params$key <- vec_cbind(params$key, new_params$key)
    return(list(guide = self, params = params))
  },

  get_layer_key = function(params, layers, data = NULL, theme = NULL) {
    params
  },

  setup_params = function(params) {
    params$direction <- arg_match0(
      params$direction,
      c("horizontal", "vertical"), arg_nm = "direction"
    )
    params
  },

  setup_elements = function(params, elements, theme) {
    # We set the defaults in `theme` so that the `params$theme` can still
    # overrule defaults given here
    if (params$direction == "horizontal") {
      theme$legend.key.width  <- theme$legend.key.width * 5
      valid_position <- c("bottom", "top")
    } else {
      theme$legend.key.height <- theme$legend.key.height * 5
      valid_position <- c("right", "left")
    }

    # Set defaults
    theme <- replace_null(
      theme,
      legend.text.position = valid_position[1],
      legend.ticks         = params$default_ticks,
      legend.frame         = params$default_frame
    )

    # Let the legend guide handle the rest
    elements <- GuideLegend$setup_elements(params, elements, theme)

    # Check text position
    if (!elements$text_position %in% valid_position) {
      cli::cli_abort(paste0(
        "When {.arg direction} is {.val {params$direction}}, ",
        "{.arg legend.text.position} must be one of ",
        "{.or {.val {valid_position}}}, not {.val {elements$text_position}}."
      ))
    }
    elements
  },

  build_labels = function(key, elements, params) {
    n_labels <- length(key$.label)
    if (n_labels < 1) {
      return(list(labels = zeroGrob()))
    }

    list(labels = flip_element_grob(
      elements$text,
      label = validate_labels(key$.label),
      x = unit(key$.value, "npc"),
      margin_x = FALSE,
      margin_y = TRUE,
      flip = params$direction == "vertical"
    ))
  },

  build_ticks = function(key, elements, params, position = params$position) {
    pos <- key$.value
    if (!params$draw_lim[1]) pos <- pos[-1]
    if (!params$draw_lim[2]) pos <- pos[-length(pos)]
    position <- switch(
      params$direction,
      "horizontal" = c("bottom", "top"),
      "vertical"   = c("right", "left")
    )
    ticks_length <- rep(elements$ticks_length, length.out = 2)

    grobTree(
      Guide$build_ticks(pos, elements, params, position[1], ticks_length[1]),
      Guide$build_ticks(pos, elements, params, position[2], ticks_length[2])
    )
  },

  build_decor = function(decor, grobs, elements, params) {
    if (params$display == "raster") {
      image <- switch(
        params$direction,
        "horizontal" = t(decor$colour),
        "vertical"   = rev(decor$colour)
      )
      grob <- rasterGrob(
        image  = image,
        width  = 1,
        height = 1,
        default.units = "npc",
        gp = gpar(col = NA),
        interpolate = TRUE
      )
    } else if (params$display == "rectangles") {
      if (params$direction == "horizontal") {
        width  <- 1 / nrow(decor)
        height <- 1
        x <- (seq_len(nrow(decor)) - 1) * width
        y <- 0
      } else {
        width  <- 1
        height <- 1 / nrow(decor)
        y <- (seq_len(nrow(decor)) - 1) * height
        x <- 0
      }
      grob <- rectGrob(
        x = x, y = y,
        vjust = 0, hjust = 0,
        width = width, height = height,
        default.units = "npc",
        gp = gg_par(col = NA, fill = decor$colour)
      )
    } else if (params$display == "gradient") {
      check_device("gradients", call = expr(guide_colourbar()))
      value <- if (isTRUE(params$reverse)) {
        rescale(decor$value, to = c(1, 0))
      } else {
        rescale(decor$value, to = c(0, 1))
      }
      position <- switch(
        params$direction,
        horizontal = list(y1 = unit(0.5, "npc"), y2 = unit(0.5, "npc")),
        vertical   = list(x1 = unit(0.5, "npc"), x2 = unit(0.5, "npc"))
      )
      gradient <- inject(linearGradient(decor$colour, value, !!!position))
      grob <- rectGrob(gp = gg_par(fill = gradient, col = NA))
    }

    frame <- element_grob(elements$frame, fill = NA)

    list(bar = grob, frame = frame, ticks = grobs$ticks)
  },

  measure_grobs = function(grobs, params, elements) {
    params$sizes <- list(
      widths  = elements$width_cm,
      heights = elements$height_cm
    )
    GuideLegend$measure_grobs(grobs, params, elements)
  }
)
