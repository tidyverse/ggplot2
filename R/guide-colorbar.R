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
#' @param barwidth A numeric or a [grid::unit()] object specifying
#'   the width of the colourbar. Default value is `legend.key.width` or
#'   `legend.key.size` in [theme()] or theme.
#' @param barheight A numeric or a [grid::unit()] object specifying
#'   the height of the colourbar. Default value is `legend.key.height` or
#'   `legend.key.size` in [theme()] or theme.
#' @param frame A theme object for rendering a frame drawn around the bar.
#'   Usually, the object of `element_rect()` is expected. If `element_blank()`
#'   (default), no frame is drawn.
#' @param frame.colour A string specifying the colour of the frame
#'   drawn around the bar. For backward compatibility, if this argument is
#'   not `NULL`, the `frame` argument will be set to `element_rect()`.
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
#' @param ticks A theme object for rendering tick marks at the colourbar.
#'   Usually, the object of `element_line()` is expected (default). If
#'   `element_blank()`, no tick marks are drawn. For backward compatibility,
#'   can also be a logical which translates `TRUE` to `element_line()` and
#'   `FALSE` to `element_blank()`.
#' @param ticks.colour A string specifying the colour of the tick marks.
#' @param ticks.linewidth A numeric specifying the width of the tick marks in
#'   millimetres.
#' @param ticks.length A numeric or a [grid::unit()] object specifying the
#'   length of tick marks at the colourbar.
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
  frame = element_blank(),
  frame.colour = NULL,
  frame.linewidth = NULL,
  frame.linetype = NULL,

  # ticks
  ticks = element_line(),
  ticks.colour = NULL,
  ticks.linewidth = NULL,
  ticks.length = unit(0.2, "npc"),
  draw.ulim = TRUE,
  draw.llim = TRUE,

  # general
  direction = NULL,
  default.unit = "line",
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),
  ...
) {
  if (!(is.null(barwidth) || is.unit(barwidth))) {
    barwidth <- unit(barwidth, default.unit)
  }
  if (!(is.null(barheight) || is.unit(barheight))) {
    barheight <- unit(barheight, default.unit)
  }
  if (!is.unit(ticks.length)) {
    ticks.length <- unit(ticks.length, default.unit)
  }

  if (!is.null(title.position)) {
    title.position <- arg_match0(title.position, .trbl)
  }
  if (!is.null(direction)) {
    direction <- arg_match0(direction, c("horizontal", "vertical"))
  }
  if (!is.null(label.position)) {
    label.position <- arg_match0(label.position, .trbl)
  }

  if (!is.null(frame.colour) && !inherits(frame, "element_rect")) {
    # For backward compatibility, frame should not be element_blank when
    # colour is not NULL
    cli::cli_inform(c(paste0(
      "If {.arg frame.colour} is set, {.arg frame} should not be ",
      "{.cls {class(frame)[[1]]}}."
    ), "i" = "{.arg frame} has been converted to {.cls element_rect}."))
    frame <- element_rect()
  }
  if (inherits(frame, "element_rect")) {
    frame$colour    <- frame.colour    %||% frame$colour
    frame$linewidth <- frame.linewidth %||% frame$linewidth %||% (0.5 / .pt)
    frame$linetype  <- frame.linetype  %||% frame$linetype  %||% 1
  } else {
    frame <- element_blank()
  }

  if (is.logical(ticks)) {
    # Also for backward compatibility. `ticks = FALSE` used to mean: don't draw
    # the ticks
    ticks <- if (ticks) element_line() else element_blank()
  }
  if (inherits(ticks, "element_line")) {
    ticks$colour    <- ticks.colour    %||% ticks$colour    %||% "white"
    ticks$linewidth <- ticks.linewidth %||% ticks$linewidth %||% (0.5 / .pt)
  }

  # Trick to re-use this constructor in `guide_coloursteps()`.
  args  <- list2(...)
  super <- args$super %||% GuideColourbar
  args$super <- NULL

  new_guide(
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
    keywidth = barwidth,
    keyheight = barheight,
    nbin = nbin,
    raster = raster,

    # frame
    frame = frame,

    # ticks
    ticks = ticks,
    ticks_length = ticks.length,
    draw_lim = c(isTRUE(draw.llim), isTRUE(draw.ulim)),

    # general
    direction = direction,
    reverse = reverse,
    order = order,

    # parameter
    available_aes = available_aes,
    name = "colourbar",
    !!!args,
    super = super
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
    keywidth  = NULL,
    keyheight = NULL,
    nbin = 300,
    raster = TRUE,

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
    frame       = "rect",
    ticks       = "line",
    ticks_length = unit(0.2, "npc"),
    background  = "legend.background",
    margin      = "legend.margin",
    spacing     = "legend.spacing",
    spacing.x   = "legend.spacing.x",
    spacing.y   = "legend.spacing.y",
    key         = "legend.key",
    key.height  = "legend.key.height",
    key.width   = "legend.key.width",
    text        = "legend.text",
    text.align  = "legend.text.align",
    theme.title = "legend.title",
    title.align = "legend.title.align"
  ),

  extract_decor = function(scale, aesthetic, nbin = 300, reverse = FALSE, ...) {

    limits <- scale$get_limits()
    bar <- seq(limits[1], limits[2], length.out = nbin)
    if (length(bar) == 0) {
      bar <- unique0(limits)
    }
    bar <- data_frame0(
      colour = scale$map(bar),
      value  = bar,
      .size  = length(bar)
    )
    if (reverse) {
      bar <- bar[nrow(bar):1, , drop = FALSE]
    }
    return(bar)
  },

  extract_params = function(scale, params, hashables,
                            title  = waiver(), direction = "vertical", ...) {
    params$title <- scale$make_title(
      params$title %|W|% scale$name %|W|% title
    )
    params$direction <- arg_match0(
      params$direction %||% direction,
      c("horizontal", "vertical"), arg_nm = "direction"
    )
    valid_label_pos <- switch(
      params$direction,
      "horizontal" = c("bottom", "top"),
      "vertical"   = c("right", "left")
    )
    params$label.position <- params$label.position %||% valid_label_pos[1]
    if (!params$label.position %in% valid_label_pos) {
      cli::cli_abort(paste0(
        "When {.arg direction} is {.val {params$direction}}, ",
        "{.arg label.position} must be one of {.or {.val {valid_label_pos}}}, ",
        "not {.val {params$label.position}}."
      ))
    }

    limits <- c(params$decor$value[1], params$decor$value[nrow(params$decor)])
    params$key$.value <- rescale(
      params$key$.value,
      c(0.5, params$nbin - 0.5) / params$nbin,
      limits
    )
    Guide$extract_params(scale, params, hashables)
  },

  merge = function(self, params, new_guide, new_params) {
    return(list(guide = self, params = params))
  },

  get_layer_key = function(params, layers) {

    guide_layers <- lapply(layers, function(layer) {

      matched_aes <- matched_aes(layer, params)

      # Check if this layer should be included
      if (include_layer_in_guide(layer, matched_aes)) {
        layer
      } else {
        NULL
      }
    })

    if (length(compact(guide_layers)) == 0) {
      return(NULL)
    }
    return(params)
  },

  setup_params = function(params) {
    params$title.position <- arg_match0(
      params$title.position %||%
        switch(params$direction, vertical = "top", horizontal = "left"),
      .trbl, arg_nm = "title.position"
    )
    params$rejust_labels <- FALSE
    params
  },

  override_elements = function(params, elements, theme) {
    # These key sizes are the defaults, the GuideLegend method may overrule this
    if (params$direction == "horizontal") {
      elements$key.width <- elements$key.width * 5
    } else {
      elements$key.height <- elements$key.height * 5
    }
    elements$ticks <- combine_elements(elements$ticks, theme$line)
    elements$frame <- combine_elements(elements$frame, theme$rect)
    GuideLegend$override_elements(params, elements, theme)
  },

  build_labels = function(key, elements, params) {
    just <- if (params$direction == "horizontal") {
      elements$text$vjust
    } else {
      elements$text$hjust
    }

    list(labels = flip_element_grob(
      elements$text,
      label = key$.label,
      x = unit(key$.value, "npc"),
      y = rep(just, nrow(key)),
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
    elements$ticks_length <- rep(elements$ticks_length, length.out = 2)
    elem1 <- elem2 <- elements
    elem1$ticks_length <- elements$ticks_length[2]
    elem2$ticks_length <- elements$ticks_length[1]

    grobTree(
      Guide$build_ticks(pos, elem1, params, position[1]),
      Guide$build_ticks(pos, elem2, params, position[2])
    )
  },

  build_decor = function(decor, grobs, elements, params) {

    if (params$raster) {
      image <- switch(
        params$direction,
        "horizontal" = t(decor$colour),
        "vertical"   = rev(decor$colour)
      )
      grob <- rasterGrob(
        image  = image,
        width  = elements$key.width,
        height = elements$key.height,
        default.units = "cm",
        gp = gpar(col = NA),
        interpolate = TRUE
      )
    } else{
      if (params$direction == "horizontal") {
        width  <- elements$key.width / nrow(decor)
        height <- elements$key.height
        x <- (seq(nrow(decor)) - 1) * width
        y <- 0
      } else {
        width  <- elements$key.width
        height <- elements$key.height / nrow(decor)
        y <- (seq(nrow(decor)) - 1) * height
        x <- 0
      }
      grob <- rectGrob(
        x = x, y = y,
        vjust = 0, hjust = 0,
        width = width, height = height,
        default.units = "cm",
        gp = gpar(col = NA, fill = decor$colour)
      )
    }

    frame <- element_grob(elements$frame, fill = NA)

    list(bar = grob, frame = frame, ticks = grobs$ticks)
  },

  measure_grobs = function(grobs, params, elements) {
    params$sizes <- list(
      widths  = elements$key.width,
      heights = elements$key.height
    )
    GuideLegend$measure_grobs(grobs, params, elements)
  }
)
