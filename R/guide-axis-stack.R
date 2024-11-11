#' @include guide-axis.R
NULL

#' Stacked axis guides
#'
#' This guide can stack other position guides that represent position scales,
#' like those created with [scale_(x|y)_continuous()][scale_x_continuous()] and
#' [scale_(x|y)_discrete()][scale_x_discrete()].
#'
#' @inheritParams guide_axis
#' @param first A position guide given as one of the following:
#' * A string, for example `"axis"`.
#' * A call to a guide function, for example `guide_axis()`.
#' @param ... Additional guides to stack given in the same manner as `first`.
#' @param spacing A [unit()] objects that determines how far separate guides are
#'   spaced apart.
#'
#' @details
#' The `first` guide will be placed closest to the panel and any subsequent
#' guides provided through `...` will follow in the given order.
#'
#' @export
#'
#' @examples
#' #' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme(axis.line = element_line())
#'
#' # A normal axis first, then a capped axis
#' p + guides(x = guide_axis_stack("axis", guide_axis(cap = "both")))
guide_axis_stack <- function(first = "axis", ..., title = waiver(), theme = NULL,
                             spacing = NULL, order = 0, position = waiver()) {

  check_object(spacing, is.unit, "{.cls unit}", allow_null = TRUE)

  # Validate guides
  axes <- list2(first, ...)
  axes <- lapply(axes, validate_guide)

  # Check available aesthetics
  available <- lapply(axes, `[[`, name = "available_aes")
  available <- vapply(available, function(x) all(c("x", "y") %in% x), logical(1))
  if (!any(available)) {
    cli::cli_abort(paste0(
      "{.fn guide_axis_stack} can only use guides that handle {.field x} and ",
      "{.field y} aesthetics."
    ))
  }

  # Remove guides that don't support x/y aesthetics
  if (!all(available)) {
    remove  <- which(!available)
    removed <- vapply(axes[remove], snake_class, character(1))
    axes[remove] <- NULL
    cli::cli_warn(c(paste0(
      "{.fn guide_axis_stack} cannot use the following guide{?s}: ",
      "{.and {.fn {removed}}}."
    ), i = "Guides need to handle {.field x} and {.field y} aesthetics."))
  }

  params <- lapply(axes, `[[`, name = "params")

  new_guide(
    title = title,
    theme = theme,
    guides = axes,
    guide_params = params,
    spacing = spacing,
    available_aes = c("x", "y", "theta", "r"),
    order = order,
    position = position,
    name = "stacked_axis",
    super = GuideAxisStack
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideAxisStack <- ggproto(
  "GuideAxisStack", GuideAxis,

  params = list(
    # List of guides to track the guide objects
    guides    = list(),
    # List of parameters to each guide
    guide_params = list(),
    spacing   = NULL,
    # Standard guide stuff
    name      = "stacked_axis",
    title     = waiver(),
    theme     = NULL,
    angle     = waiver(),
    hash      = character(),
    position  = waiver(),
    direction = NULL,
    order     = 0
  ),

  available_aes = c("x", "y", "theta", "r"),

  # Doesn't depend on keys, but on member axis' class
  hashables = exprs(title, lapply(guides, snake_class), name),

  # Sets position, loops through guides to train
  train = function(self, params = self$params, scale, aesthetic = NULL, ...) {
    position <- arg_match0(
      params$position, c(.trbl, "theta", "theta.sec"),
      arg_nm = "position"
    )
    for (i in seq_along(params$guides)) {
      params$guide_params[[i]]$position <- position
      params$guide_params[[i]]$angle <- params$guide_params[[i]]$angle %|W|% params$angle
      params$guide_params[[i]] <- params$guides[[i]]$train(
        params = params$guide_params[[i]],
        scale = scale, aesthetic = aesthetic,
        ...
      )
    }
    params
  },

  # Just loops through guides
  transform = function(self, params, coord, panel_params) {
    for (i in seq_along(params$guides)) {
      params$guide_params[[i]] <- params$guides[[i]]$transform(
        params = params$guide_params[[i]],
        coord = coord, panel_params = panel_params
      )
    }
    params
  },

  # Just loops through guides
  get_layer_key = function(params, layers, ...) {
    for (i in seq_along(params$guides)) {
      params$guide_params[[i]] <- params$guides[[i]]$get_layer_key(
        params = params$guide_params[[i]],
        layers = layers
      )
    }
    params
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    theme <- add_theme(theme, params$theme)

    position  <- params$position  %||% position
    direction <- params$direction %||% direction

    # If we are instructed to not draw labels at interior panels, just render
    # the first axis
    draw_label  <- params$draw_label %||% TRUE
    guide_index <- if (draw_label) seq_along(params$guides) else 1L

    if (position %in% c("theta", "theta.sec")) {
      # If we are a theta guide, we need to keep track how much space in the
      # radial direction a guide occupies, and add that as an offset to the
      # next guide.
      offset  <- unit(0, "cm")
      spacing <- params$spacing %||% unit(2.25, "pt")
      grobs   <- list()

      for (i in guide_index) {
        # Add offset to params
        pars <- params$guide_params[[i]]
        pars$stack_offset <- offset
        # Draw guide
        grobs[[i]] <- params$guides[[i]]$draw(
          theme, position = position, direction = direction,
          params = pars
        )
        # Increment offset
        if (!is.null(grobs[[i]]$offset)) {
          offset <- offset + spacing + grobs[[i]]$offset
          offset <- convertUnit(offset, "cm")
        }
      }
      grob <- inject(grobTree(!!!grobs))
      return(grob)
    }

    # Loop through every guide's draw method
    grobs <- list()
    for (i in guide_index) {
      pars <- params$guide_params[[i]]
      pars$draw_label <- draw_label
      grobs[[i]] <- params$guides[[i]]$draw(
        theme, position = position, direction = direction,
        params = pars
      )
    }

    # Remove empty grobs
    grobs <- grobs[!vapply(grobs, is.zero, logical(1))]
    if (length(grobs) == 0) {
      return(zeroGrob())
    }
    along <- seq_along(grobs)

    # Get sizes
    widths  <- inject(unit.c(!!!lapply(grobs, grobWidth)))
    heights <- inject(unit.c(!!!lapply(grobs, grobHeight)))

    # Set spacing
    if (is.null(params$spacing)) {
      aes <- if (position %in% c("top", "bottom")) "x" else "y"
      spacing <- paste("axis.ticks.length", aes, position, sep = ".")
      spacing <- calc_element(spacing, theme)
    } else {
      spacing <- params$spacing
    }

    # Reorder grobs/sizes if necessary
    if (position %in% c("top", "left")) {
      along   <- rev(along)
      widths  <- rev(widths)
      heights <- rev(heights)
    }

    # Place guides in a gtable, apply spacing
    if (position %in% c("bottom", "top")) {
      gt <- gtable(widths = unit(1, "npc"), heights = heights)
      gt <- gtable_add_grob(gt, grobs, t = along, l = 1, name = "axis", clip = "off")
      gt <- gtable_add_row_space(gt, height = spacing)
      vp <- exec(
        viewport,
        y    = unit(as.numeric(position == "bottom"), "npc"),
        height = grobHeight(gt),
        just = opposite_position(position)
      )
    } else {
      gt <- gtable(widths = widths, heights = unit(1, "npc"))
      gt <- gtable_add_grob(gt, grobs, t = 1, l = along, name = "axis", clip = "off")
      gt <- gtable_add_col_space(gt, width = spacing)
      vp <- exec(
        viewport,
        x     = unit(as.numeric(position == "left"), "npc"),
        width = grobWidth(gt),
        just  = opposite_position(position)
      )
    }

    absoluteGrob(
      grob   = gList(gt),
      width  = gtable_width(gt),
      height = gtable_height(gt),
      vp = vp
    )
  }
)

