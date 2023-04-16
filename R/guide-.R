#' Guide constructor
#'
#' A constructor function for guides, which performs some standard compatability
#' checks between the guide and provided arguments.
#'
#' @param ... Named arguments that match the parameters of `super$params` or
#'   the theme elements in `super$elements`.
#' @param available_aes A vector of character strings listing the aesthetics
#'   for which the guide can be drawn.
#' @param super The super class to use for the constructed guide. Should be a
#'   Guide class object.
#'
#' @return A `Guide` ggproto object.
#' @keywords internal
#' @export
new_guide <- function(..., available_aes = "any", super) {

  pf <- parent.frame()
  super <- check_subclass(super, "Guide", env = pf)

  args <- list2(...)

  # Set parameters
  param_names <- names(super$params)
  params <- intersect(names(args), param_names)
  params <- defaults(args[params], super$params)

  # Set elements
  elems_names <- names(super$elements)
  elems  <- intersect(names(args), elems_names)
  elems  <- defaults(args[elems], super$elements)

  # Warn about extra arguments
  extra_args <- setdiff(names(args), union(param_names, elems_names))
  if (length(extra_args) > 0) {
    cli::cli_warn(paste0(
      "Ignoring unknown {cli::qty(extra_args)} argument{?s} to ",
      "{.fn {snake_class(super)}}: {.arg {extra_args}}."
    ))
  }

  # Stop when some required parameters are missing.
  # This should only happen with mis-constructed guides
  required_params <- names(Guide$params)
  missing_params  <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    cli::cli_abort(paste0(
      "The following parameter{?s} {?is/are} required for setting up a guide, ",
      "but {?is/are} missing: {.field {missing_params}}"
    ))
  }

  # Ensure 'order' is length 1 integer
  params$order <- vec_cast(params$order, 0L, x_arg = "order", call = pf)
  vec_assert(params$order, 0L, size = 1L, arg = "order", call = pf)

  ggproto(
    NULL, super,
    params   = params,
    elements = elems,
    available_aes = available_aes
  )
}

#' @section Guides:
#'
#' The `guide_*()` functions, such as `guide_legend()` return an object that
#' is responsible for displaying how objects in the plotting panel are related
#' to actual values.
#'
#' Each of the `Guide*` object is a [ggproto()] object, descended from the
#' top-level `Guide`, and each implements their own methods for drawing.
#'
#' To create a new type of Guide object, you typically will want to override
#' one or more of the following:
#'
#'  TODO: Fill this in properly
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Guide <- ggproto(
  "Guide",

  # `params` is a list of initial parameters that gets updated upon
  #  construction. After construction, parameters are manged by the
  #  `GuidesList` class.
  params = list(
    title     = waiver(),
    name      = character(),
    position  = waiver(),
    direction = NULL,
    order     = 0,
    hash      = character()
  ),

  # A list of theme elements that should be calculated
  elements = list(),

  # The aesthetics for which this guide is appropriate
  available_aes = character(),

  # The `hashables` are the parameters of the guide that are used to generate a
  # unique hash that determines whether other guides are compatible.
  hashables = exprs(title, name),

  # Training has the task of updating parameters based the scale.
  # There are 3 sub-tasks:
  # 1. Extract a key from the scale
  # 2. (Optionally) extract further decoration from the scale (e.g. the
  #    colour bar).
  # 3. Extract further parameters
  train = function(self, params = self$params, scale, aesthetic = NULL, ...) {
    params$aesthetic <- aesthetic %||% scale$aesthetics[1]
    params$key   <- inject(self$extract_key(scale, !!!params))
    if (is.null(params$key)) {
      return(NULL)
    }
    params$decor <- inject(self$extract_decor(scale, !!!params))
    self$extract_params(scale, params, self$hashables, ...)
  },

  # Setup parameters that are only available after training
  # TODO: Maybe we only need the hash on demand during merging?
  extract_params = function(scale, params, hashables, ...) {
    # Make hash
    params$hash <- hash(lapply(unname(hashables), eval_tidy, data = params))
    params
  },

  # Function for generating a `key` data.frame from the scale
  extract_key = function(scale, aesthetic, ...) {
    breaks <- scale$get_breaks()
    if (length(breaks) == 0) {
      return(NULL)
    }

    mapped <- scale$map(breaks)
    labels <- scale$get_labels(breaks)

    key <- data_frame(mapped, .name_repair = ~ aesthetic)
    key$.value <- breaks
    key$.label <- labels

    if (is.numeric(breaks)) {
      key[is.finite(breaks), , drop = FALSE]
    } else {
      key
    }
  },

  # Function for extracting decoration from the scale.
  # This is for `guide_colourbar` to extract the bar as well as the key,
  # and might be a good extension point.
  extract_decor = function(scale, aesthetic, ...) {
    return(invisible()) # By default, nothing else needs to be extracted
  },

  # Function for merging multiple guides.
  # Mostly applies to `guide_legend()` and `guide_binned()`.
  # Defaults to returning the *other* guide, because this parent class is
  # mostly a virtual class and children should implement their own merges.
  merge = function(self, params, new_guide, new_params) {
    return(list(guide = new_guide, params = new_params))
  },

  # Function for applying coord-transformation.
  # Mostly applied to position guides, such as `guide_axis()`.
  transform = function(self, params, coord, ...) {
    cli::cli_abort(c(
      "{.fn {snake_class(self)}} does not implement a {.fn transform} method.",
      "i" = "Did you mean to use {.fn guide_axis}?"
    ))
  },

  # Function for extracting information from the layers.
  # Mostly applies to `guide_legend()` and `guide_binned()`
  get_layer_key = function(params, layers) {
    return(params)
  },

  # Called at start of the `draw` method. Typically used to either overrule
  # user-specified parameters or populate extra parameters derived from
  # the guide's direction or position.
  setup_params = function(params) {
    params
  },

  # Converts the `elements` field to proper elements to be accepted by
  # `element_grob()`. String-interpolates aesthetic/position dependent elements.
  setup_elements = function(params, elements, theme) {
    is_char  <- vapply(elements, is.character, logical(1))
    elements[is_char] <- lapply(elements[is_char], calc_element, theme = theme)
    elements
  },

  # Called after `setup_elements` to overrule any element defaults descended
  # from the theme.
  override_elements = function(params, elements, theme) {
    elements
  },

  # Main drawing function that organises more specialised aspects of guide
  # drawing.
  draw = function(self, theme, params = self$params) {

    key <- params$key

    # Setup parameters and theme
    params <- self$setup_params(params)
    elems  <- self$setup_elements(params, self$elements, theme)
    elems  <- self$override_elements(params, elems, theme)

    # Allow early exit when key is empty
    if (prod(dim(key)) == 0) {
      out <- self$draw_early_exit(params, elems)
      return(out)
    }

    # Build grobs
    grobs <- list(
      title  = self$build_title(params$title, elems, params),
      labels = self$build_labels(key, elems, params),
      ticks  = self$build_ticks(key, elems, params)
    )
    grobs$decor <- self$build_decor(params$decor, grobs, elems, params)

    # Arrange and assemble grobs
    sizes  <- self$measure_grobs(grobs, params, elems)
    layout <- self$arrange_layout(key, sizes, params)
    self$assemble_drawing(grobs, layout, sizes, params, elems)
  },

  # Makes measurements of grobs that can be used in the layout or assembly
  # stages of guide drawing.
  measure_grobs = function(grobs, params, elements) {
    return(invisible())
  },

  # Takes care of where grobs should be added to the output gtable.
  arrange_layout = function(key, sizes, params) {
    return(invisible())
  },

  # Combines grobs into a single gtable.
  assemble_drawing = function(grobs, layout, sizes, params, elements) {
    zeroGrob()
  },

  # Renders the guide title
  build_title = function(label, elements, params) {
    ggname(
      "guide.title",
      element_grob(
        elements$title,
        label    = label,
        margin_x = TRUE,
        margin_y = TRUE
      )
    )
  },

  # Renders the guide labels
  # TODO: See if we can generalise label drawing for many guides
  build_labels = function(key, elements, params) {
    zeroGrob()
  },

  # Renders 'decor', which can have different meanings for different guides.
  # The other grobs are provided, as a colourbar might use the ticks for example
  build_decor = function(decor, grobs, elements, params) {
    zeroGrob()
  },

  # Renders tickmarks
  build_ticks = function(key, elements, params, position = params$position) {

    if (!is.list(key)) {
      breaks <- key
    } else {
      breaks <- key[[params$aes]]
    }
    n_breaks <- length(breaks)

    # Early exit if there are no breaks
    if (n_breaks < 1) {
      return(zeroGrob())
    }

    tick_len <- rep(elements$ticks_length %||% unit(0.2, "npc"),
                    length.out = n_breaks)

    # Resolve mark
    mark <- unit(rep(breaks, each = 2), "npc")

    # Resolve ticks
    pos <- unname(c(top = 1, bottom = 0, left = 0, right = 1)[position])
    dir <- -2 * pos + 1
    pos <- unit(rep(pos, 2 * n_breaks), "npc")
    dir <- rep(vec_interleave(0, dir), n_breaks) * tick_len
    tick <- pos + dir

    # Build grob
    flip_element_grob(
      elements$ticks,
      x = tick, y = mark,
      id.lengths = rep(2, n_breaks),
      flip = position %in% c("top", "bottom")
    )
  },

  draw_early_exit = function(self, params, elements) {
    zeroGrob()
  }
)

# Helper function that may facilitate flipping theme elements by
# swapping x/y related arguments to `element_grob()`
flip_element_grob = function(..., flip = FALSE) {
  if (!flip) {
    ans <- element_grob(...)
    return(ans)
  }
  args <- list(...)
  translate <- names(args) %in% names(flip_names)
  names(args)[translate] <- flip_names[names(args)[translate]]
  do.call(element_grob, args)
}

# The flippable arguments for `flip_element_grob()`.
flip_names = c(
  "x"        = "y",
  "y"        = "x",
  "width"    = "height",
  "height"   = "width",
  "hjust"    = "vjust",
  "vjust"    = "hjust",
  "margin_x" = "margin_y",
  "margin_y" = "margin_x"
)

# Shortcut for position argument matching
.trbl <- c("top", "right", "bottom", "left")

