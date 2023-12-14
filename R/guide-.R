#' @include theme-elements.R
NULL

#' Guide constructor
#'
#' A constructor function for guides, which performs some standard compatibility
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

  # Warn about extra arguments
  extra_args <- setdiff(names(args), param_names)
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

  # Validate theme settings
  if (!is.null(params$theme)) {
    check_object(params$theme, is.theme, what = "a {.cls theme} object")
    validate_theme(params$theme)
    params$direction <- params$direction %||% params$theme$legend.direction
  }

  # Ensure 'order' is length 1 integer
  params$order <- vec_cast(params$order, 0L, x_arg = "order", call = pf)
  vec_assert(params$order, 0L, size = 1L, arg = "order", call = pf)

  ggproto(
    NULL, super,
    params = params,
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
#' Properties:
#'
#' - `available_aes` A `character` vector with aesthetics that this guide
#'   supports. The value `"any"` indicates all non-position aesthetics.
#'
#' - `params` A named `list` of parameters that the guide needs to function.
#'   It has the following roles:
#'
#'   - `params` provides the defaults for a guide.
#'   - `names(params)` determines what are valid arguments to `new_guide()`.
#'   Some parameters are *required* to render the guide. These are: `title`,
#'   `name`, `position`, `direction`, `order` and `hash`.
#'   - During build stages, `params` holds information about the guide.
#'
#' - `elements` A named list of `character`s, giving the name of theme elements
#'   that should be retrieved automatically, for example `"legend.text"`.
#'
#' - `hashables` An `expression` that can be evaluated in the context of
#'   `params`. The hash of the evaluated expression determines the merge
#'   compatibility of guides, and is stored in `params$hash`.
#'
#' Methods:
#'
#' - `extract_key()` Returns a `data.frame` with (mapped) breaks and labels
#'   extracted from the scale, which will be stored in `params$key`.
#'
#' - `extract_decor()` Returns a `data.frame` containing other structured
#'   information extracted from the scale, which will be stored in
#'   `params$decor`. The `decor` has a guide-specific  meaning: it is the bar in
#'   `guide_colourbar()`, but specifies the `axis.line` in `guide_axis()`.
#'
#' - `extract_params()` Updates the `params` with other, unstructured
#'   information from the scale. An example of this is inheriting the guide's
#'   title from the `scale$name` field.
#'
#' - `transform()` Updates the `params$key` based on the coordinates. This
#'   applies to position guides, as it rescales the aesthetic to the \[0, 1\]
#'   range.
#'
#' - `merge()` Combines information from multiple guides with the same
#'   `params$hash`. This ensures that e.g. `guide_legend()` can display both
#'   `shape` and `colour` in the same guide.
#'
#' - `process_layers()` Extract information from layers. This acts mostly
#'   as a filter for which layers to include and these are then (typically)
#'   forwarded to `get_layer_key()`.
#'
#' - `get_layer_key()` This can be used to gather information about how legend
#'   keys should be displayed.
#'
#' - `setup_params()` Set up parameters at the beginning of drawing stages.
#'   It can be used to overrule user-supplied parameters or perform checks on
#'   the `params` property.
#'
#' - `override_elements()` Take populated theme elements derived from the
#'   `elements` property and allows overriding these theme settings.
#'
#' - `build_title()` Render the guide's title.
#'
#' - `build_labels()` Render the guide's labels.
#'
#' - `build_decor()` Render the `params$decor`, which is different for every
#'   guide.
#'
#' - `build_ticks()` Render tick marks.
#'
#' - `measure_grobs()` Measure dimensions of the graphical objects produced
#'   by the `build_*()` methods to be used in the layout or assembly.
#'
#' - `arrange_layout()` Set up a layout for how graphical objects produced by
#'   the `build_*()` methods should be arranged.
#'
#' - `assemble_drawing()` Take the graphical objects produced by the `build_*()`
#'   methods, the measurements from `measure_grobs()` and layout from
#'   `arrange_layout()` to finalise the guide.
#'
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
    theme     = NULL,
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
    params <- self$extract_params(scale, params, ...)
    # Make hash
    # TODO: Maybe we only need the hash on demand during merging?
    params$hash <- hash(lapply(unname(self$hashables), eval_tidy, data = params))
    params
  },

  # Setup parameters that are only available after training
  extract_params = function(scale, params, ...) {
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
    # {vctrs} doesn't play nice with expressions, convert to list.
    # see also https://github.com/r-lib/vctrs/issues/559
    if (is.expression(labels)) {
      labels <- as.list(labels)
    }

    key <- data_frame(mapped, .name_repair = ~ aesthetic)
    key$.value <- breaks
    key$.label <- labels

    if (is.numeric(breaks)) {
      range <- scale$continuous_range %||% scale$get_limits()
      key <- vec_slice(key, is.finite(oob_censor_any(breaks, range)))
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
  process_layers = function(self, params, layers, data = NULL) {
    self$get_layer_key(params, layers, data)
  },

  get_layer_key = function(params, layers, data = NULL) {
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
    theme <- add_theme(theme, params$theme)
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
  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    key <- params$key

    # Setup parameters and theme
    params <- replace_null(params, position = position, direction = direction)
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
  build_ticks = function(key, elements, params, position = params$position,
                         length = elements$ticks_length) {
    if (!inherits(elements, "element")) {
      elements <- elements$ticks
    }
    if (!inherits(elements, "element_line")) {
      return(zeroGrob())
    }

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

    tick_len <- rep(length %||% unit(0.2, "npc"), length.out = n_breaks)

    # Resolve mark
    mark <- unit(rep(breaks, each = 2), "npc")

    # Resolve ticks
    pos <- unname(c(top = 1, bottom = 0, left = 0, right = 1)[position])
    dir <- -2 * pos + 1
    pos <- unit(rep(pos, 2 * n_breaks), "npc")
    dir <- rep(vec_interleave(dir, 0), n_breaks) * rep(tick_len, each = 2)
    tick <- pos + dir

    # Build grob
    flip_element_grob(
      elements,
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

opposite_position <- function(position) {
  switch(
    position,
    top    = "bottom",
    bottom = "top",
    left   = "right",
    right  = "left"
  )
}

# Ensure that labels aren't a list of expressions, but proper expressions
validate_labels <- function(labels) {
  if (!is.list(labels)) {
    return(labels)
  }
  if (any(vapply(labels, is.language, logical(1)))) {
    do.call(expression, labels)
  } else {
    unlist(labels)
  }
}
