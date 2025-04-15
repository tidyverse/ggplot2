#' @include legend-draw.R
#' @include utilities-checks.R
NULL

#' Geoms
#'
#' @description
#' All `geom_*()` functions (like `geom_point()`) return a layer that
#' contains a `Geom*` object (like `GeomPoint`). The `Geom*`
#' object is responsible for rendering the data in the plot.
#'
#' @details
#' Each of the `Geom*` objects is a [ggproto()] object, descended
#' from the top-level `Geom`, and each implements various methods and
#' fields. The object and its parameters are chaperoned by the [Layer] class.
#'
#' Compared to `Stat` and `Position`, `Geom` is a little
#' different because the execution of the setup and compute functions is
#' split up. `setup_data` runs before position adjustments, and
#' `draw_layer()` is not run until render time, much later.
#'
#' When creating a new Geom class, you may want to consider override one or
#' more of the following:
#' * The `required_aes` and `default_aes` fields.
#' * The `setup_data()` and `setup_params()` functions.
#' * Either the `draw_panel()` or `draw_group()` function.
#' * The `draw_key` field.
#'
#' @section Conventions:
#'
#' The object name that a new class is assigned to is typically the same as
#' the class name. Geom class names are in UpperCamelCase and start with the
#' `Geom*` prefix, like `GeomNew`.
#'
#' A constructor function is usually paired with a Geom class. The constructor
#' wraps a call to `layer()`, where e.g. `layer(geom = GeomNew)`. The constructor
#' function name is formatted by taking the Geom class name and formatting it
#' with snake_case, so that `GeomNew` becomes `geom_new()`.
#'
#' @export
#' @format NULL
#' @usage NULL
#' @seealso The `r link_book("new geoms section", "extensions#sec-new-geoms")`
#' @seealso Run `vignette("extending-ggplot2")`, in particular the "Creating a
#' new geom" section.
#' @examples
#' # Extending the class
#' GeomSimplePoint <- ggproto(
#'   "GeomSimplePoint", Geom,
#'   # Fields
#'   required_aes = c("x", "y"),
#'   draw_key     = draw_key_point,
#'   # Methods
#'   draw_panel = function(data, panel_params, coord) {
#'     data <- coord$transform(data, panel_params)
#'     grid::pointsGrob(data$x, data$y)
#'   }
#' )
#'
#' # Building a constructor
#' geom_simple_point <- function(mapping = NULL, data = NULL, stat = "identity",
#'                               position = "identity", ..., na.rm = FALSE,
#'                               show.legend = NA, inherit.aes = TRUE) {
#'   layer(
#'     mapping = mapping, data = data,
#'     geom = GeomSimplePoint, stat = stat, position = position,
#'     show.legend = show.legend, inherit.aes = inherit.aes,
#'     params = list(na.rm = na.rm, ...)
#'   )
#' }
#'
#' # Use new geom in plot
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_simple_point()
Geom <- ggproto(
  "Geom",

  # Fields ------------------------------------------------------------------

  #' @field required_aes A character vector naming aesthetics that are necessary
  #' to render the geom.
  required_aes = character(),

  #' @field non_missing_aes A character vector naming aesthetics that will cause
  #' removal if they have missing values.
  non_missing_aes = character(),

  #' @field optional_aes A character vector naming aesthetics that will be
  #' accepted by `layer()`, but are not required or described in the
  #' `default_aes` field.
  optional_aes = character(),

  #' @field default_aes A [mapping][aes()] of default values for aesthetics.
  #' Aesthetics can be set to `NULL` to be included as optional aesthetic.
  default_aes = aes(),

  #' @field rename_size
  #' A scalar boolean: whether to rename `size` aesthetics to `linewidth`.
  rename_size = FALSE,

  #' @field extra_params A character vector of parameter names in addition to
  #' those imputed from the `draw_panel()` or `draw_groups()` methods. This
  #' field can be set to include parameters for `setup_data()` or `handle_na()`
  #' methods. By default, this only contains `"na.rm"`.
  extra_params = c("na.rm"),

  #' @field draw_key A function generating a single legend glyph for the geom.
  #' Typically one of the functions prefixed by [`draw_key_`][draw_key].
  draw_key = draw_key_point,

  # Methods -----------------------------------------------------------------

  ## compute_geom_1 ---------------------------------------------------------

  #' @field setup_params
  #' **Description**
  #'
  #' A function method for modifying or checking the parameters based on the
  #' data. The default method returns the parameters unaltered.
  #'
  #' **Usage**
  #' ```r
  #' Geom$setup_params(data, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list of current parameters.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of parameters
  setup_params = function(data, params) {
    params
  },

  #' @field setup_data
  #' **Description**
  #'
  #' A function method for modifying or checking the data prior to adding
  #' defaults. The default method returns data unaltered.
  #'
  #' **Usage**
  #' ```r
  #' Geom$setup_data(data, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  setup_data = function(data, params) {
    data
  },

  ## compute_geom_2 ----------------------------------------------------------

  #' @field use_defaults
  #' **Description**
  #'
  #' A function method for completing the layer data by filling in default
  #' aesthetics that are not present. It is not recommended to use as an
  #' extension point.
  #'
  #' It takes on these tasks:
  #' * Evaluation of default aesthetics from the `default_aes` field.
  #' * Handling the [`after_scale()`]/`stage(after_scale)` stage of delayed
  #'   evaluation.
  #' * Fill in fixed, unmapped aesthetics passed as parameters.
  #'
  #' **Usage**
  #' ```r
  #' Geom$use_defaults(data, params, modifiers, default_aes, theme, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame of the layer's data, coming from the
  #'   `setup_data()` method. Can be `NULL`, in which case resolved defaults
  #'   should be returned.}
  #'   \item{`params`}{A list of fixed aesthetic parameters}
  #'   \item{`modifiers`}{A [mapping][aes()] with delayed evaluations.}
  #'   \item{`default_aes`}{A [mapping][aes()] with default aesthetics.}
  #'   \item{`theme`}{A [completed theme][complete_theme()].}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with completed layer data.
  use_defaults = function(self, data, params = list(), modifiers = aes(),
                          default_aes = NULL, theme = NULL, ...) {
    default_aes <- default_aes %||% self$default_aes

    # Inherit size as linewidth if no linewidth aesthetic and param exist
    if (self$rename_size && is.null(data$linewidth) && is.null(params$linewidth)) {
      data$linewidth <- data$size
      params$linewidth <- params$size
    }
    # Take care of subclasses setting the wrong default when inheriting from
    # a geom with rename_size = TRUE
    if (self$rename_size && is.null(default_aes$linewidth)) {
      deprecate_warn0("3.4.0", I("Using the `size` aesthetic in this geom"), I("`linewidth` in the `default_aes` field and elsewhere"))
      default_aes$linewidth <- default_aes$size
    }

    # Fill in missing aesthetics with their defaults
    missing_aes <- setdiff(names(default_aes), names(data))
    default_aes <- default_aes[missing_aes]
    themed_defaults <- eval_from_theme(default_aes, theme, class(self))
    default_aes[names(themed_defaults)] <- themed_defaults

    # Mark staged/scaled defaults as modifier (#6135)
    delayed <- is_scaled_aes(default_aes) | is_staged_aes(default_aes)
    if (any(delayed)) {
      modifiers <- defaults(modifiers, default_aes[delayed])
      default_aes <- default_aes[!delayed]
    }

    missing_eval <- lapply(default_aes, eval_tidy)
    # Needed for geoms with defaults set to NULL (e.g. GeomSf)
    missing_eval <- compact(missing_eval)

    if (empty(data)) {
      data <- as_gg_data_frame(missing_eval)
    } else {
      data[names(missing_eval)] <- missing_eval
    }

    themed <- is_themed_aes(modifiers)
    if (any(themed)) {
      themed <- eval_from_theme(modifiers[themed], theme)
      modifiers <- modifiers[setdiff(names(modifiers), names(themed))]
      data[names(themed)] <- themed
    }

    # If any after_scale mappings are detected they will be resolved here
    # This order means that they will have access to all default aesthetics
    if (length(modifiers) != 0) {
      modified_aes <- try_fetch(
        eval_aesthetics(
          substitute_aes(modifiers), data,
          mask = list(stage = stage_scaled)
        ),
        error = function(cnd) {
          cli::cli_warn("Unable to apply staged modifications.", parent = cnd)
          data_frame0()
        }
      )

      # Check that all output are valid data
      check_nondata_cols(
        modified_aes, modifiers,
        problem = "Aesthetic modifiers returned invalid values.",
        hint    = "Did you map the modifier in the wrong layer?"
      )

      modified_aes <- cleanup_mismatched_data(modified_aes, nrow(data), "after_scale")
      data[names(modified_aes)] <- modified_aes
    }

    # Override mappings with params
    aes_params <- intersect(self$aesthetics(), names(params))
    new_params <- params[aes_params]
    check_aesthetics(new_params, nrow(data))
    data[aes_params] <- new_params

    # Restore any AsIs classes (#5656)
    is_asis <- which(vapply(new_params, inherits, what = "AsIs", logical(1)))
    for (i in aes_params[is_asis]) {
      data[[i]] <- I(data[[i]])
    }
    data
  },

  ## draw_geom ---------------------------------------------------------------

  #' @field handle_na
  #' **Description**
  #'
  #' A function method to handle missing values. The default method will
  #' remove rows that have missing values for the aesthetics listed in the
  #' `required_aes` and `non_missing_aes` fields. It is not recommended to
  #' use this method as an extension point.
  #'
  #' **Usage**
  #' ```r
  #' Geom$handle_na(data, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data coming from the
  #'   `update_defaults()` method.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  handle_na = function(self, data, params) {
    remove_missing(data, params$na.rm,
      c(self$required_aes, self$non_missing_aes),
      snake_class(self)
    )
  },

  #' @field draw_layer
  #' **Description**
  #'
  #' A function method orchestrating the drawing of the entire layer. The
  #' default method splits the data and passes on drawing tasks to the
  #' panel-level `draw_panel()` method. It is not recommended to use this method
  #' as an extension point.
  #'
  #' **Usage**
  #' ```r
  #' Geom$draw_layer(data, params, layout, coord)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list of parameters}
  #'   \item{`layout`}{A completed `<Layout>` ggproto object.}
  #'   \item{`coord`}{A `<Coord>` ggproto object.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of grobs, one per panel
  draw_layer = function(self, data, params, layout, coord) {
    if (empty(data)) {
      n <- if (is.factor(data$PANEL)) nlevels(data$PANEL) else 1L
      return(rep(list(zeroGrob()), n))
    }

    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]

    if (nlevels(as.factor(data$PANEL)) > 1L) {
      data_panels <- split(data, data$PANEL)
    } else {
      data_panels <- list(data)
    }
    lapply(data_panels, function(data) {
      if (empty(data)) return(zeroGrob())

      panel_params <- layout$panel_params[[data$PANEL[1]]]
      inject(self$draw_panel(data, panel_params, coord, !!!params))
    })
  },

  #' @field draw_panel,draw_group
  #' **Description**
  #'
  #' A function method orchestrating the drawing of the layer for a single
  #' panel or group. The default `draw_panel()` method splits the data into groups,
  #' passes on the drawing tasks to the group-level `draw_group()` method and
  #' finally assembles these into a single grob. The default `draw_group` method
  #' is not implemented.
  #'
  #' **Usage**
  #' ```r
  #' Geom$draw_panel(data, panel_params, coord, ...)
  #' Geom$draw_group(data, panel_params, coord, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`panel_params`}{A list of per-panel parameters constructed by
  #'   `Coord$setup_panel_params()`. This should be considered an opaque data
  #'   structure that is just passed along when calling coord methods.}
  #'   \item{`coord`}{A `<Coord>` ggproto object. To correctly scale the
  #'   position data, one should always call
  #'   `coord$transform(data, panel_params)`. When working with non-linear
  #'   coordinate systems, data should be converted to fit a primitive geom
  #'   (e.g. point, path or polygon) and passed on to the corresponding draw
  #'   method for [munching][coord_munch()].}
  #'   \item{`...`}{Reserved for extensions. By default, this is passed on to
  #'   the `draw_group()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' A single grob or [`zeroGrob()`] when there is nothing to draw. For
  #' `draw_panel()` this can be a [gTree][grid::grob] holding individual grobs
  #' from the `draw_group()` method.
  draw_panel = function(self, data, panel_params, coord, ...) {
    groups <- split(data, factor(data$group))
    grobs <- lapply(groups, function(group) {
      self$draw_group(group, panel_params, coord, ...)
    })

    ggname(snake_class(self), gTree(
      children = inject(gList(!!!grobs))
    ))
  },

  draw_group = function(self, data, panel_params, coord) {
    cli::cli_abort("{.fn {snake_class(self)}}, has not implemented a {.fn draw_group} method")
  },

  ## Utilities ---------------------------------------------------------------

  #' @field parameters
  #' **Description**
  #'
  #' A function method for listing out all acceptable parameters for this geom.
  #'
  #' **Usage**
  #' ```r
  #' Geom$parameters(extra)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`extra`}{A boolean: whether to include the `extra_params` field.}
  #' }
  #'
  #' **Value**
  #'
  #' A character vector of parameter names.
  parameters = function(self, extra = FALSE) {
    # Look first in draw_panel. If it contains ... then look in draw groups
    panel_args <- names(ggproto_formals(self$draw_panel))
    group_args <- names(ggproto_formals(self$draw_group))
    args <- if ("..." %in% panel_args) group_args else panel_args

    # Remove arguments of defaults
    args <- setdiff(args, names(ggproto_formals(Geom$draw_group)))

    if (extra) {
      args <- union(args, self$extra_params)
    }
    args
  },

  #' @field aesthetics
  #' **Description**
  #'
  #' A function method for listing out all acceptable aesthetics for this geom.
  #'
  #' **Usage**
  #' ```r
  #' Geom$aesthetics()
  #' ```
  #' **Value**
  #'
  #' A character vector of aesthetic names.
  aesthetics = function(self) {
    if (is.null(self$required_aes)) {
      required_aes <- NULL
    } else {
      required_aes <- unlist(strsplit(self$required_aes, '|', fixed = TRUE))
    }
    c(union(required_aes, names(self$default_aes)), self$optional_aes, "group")
  }
)

# Helpers -----------------------------------------------------------------

#' @export
#' @rdname is_tests
is.geom <- function(x) inherits(x, "Geom")

eval_from_theme <- function(aesthetics, theme, class = NULL) {
  themed <- is_themed_aes(aesthetics)
  if (!any(themed)) {
    return(aesthetics)
  }

  element <- calc_element("geom", theme) %||% .default_geom_element
  class <- setdiff(class, c("Geom", "ggproto", "gg"))

  if (length(class) > 0) {

    # CamelCase to dot.case
    class <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1.\\2\\3", class)
    class <- gsub("([a-z])([A-Z])", "\\1.\\2", class)
    class <- to_lower_ascii(class)

    class <- class[class %in% names(theme)]

    # Inherit up to parent geom class
    if (length(class) > 0) {
      for (cls in rev(class)) {
        element <- combine_elements(theme[[cls]], element)
      }
    }
  }

  lapply(aesthetics[themed], eval_tidy, data = element)
}

#' Graphical units
#'
#' Multiply size in mm by these constants in order to convert to the units
#' that grid uses internally for `lwd` and `fontsize`.
#'
#' @name graphical-units
#' @keywords internal
#' @aliases NULL
NULL

#' @export
#' @rdname graphical-units
.pt <- 72.27 / 25.4
#' @export
#' @rdname graphical-units
.stroke <- 96 / 25.4

check_aesthetics <- function(x, n) {
  ns <- list_sizes(x)
  good <- ns == 1L | ns == n

  if (all(good)) {
    return()
  }

  cli::cli_abort(c(
    "Aesthetics must be either length 1 or the same as the data ({n}).",
    "x" = "Fix the following mappings: {.col {names(which(!good))}}."
  ))
}

fix_linewidth <- function(data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    deprecate_warn0("3.4.0", I(paste0("Using the `size` aesthetic with ", name)), I("the `linewidth` aesthetic"))
    data$linewidth <- data$size
  }
  data
}
