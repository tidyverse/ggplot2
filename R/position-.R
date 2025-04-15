#' Positions
#'
#' @description
#' All `position_*()` functions (like `position_dodge()`) return a
#' `Position*` object (like `PositionDodge`). The `Position*`
#' object is responsible for adjusting the position of overlapping geoms.
#'
#' @details
#' The way that the `position_*` functions work is slightly different from
#' the `geom_*` and `stat_*` functions, because a `position_*`
#' function actually "instantiates" the `Position*` object by creating a
#' descendant, and returns that. The object is chaperoned by the [Layer] class.
#'
#' To create a new type of Position object, you typically will want to override
#' one or more of the following:
#'
#' * The `required_aes` and `default_aes` fields.
#' * The `setup_params()` and `setup_data()` methods.
#' * One of the `compute_layer()` or `compute_panel()` methods.
#'
#' @section Convention:
#'
#' The object name that a new class is assigned to is typically the same as the
#' class name. Position class name are in UpperCamelCase and start with the
#' `Position*` prefix, like `PositionNew`.
#'
#' A constructor functions is usually paired with a Position class. The
#' constructor copies the position class and populates parameters. The
#' constructor function name is formatted by taking the Position class name and
#' formatting it with snake_case, so that `PositionNew` becomes `position_new()`.
#'
#' @export
#' @format NULL
#' @usage NULL
#' @seealso The `r link_book("new positions section", "extensions#new-positions")`
#' @family Layer components
#' @examples
#' # Extending the class
#' PositionRank <- ggproto(
#'   "PositionRank", Position,
#'   # Fields
#'   required_aes = c("x", "y"),
#'   # Methods
#'   setup_params = function(self, data) list(width = self$width),
#'   compute_panel = function(data, params, scales) {
#'     width  <- params$width %||% (resolution(data$x, zero = FALSE, TRUE) * 0.4)
#'     rank   <- ave(data$y, data$group, FUN = rank)
#'     rank   <- scales::rescale(rank, to = c(-width, width) / 2)
#'     data$x <- data$x + rank
#'     data
#'   }
#' )
#'
#' # Building a constructor
#' position_rank <- function(width = NULL) {
#'   ggproto(NULL, PositionRank, width = width)
#' }
#'
#' # Use new position in plot
#' ggplot(mpg, aes(drv, displ)) +
#'   geom_point(position = position_rank(width = 0.5))
Position <- ggproto(
  "Position",

  # Fields ------------------------------------------------------------------

  #' @field required_aes A character vector naming aesthetics that are necessary
  #' to compute the position adjustment.
  required_aes = character(),

  #' @field default_aes A [mapping][aes()] of default values for aesthetics.
  default_aes = aes(),

  # Methods -----------------------------------------------------------------

  ## compute_position -------------------------------------------------------

  #' @field use_defaults
  #' **Description**
  #'
  #' A function method for completing the layer data by filling in default
  #' position aesthetics that are not present. These can come from two sources:
  #' either from the layer parameters as static, unmapped aesthetics or from
  #' the `default_aes` field.
  #'
  #' **Usage**
  #' ```r
  #' Position$use_defaults(data, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame of the layer's data}
  #'   \item{`params`}{A list of fixed aesthetic parameters}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with completed layer data
  use_defaults = function(self, data, params = list()) {

    aes <- self$aesthetics()
    defaults <- self$default_aes

    params <- params[intersect(names(params), aes)]
    params <- params[setdiff(names(params), names(data))]
    defaults <- defaults[setdiff(names(defaults), c(names(params), names(data)))]

    if ((length(params) + length(defaults)) < 1) {
      return(data)
    }

    new <- compact(lapply(defaults, eval_tidy, data = data))
    new[names(params)] <- params
    check_aesthetics(new, nrow(data))

    data[names(new)] <- new
    data

  },

  #' @field setup_params
  #' **Description**
  #'
  #' A function method for modifying or checking the parameters based on the
  #' data. The default method returns an empty list.
  #'
  #' **Usage**
  #' ```r
  #' Position$setup_params(data)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of parameters
  setup_params = function(self, data) {
    list()
  },

  #' @field setup_data
  #' **Description**
  #'
  #' A function method for modifying or checking the data. The default method
  #' checks for the presence of required aesthetics.
  #'
  #' **Usage**
  #' ```r
  #' Position$setup_data(data, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  setup_data = function(self, data, params) {
    check_required_aesthetics(self$required_aes, names(data), snake_class(self))
    data
  },

  #' @field compute_layer
  #' **Description**
  #'
  #' A function method orchestrating the position adjust of the entire layer.
  #' The default method splits the data and passes on adjustment tasks to the
  #' panel-level `compute_panel()`. In addition, it finds the correct scales
  #' in the layout object to pass to the panel computation.
  #'
  #' **Usage**
  #' ```r
  #' Position$compute_layer(data, params, layout)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method}
  #'   \item{`layout`}{A `<Layout>` ggproto object.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  compute_layer = function(self, data, params, layout) {
    dapply(data, "PANEL", function(data) {
      if (empty(data)) return(data_frame0())

      scales <- layout$get_scales(data$PANEL[1])
      self$compute_panel(data = data, params = params, scales = scales)
    })
  },

  #' @field compute_panel
  #' **Description**
  #'
  #' A function method executing the position adjustment at the panel level.
  #' The default method is not implemented.
  #'
  #' **Usage**
  #' ```r
  #' Position$compute_panel(data, params, scales)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list of parameters coming from the `setup_params()`
  #'   method}
  #'   \item{`scales`}{A list of pre-trained `x` and `y` scales. Note that the
  #'   position scales are not finalised at this point and reflect the initial
  #'   data range before computing stats.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  compute_panel = function(self, data, params, scales) {
    cli::cli_abort("Not implemented.")
  },

  ## Utilities ---------------------------------------------------------------

  #' @field aesthetics
  #' **Description**
  #'
  #' A function method for listing out custom position aesthetics for this
  #' position adjustment.
  #'
  #' **Usage**
  #' ```r
  #' Position$aesthetics()
  #' ```
  #' **Value**
  #'
  #' A character vector of aesthetic names.
  aesthetics = function(self) {
    required_aes <- self$required_aes
    if (!is.null(required_aes)) {
      required_aes <- unlist(strsplit(self$required_aes, "|", fixed = TRUE))
    }
    c(union(required_aes, names(self$default_aes)))
  }
)

#' @export
#' @rdname is_tests
is_position <- function(x) inherits(x, "Position")

#' Convenience function to transform all position variables.
#'
#' @param trans_x,trans_y Transformation functions for x and y aesthetics.
#'   (will transform x, xmin, xmax, xend etc)
#' @param ... Additional arguments passed to `trans_x` and `trans_y`.
#' @keywords internal
#' @export
transform_position <- function(df, trans_x = NULL, trans_y = NULL, ...) {
  # Treat df as list during transformation for faster set/get
  oldclass <- class(df)
  df <- unclass(df)
  scales <- aes_to_scale(names(df))

  if (!is.null(trans_x)) {
    df[scales == "x"] <- lapply(df[scales == "x"], trans_x, ...)
  }
  if (!is.null(trans_y)) {
    df[scales == "y"] <- lapply(df[scales == "y"], trans_y, ...)
  }

  class(df) <- oldclass

  df
}
