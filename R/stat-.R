#' Stats
#'
#' @description
#' All `stat_*()` functions (like `stat_bin()`) return a layer that
#' contains a `Stat*` object (like `StatBin`). The `Stat*`
#' object is responsible for rendering the data in the plot.
#'
#' @details
#' Each of the `Stat*` objects is a [ggproto()] object, descended
#' from the top-level `Stat`, and each implements various methods and
#' fields. The object and its parameters are chaperoned by the [Layer] class.
#'
#' To create a new type of Stat object, you typically will want to
#' override one or more of the following:
#'
#' * The `required_aes` and `default_aes` fields.
#' * One of the `compute_layer()`, `compute_panel()` or `compute_group()`
#'   functions. Typically it best to implement `compute_group()` and use the
#'   higher-up methods when there are substantial performance improvements to
#'   be gained.
#' * The `finish_layer()` method
#'
#' @section Conventions:
#'
#' The object name that a new class is assigned to is typically the same as the
#' class name. Stat class names are in UpperCamelCase and start with the `Stat*`
#' prefix, like `StatNew`.
#'
#' A constructor function is usually paired wih a Stat class. The constructor
#' wraps a call to `layer()`, where e.g. `layer(stat = StatNew)`. The
#' constructor function name is formatted by taking the Stat class name and
#' formatting it with snake_case, so that `StatNew` becomes `stat_new()`.
#'
#' @export
#' @family Layer components
#' @format NULL
#' @usage NULL
#' @seealso The `r link_book("new stats section", "extensions#sec-new-stats")`.
#' @seealso Run `vignette("extending-ggplot2")`, in particular the "Creating a
#' new stat" section.
#' @examples
#' # Extending the class
#' StatKmeans <- ggproto(
#'   "StatKmeans", Stat,
#'   # Fields
#'   required_aes = c("x", "y"),
#'   # You can relate computed variables to aesthetics using `after_stat()`
#'   # in defaults
#'   default_aes = aes(colour = after_stat(cluster)),
#'   # Methods
#'   compute_panel = function(data, scales, k = 2L) {
#'     km <- kmeans(cbind(scale(data$x), scale(data$y)), centers = k)
#'     data$cluster <- factor(km$cluster)
#'     data
#'   }
#' )
#'
#' # Building a constructor
#' stat_kmeans <- function(mapping = NULL, data = NULL, geom = "point",
#'                         position = "identity", ..., k = 2L, na.rm = FALSE,
#'                         show.legend = NA, inherit.aes = TRUE) {
#'   layer(
#'     mapping = mapping, data = data,
#'     geom = geom, stat = StatKmeans, position = position,
#'     show.legend = show.legend, inherit.aes = inherit.aes,
#'     params = list(na.rm = na.rm, k = k, ...)
#'   )
#' }
#'
#' # Use new stat in plot
#' ggplot(mpg, aes(displ, hwy)) +
#'   stat_kmeans(k = 3)
Stat <- ggproto(
  "Stat",

  # Fields ------------------------------------------------------------------

  #' @field required_aes A character vector naming aesthetics that are necessary
  #' to compute the stat.
  required_aes = character(),

  #' @field non_missing_aes A character vector naming aesthetics that will cause
  #' removal if they have missing values.
  non_missing_aes = character(),

  #' @field optional_aes A character vector naming aesthetics that will be
  #' accepted by `layer()`, but are not required or dscribed in the `default_aes`
  #' field.
  optional_aes = character(),

  #' @field default_aes A [mapping][aes()] of default values for aesthetics.
  #' Aesthetics can be set to `NULL` to be included as optional aesthetic.
  default_aes = aes(),

  #' @field dropped_aes A character vector naming aesthetics that can be dropped
  #' from the data without warning. Typically used for aesthetics that are
  #' 'consumed' during computation like `"weight"`.
  dropped_aes = character(),

  #' @field extra_params A character vector of parameter names in addition to
  #' those imputed from the `compute_panel()` or `compute_groups()` methods.
  #' This field can be set to include parameters for `setup_data()` methods.
  #' By default, this only contains `"na.rm"`.
  extra_params = "na.rm",

  #' @field retransform A scalar boolean: should the values produced by the
  #' statistic also be transformed in the second pass when recently added
  #' statistics are trained to the scales
  retransform = TRUE,

  # Methods -----------------------------------------------------------------

  ## compute_statistic ------------------------------------------------------

  #' @field setup_params
  #' **Description**
  #'
  #' A function method for modifying or checking the parameters based on the
  #' data. The default method returns the parameters unaltered.
  #'
  #' **Usage**
  #' ```r
  #' Stat$setup_params(data, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list of current parameters}
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
  #' A function method for modifying or checking the data. The default method
  #' returns data unaltered.
  #'
  #' **Usage**
  #' ```r
  #' Stat$setup_data(data, params)
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
  setup_data = function(data, params) {
    data
  },

  #' @field compute_layer
  #' **Description**
  #'
  #' A function method for orchestrating the computation of the statistic. The
  #' default method splits the data and passes on computation tasks to the
  #' panel-level `compute_panel()` method. In addition, the default method
  #' handles missing values by removing rows that have missing values for the
  #' aesthetics listed in the `required_aes` and `non_missing_aes` fields. It is
  #' not recommended to use this method as an extension point.
  #'
  #' **Usage**
  #' ```r
  #' Stat$compute_layer(data, params, layout)
  #' ```
  #'
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list of parameters}
  #'   \item{`layout`}{A pre-trained `<Layout>` ggproto object.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with computed data
  compute_layer = function(self, data, params, layout) {
    check_required_aesthetics(
      self$required_aes,
      c(names(data), names(params)),
      snake_class(self)
    )

    # TODO: for symmetry with Geom, should Stat have separate `handle_na()` method?
    # Make sure required_aes consists of the used set of aesthetics in case of
    # "|" notation in self$required_aes
    required_aes <- intersect(
      names(data),
      unlist(strsplit(self$required_aes, "|", fixed = TRUE))
    )

    data <- remove_missing(data, params$na.rm,
      c(required_aes, self$non_missing_aes),
      snake_class(self),
      finite = TRUE
    )

    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]

    args <- c(list(data = quote(data), scales = quote(scales)), params)
    dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      try_fetch(
        inject(self$compute_panel(data = data, scales = scales, !!!params)),
        error = function(cnd) {
          cli::cli_warn("Computation failed in {.fn {snake_class(self)}}.", parent = cnd)
          data_frame0()
        }
      )
    })
  },

  #' @field compute_panel,compute_group
  #' **Description**
  #'
  #' A function method orchestrating the computation of statistics for a single
  #' panel or group. The default `compute_panel()` method splits the data into
  #' groups, and passes on computation tasks to the `compute_group()` method.
  #' In addition, `compute_panel()` is tasked with preserving aesthetics that
  #' are constant within a group and preserving these if the computation loses
  #' them. The default `compute_group()` is not implemented.
  #'
  #' **Usage**
  #' ```r
  #' Stat$compute_panel(data, scales, ...)
  #' Stat$compute_group(data, scales, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`scales`}{A list of pre-trained `x` and `y` scales. Note that the
  #'   position scales are not finalised at this point and reflect the initial
  #'   data range before computing stats.}
  #'   \item{`...`}{Reserved for extensions. By default, this passes parameters
  #'   to the `compute_group()` method.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  compute_panel = function(self, data, scales, ...) {
    if (empty(data)) return(data_frame0())

    groups <- split(data, data$group)
    stats <- lapply(groups, function(group) {
      self$compute_group(data = group, scales = scales, ...)
    })

    # Record columns that are not constant within groups. We will drop them later.
    non_constant_columns <- character(0)

    stats <- mapply(function(new, old) {
      # In this function,
      #
      #   - `new` is the computed result. All the variables will be picked.
      #   - `old` is the original data. There are 3 types of variables:
      #     1) If the variable is already included in `new`, it's ignored
      #        because the values of `new` will be used.
      #     2) If the variable is not included in `new` and the value is
      #        constant within the group, it will be picked.
      #     3) If the variable is not included in `new` and the value is not
      #        constant within the group, it will be dropped. We need to record
      #        the dropped columns to drop it consistently later.

      if (empty(new)) return(data_frame0())

      # First, filter out the columns already included `new` (type 1).
      old <- old[, !(names(old) %in% names(new)), drop = FALSE]

      # Then, check whether the rest of the columns have constant values (type 2)
      # or not (type 3).
      non_constant <- vapply(old, vec_unique_count, integer(1)) > 1L

      # Record the non-constant columns.
      non_constant_columns <<- c(non_constant_columns, names(old)[non_constant])

      vec_cbind(
        new,
        # Note that, while the non-constant columns should be dropped, we don't
        # do this here because it can be filled by vec_rbind() later if either
        # one of the group has a constant value (see #4394 for the details).
        old[rep(1, nrow(new)), , drop = FALSE]
      )
    }, stats, groups, SIMPLIFY = FALSE)

    non_constant_columns <- unique0(non_constant_columns)

    # We are going to drop columns that are not constant within groups and not
    # carried over/recreated by the stat. This can produce unexpected results,
    # and hence we warn about it (variables in dropped_aes are expected so
    # ignored here).
    dropped <- non_constant_columns[!non_constant_columns %in% self$dropped_aes]
    if (length(dropped) > 0) {
      cli::cli_warn(c(
        "The following aesthetics were dropped during statistical transformation: {.field {dropped}}.",
        "i" = "This can happen when ggplot fails to infer the correct grouping structure in the data.",
        "i" = "Did you forget to specify a {.code group} aesthetic or to convert a numerical variable into a factor?"
      ))
    }

    # Finally, combine the results and drop columns that are not constant.
    data_new <- vec_rbind0(!!!stats)
    data_new[, !names(data_new) %in% non_constant_columns, drop = FALSE]
  },

  compute_group = function(self, data, scales) {
    cli::cli_abort("Not implemented.")
  },

  # finish_statistics -------------------------------------------------------

  #' @field finish_layer
  #' **Description**
  #'
  #' A function method acting as a hook to modify data after scales have been
  #' applied, but before geoms have to render. The default is to pass the data
  #' unaltered. This can be used as an extension point when actual aesthetic
  #' values rather than values mapped to the aesthetic are needed.
  #'
  #' **Usage**
  #' ```r
  #' Stat$finish_layer(data, params)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with layer data}
  #'   \item{`params`}{A list of parameters}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  finish_layer = function(self, data, params) {
    data
  },

  ## Utilities ---------------------------------------------------------------

  #' @field parameters
  #' **Description**
  #'
  #' A function method for listing out all acceptable parameters for this stat.
  #'
  #' **Usage**
  #' ```r
  #' Stat$parameters(extra)
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
    # Look first in compute_panel. If it contains ... then look in compute_group
    panel_args <- names(ggproto_formals(self$compute_panel))
    group_args <- names(ggproto_formals(self$compute_group))
    args <- if ("..." %in% panel_args) group_args else panel_args

    # Remove arguments of defaults
    args <- setdiff(args, names(ggproto_formals(Stat$compute_group)))

    if (extra) {
      args <- union(args, self$extra_params)
    }
    args
  },

  #' @field aesthetics
  #' **Description**
  #'
  #' A function method for listing out all acceptable aesthetics for this stat.
  #'
  #' **Usage**
  #' ```r
  #' Stat$aesthetics()
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

#' @export
#' @rdname is_tests
is.stat <- function(x) inherits(x, "Stat")
