#' @section Stats:
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' \itemize{
#'   \item Override either \code{compute_panel(self, data, panel_info, ...)} or
#'     \code{compute_group(self, data, panel_info, ...)}. \code{compute_panel} is
#'     called once per panel, \code{compute_group} is called once per group.
#'     If you override \code{compute}, you're responsible for preserving
#'     non-transformed columns.
#'
#'     \code{data} is a data frame containing the variables named according
#'     to the aesthetics that they're mapped to. \code{panel_info} is a list
#'     of scale information for the panel. This is present mostly for historical
#'     reasons, and I would discourage you from relying on it. \code{...}
#'     contains the parameters returned by \code{compute_defaults()}.
#'
#'     Must return a data frame.
#'   \item \code{compute_defaults(data, params)}: called once for each layer.
#'      Used to compute defaults that need to complete dataset, and to inform
#'      the user of important choices. Returns a list of updated parameters.
#'   \item \code{compute_data(data, params)}: called once for each layer,
#'      after \code{compute_defaults()}. Returns modified \code{data}.
#'      Default methods removes all rows containing a missing value in
#'      required aesthetics (with a warning if \code{!na.rm}).
#'   \item \code{required_aes}: A character vector of aesthetics needed to
#'     render the geom.
#'   \item \code{default_aes}: A list (generated by \code{\link{aes}()} of
#'     default values for aesthetics.
#' }
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Stat <- ggproto("Stat",
  # Should the values produced by the statistic also be transformed
  # in the second pass when recently added statistics are trained to
  # the scales
  retransform = TRUE,

  default_aes = aes(),

  required_aes = c(),

  compute_defaults = function(data, params) {
    params
  },

  compute_data = function(self, data, params) {
    remove_missing(data, isTRUE(params$na.rm), self$required_aes, name = snake_class(self))
  },

  compute_panel = function(self, data, panel_info, ...) {
    if (empty(data)) return(data.frame())

    groups <- split(data, data$group)
    stats <- lapply(groups, function(group) {
      self$compute_group(data = group, panel_info = panel_info, ...)
    })

    stats <- mapply(function(new, old) {
      if (empty(new)) return(data.frame())
      unique <- uniquecols(old)
      missing <- !(names(unique) %in% names(new))
      cbind(
        new,
        unique[rep(1, nrow(new)), missing,drop = FALSE]
      )
    }, stats, groups, SIMPLIFY = FALSE)

    do.call(plyr::rbind.fill, stats)
  },

  compute_group = function(self, data, panel_info, ...) {
    stop("Not implemented", call. = FALSE)
  }
)

# make_stat("bin") returns StatBin
make_stat <- function(class) {
  name <- paste0("Stat", camelize(class, first = TRUE))
  if (!exists(name)) {
    stop("No stat called ", name, ".", call. = FALSE)
  }

  obj <- get(name)
  if (!inherits(obj, "Stat")) {
    stop("Found object is not a stat", call. = FALSE)
  }

  obj
}
