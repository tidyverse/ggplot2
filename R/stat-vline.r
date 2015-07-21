#' Add a line with slope and intercept.
#'
#' @keywords internal
#' @inheritParams stat_identity
#' @seealso \code{\link{geom_abline}} for code examples.
#' @export
#' @examples
#' # see geom_abline
stat_abline <- function (mapping = NULL, data = NULL, geom = "abline",
  position = "identity", show_guide = NA, inherit.aes = TRUE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatAbline,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

StatAbline <- proto2(
  class = "StatAbline",
  inherit = Stat,
  members = list(
    objname = "abline",

    calculate = function(self, data, scales, intercept = NULL, slope = NULL, ...) {
      if (is.null(intercept)) {
        if (is.null(data$intercept)) data$intercept <- 0
      } else {
        data <- data[rep(1, length(intercept)), , drop = FALSE]
        data$intercept <- intercept
      }
      if (is.null(slope)) {
        if (is.null(data$slope)) data$slope <- 1
      } else {
        data <- data[rep(1, length(slope)), , drop = FALSE]
        data$slope <- slope
      }
      unique(data)
    }
  )
)

#' Add a vertical line
#'
#' @keywords internal
#' @inheritParams stat_identity
#' @seealso \code{\link{geom_vline}} for code examples.
#' @export
#' @examples
#' # see geom_vline
stat_vline <- function (mapping = NULL, data = NULL, geom = "vline",
  position = "identity", xintercept, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatVline,
    geom = geom,
    position = position,
    params = list(xintercept = xintercept, ...)
  )
}

StatVline <- proto2(
  class = "StatVline",
  inherit = Stat,
  members = list(
    objname = "vline",

    calculate = function(self, data, scales, xintercept = NULL, intercept, ...) {
      if (!missing(intercept)) {
        stop("stat_vline now uses xintercept instead of intercept")
      }
      data <- compute_intercept(data, xintercept, "x")

      unique(within(data, {
        x    <- xintercept
        xend <- xintercept
      }))
    },

    required_aes = c()
  )
)

#' Add a horizontal line
#'
#' @keywords internal
#' @inheritParams stat_identity
#' @seealso \code{\link{geom_hline}} for code examples.
#' @export
#' @examples
#' # see geom_hline
stat_hline <- function (mapping = NULL, data = NULL, geom = "hline",
  position = "identity", yintercept, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatHline,
    geom = geom,
    position = position,
    params = list(yintercept = yintercept, ...)
  )
}

StatHline <- proto2(
  class = "StatHline",
  inherit = Stat,
  members = list(
    calculate = function(self, data, scales, yintercept = NULL, intercept, ...) {
      if (!missing(intercept)) {
        stop("stat_hline now uses yintercept instead of intercept")
      }

      data <- compute_intercept(data, yintercept, "y")

      unique(within(data, {
        y    <- yintercept
        yend <- yintercept
      }))
    },

    objname = "hline",

    desc = "Add a horizontal line",

    required_aes = c(),

    examples = function(self) {
      # See geom_hline for examples
    }
  )
)


# Compute intercept for vline and hline from data and parameters
#
# @keyword internal
compute_intercept <- function(data, intercept, var = "x") {
  ivar <- paste(var, "intercept", sep = "")
  if (is.null(intercept)) {
    # Intercept comes from data, default to 0 if not set
    if (is.null(data[[ivar]])) data[[ivar]] <- 0

  } else if (is.numeric(intercept)) {
    # Intercept is a numeric vector of positions
    data <- data[rep(1, length(intercept)), ]
    data[[ivar]] <- intercept

  } else if (is.character(intercept) || is.function(intercept)) {
    # Intercept is a function
    f <- match.fun(intercept)
    trans <- function(data) {
      data[[ivar]] <- f(data[[var]])
      data
    }
    data <- ddply(data, "group", trans)
  } else {
    stop("Invalid intercept type: should be a numeric vector, a function",
         ", or a name of a function", call. = FALSE)
  }
  data
}
