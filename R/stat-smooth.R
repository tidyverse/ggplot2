#' @param method Smoothing method (function) to use, accepts either
#'   `NULL` or a character vector, e.g. `"lm"`, `"glm"`, `"gam"`, `"loess"`
#'   or a function, e.g. `MASS::rlm` or `mgcv::gam`, `stats::lm`, or `stats::loess`.
#'   `"auto"` is also accepted for backwards compatibility.  It is equivalent to
#'   `NULL`.
#'
#'   For `method = NULL` the smoothing method is chosen based on the
#'   size of the largest group (across all panels). [stats::loess()] is
#'   used for less than 1,000 observations; otherwise [mgcv::gam()] is
#'   used with `formula = y ~ s(x, bs = "cs")` with `method = "REML"`. Somewhat anecdotally,
#'   `loess` gives a better appearance, but is \eqn{O(N^{2})}{O(N^2)} in memory,
#'   so does not work for larger datasets.
#'
#'   If you have fewer than 1,000 observations but want to use the same `gam()`
#'   model that `method = NULL` would use, then set
#'   `method = "gam", formula = y ~ s(x, bs = "cs")`.
#' @param formula Formula to use in smoothing function, eg. `y ~ x`,
#'   `y ~ poly(x, 2)`, `y ~ log(x)`. `NULL` by default, in which case
#'   `method = NULL` implies `formula = y ~ x` when there are fewer than 1,000
#'   observations and `formula = y ~ s(x, bs = "cs")` otherwise.
#' @param se Display confidence band around smooth? (`TRUE` by default, see
#'   `level` to control.)
#' @param fullrange If `TRUE`, the smoothing line gets expanded to the range of the plot,
#'   potentially beyond the data. This does not extend the line into any additional padding
#'   created by `expansion`.
#' @param xseq A numeric vector of values at which the smoother is evaluated.
#'   When `NULL` (default), `xseq` is internally evaluated as a sequence of `n`
#'   equally spaced points for continuous data.
#' @param level Level of confidence band to use (0.95 by default).
#' @param span Controls the amount of smoothing for the default loess smoother.
#'   Smaller numbers produce wigglier lines, larger numbers produce smoother
#'   lines. Only used with loess, i.e. when `method = "loess"`,
#'   or when `method = NULL` (the default) and there are fewer than 1,000
#'   observations.
#' @param n Number of points at which to evaluate smoother.
#' @param method.args List of additional arguments passed on to the modelling
#'   function defined by `method`.
#'
#' @eval rd_computed_vars(
#'   .details = "`stat_smooth()` provides the following variables, some of
#'   which depend on the orientation:",
#'   "y|x" = "Predicted value.",
#'   "ymin|xmin" = "Lower pointwise confidence band around the mean.",
#'   "ymax|xmax" = "Upper pointwise confidence band around the mean.",
#'   "se" = "Standard error."
#' )
#' @export
#' @rdname geom_smooth
stat_smooth <- function(mapping = NULL, data = NULL,
                        geom = "smooth", position = "identity",
                        ...,
                        method = NULL,
                        formula = NULL,
                        se = TRUE,
                        n = 80,
                        span = 0.75,
                        fullrange = FALSE,
                        xseq = NULL,
                        level = 0.95,
                        method.args = list(),
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmooth,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      span = span,
      xseq = xseq,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSmooth <- ggproto("StatSmooth", Stat,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    msg <- character()
    method <- params$method
    if (is.null(method) || identical(method, "auto")) {
      # Use loess for small datasets, gam with a cubic regression basis for
      # larger. Based on size of the _largest_ group to avoid bad memory
      # behaviour of loess
      max_group <- max(table(interaction(data$group, data$PANEL, drop = TRUE)))

      if (max_group < 1000) {
        method <- "loess"
      } else {
        method <- "gam"
      }
      msg <- c(msg, paste0("method = '", method, "'"))
    }

    if (identical(method, "gam") &&
        !prompt_install("mgcv", "for using {.code method = \"gam\"}")) {
      cli::cli_inform(c(
        "The {.arg method} was set to {.val gam}, but {.pkg mgcv} is not installed.",
        "!" = "Falling back to {.code method = \"lm\"}.",
        i = "Install {.pkg mgcv} or change the {.arg method} argument to \\
        resolve this issue."
      ))
      method <- "lm"
    }

    if (is.null(params$formula)) {
      if (identical(method, "gam")) {
        params$formula <- y ~ s(x, bs = "cs")
      } else {
        params$formula <- y ~ x
      }
      msg <- c(msg, paste0("formula = '", deparse(params$formula), "'"))
    }

    # Special case span because it's the most commonly used model argument
    if (identical(method, "loess")) {
      params$method.args$span <- params$span %||% 0.75
    }

    if (is.character(method)) {
      if (identical(method, "gam")) {
        method <- gam_method()
      } else {
        method <- match.fun(method)
      }
    }
    # If gam and gam's method is not specified by the user then use REML
    if (identical(method, gam_method())) {
      params$method.args$method <- params$method.args$method %||% "REML"
    }

    if (length(msg) > 0) {
      cli::cli_inform("{.fn geom_smooth} using {msg}")
    }

    params$method <- method
    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(data, scales, method = NULL, formula = NULL,
                           se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                           xseq = NULL, level = 0.95, method.args = list(),
                           na.rm = FALSE, flipped_aes = NA) {
    data <- flip_data(data, flipped_aes)
    if (vec_unique_count(data$x) < 2) {
      # Not enough data to perform fit
      return(data_frame0())
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scales$x$dimension()
        } else {
          xseq <- sort(unique0(data$x))
        }
      } else {
        if (fullrange) {
          range <- scales$x$dimension()
        } else {
          range <- range(data$x, na.rm = TRUE)
        }
        xseq <- seq(range[1], range[2], length.out = n)
      }
    }

    prediction <- try_fetch(
      {
        model <- inject(method(
          formula,
          data = data,
          weights = weight,
          !!!method.args
        ))
        predictdf(model, xseq, se, level)
      },
      error = function(cnd) {
        cli::cli_warn("Failed to fit group {data$group[1]}.", parent = cnd)
        NULL
      }
    )
    if (is.null(prediction)) {
      return(NULL)
    }

    prediction$flipped_aes <- flipped_aes
    flip_data(prediction, flipped_aes)
  },

  dropped_aes = c("weight"),

  required_aes = c("x", "y")
)

# This function exists to silence an undeclared import warning
gam_method <- function() {
  if (is_installed("mgcv")) {
    mgcv::gam
  } else {
    NA
  }
}
