#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams stat_density
#' @param scale if "area" (default), all violins have the same area (before trimming
#'   the tails). If "count", areas are scaled proportionally to the number of
#'   observations. If "width", all violins have the same maximum width.
#' @param drop Whether to discard groups with less than 2 observations
#'   (`TRUE`, default) or keep such groups for position adjustment purposes
#'   (`FALSE`).
#'
#' @eval rd_computed_vars(
#'   density = "Density estimate.",
#'   scaled  = "Density estimate, scaled to a maximum of 1.",
#'   count   = "Density * number of points - probably useless for violin
#'   plots.",
#'   violinwidth = "Density scaled for the violin plot, according to area,
#'   counts or to a constant maximum width.",
#'   n = "Number of points.",
#'   width = "Width of violin bounding box."
#' )
#'
#' @seealso [geom_violin()] for examples, and [stat_density()]
#'   for examples with data along the x axis.
#' @export
#' @rdname geom_violin
stat_ydensity <- function(mapping = NULL, data = NULL,
                          geom = "violin", position = "dodge",
                          ...,
                          bw = "nrd0",
                          adjust = 1,
                          kernel = "gaussian",
                          trim = TRUE,
                          scale = "area",
                          drop  = TRUE,
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          bounds = c(-Inf, Inf)) {
  scale <- arg_match0(scale, c("area", "count", "width"))

  layer(
    data = data,
    mapping = mapping,
    stat = StatYdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      scale = scale,
      drop  = drop,
      na.rm = na.rm,
      bounds = bounds,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatYdensity <- ggproto("StatYdensity", Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = TRUE, group_has_equal = TRUE)

    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(self, data, scales, width = NULL, bw = "nrd0", adjust = 1,
                       kernel = "gaussian", trim = TRUE, na.rm = FALSE,
                       drop = TRUE, flipped_aes = FALSE, bounds = c(-Inf, Inf)) {
    if (nrow(data) < 2) {
      if (isTRUE(drop)) {
        cli::cli_warn(c(
          "Groups with fewer than two datapoints have been dropped.",
          i = paste0(
            "Set {.code drop = FALSE} to consider such groups for position ",
            "adjustment purposes."
        )))
        return(data_frame0())
      }
      ans <- data_frame0(x = data$x, n = nrow(data))
      return(ans)
    }
    range <- range(data$y, na.rm = TRUE)
    modifier <- if (trim) 0 else 3
    bw <- calc_bw(data$y, bw)
    dens <- compute_density(
      data$y, data[["weight"]],
      from = range[1] - modifier * bw, to = range[2] + modifier * bw,
      bw = bw, adjust = adjust, kernel = kernel, bounds = bounds
    )

    dens$y <- dens$x

    # Compute width if x has multiple values
    if (vec_unique_count(data$x) > 1) {
      dens$x <- mean(range(data$x))
      width <- diff(range(data$x)) * 0.9
    } else {
      # Explicitly repeat to preserve data$x's mapped_discrete class
      dens$x <- vec_rep(data$x[1], nrow(dens))
    }
    dens$width <- width

    dens
  },

  compute_panel = function(self, data, scales, width = NULL, bw = "nrd0", adjust = 1,
                           kernel = "gaussian", trim = TRUE, na.rm = FALSE,
                           scale = "area", flipped_aes = FALSE, drop = TRUE,
                           bounds = c(-Inf, Inf)) {
    data <- flip_data(data, flipped_aes)
    data <- ggproto_parent(Stat, self)$compute_panel(
      data, scales, width = width, bw = bw, adjust = adjust, kernel = kernel,
      trim = trim, na.rm = na.rm, drop = drop, bounds = bounds,
    )
    if (!drop && any(data$n < 2)) {
      cli::cli_warn(
        "Cannot compute density for groups with fewer than two datapoints."
      )
    }

    # choose how violins are scaled relative to each other
    data$violinwidth <- switch(scale,
      # area : keep the original densities but scale them to a max width of 1
      #        for plotting purposes only
      area = data$density / max(data$density, na.rm = TRUE),
      # count: use the original densities scaled to a maximum of 1 (as above)
      #        and then scale them according to the number of observations
      count = data$density / max(data$density, na.rm = TRUE) *
        data$n / max(data$n),
      # width: constant width (density scaled to a maximum of 1)
      width = data$scaled
    )
    data$flipped_aes <- flipped_aes
    flip_data(data, flipped_aes)
  },

  dropped_aes = "weight"
)

calc_bw <- function(x, bw) {
  if (is.character(bw)) {
    if (length(x) < 2) {
      cli::cli_abort("{.arg x} must contain at least 2 elements to select a bandwidth automatically")
    }

    bw <- switch(
      to_lower_ascii(bw),
      nrd0 = stats::bw.nrd0(x),
      nrd = stats::bw.nrd(x),
      ucv = stats::bw.ucv(x),
      bcv = stats::bw.bcv(x),
      sj = ,
      `sj-ste` = stats::bw.SJ(x, method = "ste"),
      `sj-dpi` = stats::bw.SJ(x, method = "dpi"),
      cli::cli_abort("{.var {bw}} is not a valid bandwidth rule")
    )
  }
  bw
}
