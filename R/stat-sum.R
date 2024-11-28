#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSum <- ggproto(
  "StatSum", Stat,
  default_aes = aes(size = after_stat(n), weight = 1),

  required_aes = c("x", "y"),

  compute_panel = function(data, scales) {
    if (is.null(data$weight)) data$weight <- 1

    group_by <- setdiff(intersect(names(data), ggplot_global$all_aesthetics), "weight")

    counts <- count(data, group_by, wt_var = "weight")
    counts <- rename(counts, c(freq = "n"))
    counts$prop <- stats::ave(counts$n, counts$group, FUN = prop.table)
    counts
  }
)

#' @inheritParams layer
#' @inheritParams geom_point
#' @eval rd_computed_vars(
#'   n = "Number of observations at position.",
#'   prop = "Percent of points in that panel at that position."
#' )
#' @export
#' @rdname geom_count
stat_sum <- make_constructor(StatSum, geom = "point")
