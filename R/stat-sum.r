#' @inheritParams stat_identity
#' @return a data.frame with additional columns
#'  \item{n}{number of observations at position}
#'  \item{prop}{percent of points in that panel at that position}
#' @export
#' @rdname geom_count
stat_sum <- function (mapping = NULL, data = NULL, geom = "point",
  position = "identity", show_guide = NA, inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = StatSum,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

StatSum <- proto2("StatSum", Stat,
  default_aes = aes(size = ..n..),

  required_aes = c("x", "y"),

  calculate_groups = function(self, data, scales, ...) {

    if (is.null(data$weight)) data$weight <- 1

    group_by <- setdiff(intersect(names(data), .all_aesthetics), "weight")

    counts <- count(data, group_by, wt_var = "weight")
    counts <- rename(counts, c(freq = "n"), warn_missing = FALSE)
    counts$prop <- ave(counts$n, counts$group, FUN = prop.table)
    counts
  }
)
