#' Sum unique values.  Useful for overplotting on scatterplots.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "sum")}
#'
#' @inheritParams stat_identity
#' @return a data.frame with additional columns
#'  \item{n}{number of observations at position}
#'  \item{prop}{percent of points in that panel at that position}
#' @export
#' @examples
#' d <- ggplot(diamonds, aes(x = cut, y = clarity))
#' d + stat_sum()
#'
#' # Always best to use in conjunction with scale_size_area which ensures
#' # 0 value is mapped to 0 size
#' d + stat_sum() +
#'   scale_size_area()
#'
#' #' # Can also weight by another variable
#' d + stat_sum(aes(weight = price)) +
#'   scale_size_area()
#'
#' # Or display proportions instead of counts. By default, all categorical
#' # variables in the plot form the grouping. Specifying stat_sum with no
#' # group identifier leads to a plot which is not useful:
#' d + stat_sum(aes(size = ..prop..))
#' # To correct this problem and achieve a more desirable plot, we need
#' # to specify which group the proportion is to be calculated over.
#' d + stat_sum(aes(size = ..prop.., group = 1)) +
#'   scale_size_area(max_size = 10)
#'
#' # Or group by x/y variables to have rows/columns sum to 1.
#' d + stat_sum(aes(size = ..prop.., group = cut)) +
#'   scale_size_area(max_size = 10)
#' d + stat_sum(aes(size = ..prop.., group = clarity)) +
#'   scale_size_area(max_size = 10)
stat_sum <- function (mapping = NULL, data = NULL, geom = "point",
  position = "identity", show_guide = NA, inherit.aes = TRUE, ...)
{
  Layer$new(
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

StatSum <- proto2(
  class = "StatSum",
  inherit = Stat,
  members = list(
    objname = "sum",

    default_aes = function(self) aes(size = ..n..),

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
)
