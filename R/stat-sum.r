#' Sum unique values.  Useful for overplotting on scatterplots.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "sum")}
#'
#' @seealso \code{\link{ggfluctuation}} for a fluctuation diagram,
#' @inheritParams stat_identity
#' @return a data.frame with additional columns
#'  \item{n}{number of observations at position}
#'  \item{prop}{percent of points in that panel at that position}
#' @export
#' @examples
#' \donttest{
#' d <- ggplot(diamonds, aes(x = cut, y = clarity))
#' # By default, all categorical variables in the plot form grouping
#' # variables, and the default behavior in stat_sum is to show the
#' # proportion. Specifying stat_sum with no group identifier leads to
#' # a plot which is not meaningful:
#' d + stat_sum()
#' # To correct this problem and achieve a more desirable plot, we need
#' # to specify which group the proportion is to be calculated over.
#' # There are several ways to do this:
#'
#' # by overall proportion
#' d + stat_sum(aes(group = 1))
#' d + stat_sum(aes(group = 1)) + scale_size(range = c(3, 10))
#' d + stat_sum(aes(group = 1)) + scale_size_area(max_size = 10)
#'
#' # by cut
#' d + stat_sum(aes(group = cut))
#' d + stat_sum(aes(group = cut, colour = cut))
#'
#' # by clarity
#' d + stat_sum(aes(group = clarity))
#' d + stat_sum(aes(group = clarity, colour = cut))
#'
#' # Instead of proportions, can also use sums
#' d + stat_sum(aes(size = ..n..))
#'
#' # Can also weight by another variable
#' d + stat_sum(aes(group = 1, weight = price))
#' d + stat_sum(aes(group = 1, weight = price, size = ..n..))
#'
#' # Or using qplot
#' qplot(cut, clarity, data = diamonds)
#' qplot(cut, clarity, data = diamonds, stat = "sum", group = 1)
#' }
stat_sum <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", ...) {
  StatSum$new(mapping = mapping, data = data, geom = geom, position = position, ...)
}

StatSum <- proto(Stat, {
  objname <- "sum"

  default_aes <- function(.) aes(size = ..prop..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPoint

  calculate_groups <- function(., data, scales, ...) {

    if (is.null(data$weight)) data$weight <- 1

    group_by <- setdiff(intersect(names(data), .all_aesthetics), "weight")

    counts <- count(data, group_by, wt_var = "weight")
    counts <- rename(counts, c(freq = "n"), warn_missing = FALSE)
    counts$prop <- ave(counts$n, counts$group, FUN = prop.table)
    counts
  }
})
