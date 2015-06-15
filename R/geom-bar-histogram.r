#' Histogram
#'
#' \code{geom_histogram} is an alias for \code{\link{geom_bar}} plus
#' \code{\link{stat_bin}} so you will need to look at the documentation for
#' those objects to get more information about the parameters.
#'
#' By default, \code{stat_bin} uses 30 bins - this is not a good default,
#' but the idea is to get you experimenting with different binwidths. You
#' may need to look at a few to uncover the full story behind your data.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "histogram")}
#'
#' @export
#' @inheritParams geom_point
#' @examples
#' \donttest{
#' set.seed(5689)
#' movies <- movies[sample(nrow(movies), 1000), ]
#' # Basic example
#' m <- ggplot(movies, aes(rating))
#' m + geom_histogram()
#' m + geom_histogram(aes(weight = votes))
#'
#' # More complex
#' m + geom_histogram(aes(y = ..density..)) +
#'     geom_density()
#'
#' m + geom_histogram(binwidth = 1)
#' m + geom_histogram(binwidth = 0.5)
#' m + geom_histogram(binwidth = 0.1)
#'
#' # Add aesthetic mappings
#' m + geom_histogram(aes(weight = votes))
#' m + geom_histogram(aes(y = ..count..))
#' m + geom_histogram(aes(fill = ..count..))
#'
#' # Change scales
#' m + geom_histogram(aes(fill = ..count..)) +
#'   scale_fill_gradient("Count", low = "green", high = "red")
#'
#' # Often we don't want the height of the bar to represent the
#' # count of observations, but the sum of some other variable.
#' # For example, the following plot shows the number of movies
#' # in each rating.
#'
#' m + geom_bar(binwidth = 0.1)
#'
#' # If, however, we want to see the number of votes cast in each
#' # category, we need to weight by the votes variable
#'
#' m + geom_bar(aes(weight = votes), binwidth = 0.1) +
#'     ylab("votes")
#'
#' m <- ggplot(movies, aes(x = votes))
#'
#' # For transformed scales, binwidth applies to the transformed data.
#' # The bins have constant width on the transformed scale.
#' m + geom_histogram() + scale_x_log10()
#' m + geom_histogram(binwidth = 1) + scale_x_log10()
#' m + geom_histogram() + scale_x_sqrt()
#' m + geom_histogram(binwidth = 10) + scale_x_sqrt()
#'
#' # For transformed coordinate systems, the binwidth applies to the
#' # raw data.  The bins have constant width on the original scale.
#'
#' # Using log scales does not work here, because the first
#' # bar is anchored at zero, and so when transformed becomes negative
#' # infinity.  This is not a problem when transforming the scales, because
#' # no observations have 0 ratings.
#' m + geom_histogram(origin = 0) + coord_trans(x = "log10")
#' # Use origin = 0, to make sure we don't take sqrt of negative values
#' m + geom_histogram(origin = 0) + coord_trans(x = "sqrt")
#' m + geom_histogram(origin = 0, binwidth = 1000) + coord_trans(x = "sqrt")
#'
#' # You can also transform the y axis.  Remember that the base of the bars
#' # has value 0, so log transformations are not appropriate
#' m <- ggplot(movies, aes(x = rating))
#' m + geom_histogram(binwidth = 0.5) + scale_y_sqrt()
#' m + geom_histogram(binwidth = 0.5) + scale_y_reverse()
#'
#' # Set aesthetics to fixed value
#' m + geom_histogram(colour = "darkgreen", fill = "white", binwidth = 0.5)
#'
#' # Use facets
#' m <- m + geom_histogram(binwidth = 0.5)
#' m + facet_grid(Action ~ Comedy)
#'
#' # Often more useful to use density on the y axis when facetting
#' m <- m + aes(y = ..density..)
#' m + facet_grid(Action ~ Comedy)
#' m + facet_wrap(~ mpaa)
#'
#' # In some situations (e.g., a plot composition), it may be useful
#' # to draw an histogram horizontally with the `h` variant:
#' ggplot(movies, aes(y = rating)) +
#'   facet_grid(Action ~ Comedy) +
#'   geom_histogramh()
#' 
#' # Multiple histograms on the same graph
#' # see ?position, ?position_fill, etc for more details.
#' set.seed(6298)
#' diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ]
#' ggplot(diamonds_small, aes(x=price)) + geom_bar()
#' hist_cut <- ggplot(diamonds_small, aes(x=price, fill=cut))
#' hist_cut + geom_bar() # defaults to stacking
#' hist_cut + geom_bar(position="fill")
#' hist_cut + geom_bar(position="dodge")
#'
#' # This is easy in ggplot2, but not visually effective.  It's better
#' # to use a frequency polygon or density plot.  Like this:
#' ggplot(diamonds_small, aes(price, ..density.., colour = cut)) +
#'   geom_freqpoly(binwidth = 1000)
#' # Or this:
#' ggplot(diamonds_small, aes(price, colour = cut)) +
#'   geom_density()
#' # Or if you want to be fancy, maybe even this:
#' ggplot(diamonds_small, aes(price, fill = cut)) +
#'   geom_density(alpha = 0.2)
#' # Which looks better when the distributions are more distinct
#' ggplot(diamonds_small, aes(depth, fill = cut)) +
#'   geom_density(alpha = 0.2) + xlim(55, 70)
#' }
#' rm(movies)
geom_histogram <- function (mapping = NULL, data = NULL, stat = "bin", position = "stack", ...) {
  GeomHistogram$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomHistogram <- proto(GeomBar, {
  objname <- "histogram"
})


#' @rdname geom_histogram
#' @export
geom_histogramh <- function (mapping = NULL, data = NULL, stat = "binh", position = "stackh", ...) {
  GeomHistogramh$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomHistogramh <- proto(GeomBarh, {
  objname <- "histogramh"
})
