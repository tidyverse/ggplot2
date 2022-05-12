#' Histograms and frequency polygons
#'
#' Visualise the distribution of a single continuous variable by dividing
#' the x axis into bins and counting the number of observations in each bin.
#' Histograms (`geom_histogram()`) display the counts with bars; frequency
#' polygons (`geom_freqpoly()`) display the counts with lines. Frequency
#' polygons are more suitable when you want to compare the distribution
#' across the levels of a categorical variable.
#'
#' `stat_bin()` is suitable only for continuous x data. If your x data is
#' discrete, you probably want to use [stat_count()].
#'
#' By default, the underlying computation (`stat_bin()`) uses 30 bins;
#' this is not a good default, but the idea is to get you experimenting with
#' different number of bins. You can also experiment modifying the `binwidth` with
#' `center` or `boundary` arguments. `binwidth` overrides `bins` so you should do
#' one change at a time. You may need to look at a few options to uncover
#' the full story behind your data.
#'
#' In addition to `geom_histogram()`, you can create a histogram plot by using
#' `scale_x_binned()` with [geom_bar()]. This method by default plots tick marks
#' in between each bar.
#'
#' @eval rd_orientation()
#'
#' @section Aesthetics:
#' `geom_histogram()` uses the same aesthetics as [geom_bar()];
#' `geom_freqpoly()` uses the same aesthetics as [geom_line()].
#'
#' @export
#' @inheritParams layer
#' @inheritParams geom_bar
#' @param geom,stat Use to override the default connection between
#'   `geom_histogram()`/`geom_freqpoly()` and `stat_bin()`.
#' @examples
#' ggplot(diamonds, aes(carat)) +
#'   geom_histogram()
#' ggplot(diamonds, aes(carat)) +
#'   geom_histogram(binwidth = 0.01)
#' ggplot(diamonds, aes(carat)) +
#'   geom_histogram(bins = 200)
#' # Map values to y to flip the orientation
#' ggplot(diamonds, aes(y = carat)) +
#'   geom_histogram()
#'
#' # For histograms with tick marks between each bin, use `geom_bar()` with
#' # `scale_x_binned()`.
#' ggplot(diamonds, aes(carat)) +
#'   geom_bar() +
#'   scale_x_binned()
#'
#' # Rather than stacking histograms, it's easier to compare frequency
#' # polygons
#' ggplot(diamonds, aes(price, fill = cut)) +
#'   geom_histogram(binwidth = 500)
#' ggplot(diamonds, aes(price, colour = cut)) +
#'   geom_freqpoly(binwidth = 500)
#'
#' # To make it easier to compare distributions with very different counts,
#' # put density on the y axis instead of the default count
#' ggplot(diamonds, aes(price, after_stat(density), colour = cut)) +
#'   geom_freqpoly(binwidth = 500)
#'
#' if (require("ggplot2movies")) {
#' # Often we don't want the height of the bar to represent the
#' # count of observations, but the sum of some other variable.
#' # For example, the following plot shows the number of movies
#' # in each rating.
#' m <- ggplot(movies, aes(rating))
#' m + geom_histogram(binwidth = 0.1)
#'
#' # If, however, we want to see the number of votes cast in each
#' # category, we need to weight by the votes variable
#' m +
#'   geom_histogram(aes(weight = votes), binwidth = 0.1) +
#'   ylab("votes")
#'
#' # For transformed scales, binwidth applies to the transformed data.
#' # The bins have constant width on the transformed scale.
#' m +
#'  geom_histogram() +
#'  scale_x_log10()
#' m +
#'   geom_histogram(binwidth = 0.05) +
#'   scale_x_log10()
#'
#' # For transformed coordinate systems, the binwidth applies to the
#' # raw data. The bins have constant width on the original scale.
#'
#' # Using log scales does not work here, because the first
#' # bar is anchored at zero, and so when transformed becomes negative
#' # infinity. This is not a problem when transforming the scales, because
#' # no observations have 0 ratings.
#' m +
#'   geom_histogram(boundary = 0) +
#'   coord_trans(x = "log10")
#' # Use boundary = 0, to make sure we don't take sqrt of negative values
#' m +
#'   geom_histogram(boundary = 0) +
#'   coord_trans(x = "sqrt")
#'
#' # You can also transform the y axis.  Remember that the base of the bars
#' # has value 0, so log transformations are not appropriate
#' m <- ggplot(movies, aes(x = rating))
#' m +
#'   geom_histogram(binwidth = 0.5) +
#'   scale_y_sqrt()
#' }
#'
#' # You can specify a function for calculating binwidth, which is
#' # particularly useful when faceting along variables with
#' # different ranges because the function will be called once per facet
#' ggplot(economics_long, aes(value)) +
#'   facet_wrap(~variable, scales = 'free_x') +
#'   geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
geom_histogram <- function(mapping = NULL, data = NULL,
                           stat = "bin", position = "stack",
                           ...,
                           binwidth = NULL,
                           bins = NULL,
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      pad = FALSE,
      ...
    )
  )
}
