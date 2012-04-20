#' Bars, rectangles with bases on x-axis
#' 
#' The bar geom is used to produce 1d area plots: bar charts for categorical
#' x, and histograms for continuous y.  stat_bin explains the details of 
#' these summaries in more detail.  In particular, you can use the
#' \code{weight} aesthetic to create weighted histograms and barcharts where
#' the height of the bar no longer represent a count of observations, but a
#' sum over some other variable.  See the examples for a practical
#' example.
#'
#' By default, multiple x's occuring in the same place will be stacked a top
#' one another by position_stack.  If you want them to be dodged from
#' side-to-side, see \code{\link{position_dodge}}. Finally, 
#' \code{\link{position_fill}} shows relative propotions at each x by stacking
#' the bars and then stretching or squashing to the same height.
#'
#' If you have presummarised data, use \code{stat="identity"} to turn off the
#' default summary.
#'
#' Sometimes, bar charts are used not as a distributional summary, but 
#' instead of a dotplot.  Generally, it's preferable to use a dotplot (see
#' \code{geom_point}) as it has a better data-ink ratio.  However, if you do
#' want to create this type of plot, you can set y to the value you have
#' calculated, and use \code{stat='identity'}
#'
#' A bar chart maps the height of the bar to a variable, and so the base of
#' the bar must always been shown to produce a valid visual comparison.  
#' Naomi Robbins has a nice
#' \href{http://www.b-eye-network.com/view/index.php?cid=2468}{article on this topic}.  
#' This is the reason it doesn't make sense to use a log-scaled y axis with a bar chart
#'
#' @seealso \code{\link{stat_bin}} for more details of the binning alogirithm, 
#'   \code{\link{position_dodge}} for creating side-by-side barcharts, 
#'   \code{\link{position_stack}} for more info on stacking,
#' @export
#' @inheritParams geom_point
#' @examples
#' \donttest{
#' # Generate data
#' c <- ggplot(mtcars, aes(factor(cyl)))
#' 
#' c + geom_bar()
#' c + geom_bar(width=.5)
#' c + geom_bar() + coord_flip()
#' c + geom_bar(fill="white", colour="darkgreen")
#' 
#' # Use qplot
#' qplot(factor(cyl), data=mtcars, geom="bar")
#' qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))
#' 
#' # Stacked bar charts    
#' qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(vs))
#' qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(gear))
#' 
#' # Stacked bar charts are easy in ggplot2, but not effective visually, 
#' # particularly when there are many different things being stacked
#' ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
#' ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
#' 
#' # Faceting is a good alternative:
#' ggplot(diamonds, aes(clarity)) + geom_bar() + 
#'   facet_wrap(~ cut)
#' # If the x axis is ordered, using a line instead of bars is another
#' # possibility:
#' ggplot(diamonds, aes(clarity)) + 
#'   geom_freqpoly(aes(group = cut, colour = cut))
#' 
#' # Dodged bar charts    
#' ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar(position="dodge")
#' # compare with 
#' ggplot(diamonds, aes(cut, fill=cut)) + geom_bar() + 
#'   facet_grid(. ~ clarity)
#' 
#' # But again, probably better to use frequency polygons instead:
#' ggplot(diamonds, aes(clarity, colour=cut)) + 
#'   geom_freqpoly(aes(group = cut))
#' 
#' # Often we don't want the height of the bar to represent the
#' # count of observations, but the sum of some other variable.
#' # For example, the following plot shows the number of diamonds
#' # of each colour
#' qplot(color, data=diamonds, geom="bar")
#' # If, however, we want to see the total number of carats in each colour
#' # we need to weight by the carat variable
#' qplot(color, data=diamonds, geom="bar", weight=carat, ylab="carat")
#' 
#' # A bar chart used to display means
#' meanprice <- tapply(diamonds$price, diamonds$cut, mean)
#' cut <- factor(levels(diamonds$cut), levels = levels(diamonds$cut))
#' qplot(cut, meanprice)
#' qplot(cut, meanprice, geom="bar", stat="identity")
#' qplot(cut, meanprice, geom="bar", stat="identity", fill = I("grey50"))
#'
#' # Another stacked bar chart example
#' k <- ggplot(mpg, aes(manufacturer, fill=class))
#' k + geom_bar()
#' # Use scales to change aesthetics defaults
#' k + geom_bar() + scale_fill_brewer()
#' k + geom_bar() + scale_fill_grey()
#'
#' # To change plot order of class varible
#' # use factor() to change order of levels
#' mpg$class <- factor(mpg$class, levels = c("midsize", "minivan", 
#' "suv", "compact", "2seater", "subcompact", "pickup"))
#' m <- ggplot(mpg, aes(manufacturer, fill=class))
#' m + geom_bar()
#' }
geom_bar <- function (mapping = NULL, data = NULL, stat = "bin", position = "stack", ...) {
  GeomBar$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomBar <- proto(Geom, {
  objname <- "bar"
  
  default_stat <- function(.) StatBin
  default_pos <- function(.) PositionStack
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, weight = 1, alpha = NA)
  
  required_aes <- c("x")
 
  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)
    transform(df,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  }
 
  draw_groups <- function(., data, scales, coordinates, ...) {
    GeomRect$draw_groups(data, scales, coordinates, ...)
  }
  guide_geom <- function(.) "polygon"
  
  
  icon <- function(.) {
    rectGrob(c(0.3, 0.7), c(0.4, 0.8), height=c(0.4, 0.8), width=0.3, vjust=1, gp=gpar(fill="grey20", col=NA))
  }
})
