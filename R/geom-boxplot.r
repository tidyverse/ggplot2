#' Box and whiskers plot.
#'
#' The upper and lower "hinges" correspond to the first and third quartiles
#' (the 25th and 7th percentiles). This differs slightly from the method used
#' by the \code{boxplot} function, and may be apparent with small samples.
#' See \code{\link{boxplot.stats}} for for more information on how hinge
#' positions are calculated for \code{boxplot}.
#'
#' The upper whisker extends from the hinge to the highest value that is within
#' 1.5 * IQR of the hinge, where IQR is the inter-quartile range, or distance
#' between the first and third quartiles. The lower whisker extends from the
#' hinge to the lowest value within 1.5 * IQR of the hinge. Data beyond the
#' end of the whiskers are outliers and plotted as points (as specified by Tukey).
#'
#' In a notched box plot, the notches extend \code{1.58 * IQR / sqrt(n)}.
#' This gives a roughly 95% confidence interval for comparing medians.
#' See McGill et al. (1978) for more details.
#'
#' @seealso \code{\link{stat_quantile}} to view quantiles conditioned on a
#'   continuous variable,  \code{\link{geom_jitter}} for another way to look 
#'   at conditional distributions"
#' @inheritParams geom_point
#' @param outlier.colour colour for outlying points
#' @param outlier.shape shape of outlying points
#' @param outlier.size size of outlying points
#' @param notch if \code{FALSE} (default) make a standard box plot. If
#'    \code{TRUE}, make a notched box plot. Notches are used to compare groups;
#'    if the notches of two boxes do not overlap, this is strong evidence that 
#'    the medians differ.
#' @param notchwidth for a notched box plot, width of the notch relative to
#'    the body (default 0.5)
#' @export
#'
#' @references McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of
#'     box plots. The American Statistician 32, 12-16.
#'
#' @examples
#' \donttest{
#' p <- ggplot(mtcars, aes(factor(cyl), mpg))
#' 
#' p + geom_boxplot()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot")
#' 
#' p + geom_boxplot() + geom_jitter()
#' p + geom_boxplot() + coord_flip()
#' qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot") +
#'   coord_flip()
#'
#' p + geom_boxplot(notch = TRUE)
#' p + geom_boxplot(notch = TRUE, notchwidth = .3)
#' 
#' p + geom_boxplot(outlier.colour = "green", outlier.size = 3)
#' 
#' # Add aesthetic mappings
#' # Note that boxplots are automatically dodged when any aesthetic is 
#' # a factor
#' p + geom_boxplot(aes(fill = cyl))
#' p + geom_boxplot(aes(fill = factor(cyl)))
#' p + geom_boxplot(aes(fill = factor(vs)))
#' p + geom_boxplot(aes(fill = factor(am)))
#' 
#' # Set aesthetics to fixed value
#' p + geom_boxplot(fill = "grey80", colour = "#3366FF")
#' qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot", 
#'   colour = I("#3366FF"))
#' 
#' # Scales vs. coordinate transforms -------
#' # Scale transformations occur before the boxplot statistics are computed.
#' # Coordinate transformations occur afterwards.  Observe the effect on the
#' # number of outliers.
#' library(plyr) # to access round_any
#' m <- ggplot(movies, aes(y = votes, x = rating,
#'    group = round_any(rating, 0.5)))
#' m + geom_boxplot()
#' m + geom_boxplot() + scale_y_log10()
#' m + geom_boxplot() + coord_trans(y = "log10")
#' m + geom_boxplot() + scale_y_log10() + coord_trans(y = "log10")
#' 
#' # Boxplots with continuous x:
#' # Use the group aesthetic to group observations in boxplots
#' qplot(year, budget, data = movies, geom = "boxplot")
#' qplot(year, budget, data = movies, geom = "boxplot", 
#'   group = round_any(year, 10, floor))
#'
#' # Using precomputed statistics
#' # generate sample data
#' abc <- adply(matrix(rnorm(100), ncol = 5), 2, quantile, c(0, .25, .5, .75, 1))
#' b <- ggplot(abc, aes(x = X1, ymin = `0%`, lower = `25%`, middle = `50%`, upper = `75%`, ymax = `100%`)) 
#' b + geom_boxplot(stat = "identity")
#' b + geom_boxplot(stat = "identity") + coord_flip()
#' b + geom_boxplot(aes(fill = X1), stat = "identity")
#' }
geom_boxplot <- function (mapping = NULL, data = NULL, stat = "boxplot", position = "dodge", 
outlier.colour = "black", outlier.shape = 16, outlier.size = 2,
notch = FALSE, notchwidth = .5, ...) {
  GeomBoxplot$new(mapping = mapping, data = data, stat = stat, 
  position = position, outlier.colour = outlier.colour, outlier.shape = outlier.shape, 
  outlier.size = outlier.size, notch = notch, notchwidth = notchwidth, ...)
}

GeomBoxplot <- proto(Geom, {
  objname <- "boxplot"

  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    if (!is.null(df$outliers)) {
      suppressWarnings({
        out_min <- vapply(df$outliers, min, numeric(1))
        out_max <- vapply(df$outliers, max, numeric(1))
      })
      
      df$ymin_final <- pmin(out_min, df$ymin)
      df$ymax_final <- pmax(out_max, df$ymax)
    } 
    
    transform(df,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )

  }
  
  draw <- function(., data, ..., fatten = 2, outlier.colour = NULL, outlier.shape = NULL, outlier.size = 2,
                   notch = FALSE, notchwidth = .5) { 
    common <- data.frame(
      colour = data$colour, 
      size = data$size, 
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),  
      group = NA, 
      stringsAsFactors = FALSE
    )

    whiskers <- data.frame(
      x = data$x,
      xend = data$x, 
      y = c(data$upper, data$lower), 
      yend = c(data$ymax, data$ymin),
      alpha = NA,
      common)

    box <- data.frame(
      xmin = data$xmin, 
      xmax = data$xmax, 
      ymin = data$lower, 
      y = data$middle, 
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha, 
      common)
    
    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- data.frame(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        fill = NA,
        alpha = NA,
        stringsAsFactors = FALSE)
      outliers_grob <- GeomPoint$draw(outliers, ...)
    } else {
      outliers_grob <- NULL
    }
    
    ggname(.$my_name(), grobTree(
      outliers_grob,
      GeomSegment$draw(whiskers, ...),
      GeomCrossbar$draw(box, fatten = fatten, ...)
    ))
  }

  guide_geom <- function(.) "boxplot"  
  draw_legend <- function(., data, ...)  {
    data <- aesdefaults(data, .$default_aes(), list(...))
    gp <- with(data, gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt))
    gTree(gp = gp, children = gList(
      linesGrob(0.5, c(0.1, 0.25)),
      linesGrob(0.5, c(0.75, 0.9)),
      rectGrob(height=0.5, width=0.75),
      linesGrob(c(0.125, 0.875), 0.5)
    ))
  }
  icon <- function(.) {
    gTree(children=gList(
      segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
      rectGrob(c(0.3, 0.7), c(0.6, 0.8), width=0.3, height=c(0.4, 0.4), vjust=1),
      segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
    ))
  }
  
  default_stat <- function(.) StatBoxplot
  default_pos <- function(.) PositionDodge
  default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5, alpha = NA, shape = 16, linetype = "solid")
  required_aes <- c("x", "lower", "upper", "middle", "ymin", "ymax")

})
