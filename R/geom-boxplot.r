#' Box and whiskers plot.
#'
#' @seealso \code{\link{stat_quantile}} to view quantiles conditioned on a
#'   continuous variable,  \code{\link{geom_jitter}} for another way to look 
#'   at conditional distributions"
#' @param outlier.colour colour for outlying points
#' @param outlier.shape shape of outlying points
#' @param outlier.size size of outlying points
#' @export
#' @examples
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
geom_boxplot <- function (mapping = NULL, data = NULL, stat = "boxplot", position = "dodge", 
outlier.colour = "black", outlier.shape = 16, outlier.size = 2, ...) {
  GeomBoxplot$new(mapping = mapping, data = data, stat = stat, 
  position = position, outlier.colour = outlier.colour, outlier.shape = outlier.shape, 
  outlier.size = outlier.size, ...)
}

GeomBoxplot <- proto(Geom, {
  objname <- "boxplot"

  reparameterise <- function(., df, params) {
    df$width <- df$width %||% 
      params$width %||% (resolution(df$x, FALSE) * 0.9)
    df$ymin_final <- min(df$outliers[[1]])
    df$ymax_final <- max(df$outliers[[1]])
    transform(df,
      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )

  }
  
  draw <- function(., data, ..., fatten = 2, outlier.colour = NULL, outlier.shape = NULL, outlier.size = 2) { 
    common <- data.frame(
      colour = data$colour, 
      size = data$size, 
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),  
      group = 1, 
      stringsAsFactors = FALSE
    )

    whiskers <- data.frame(
      x = data$x,
      xend = data$x, 
      y = c(data$upper, data$lower), 
      yend = c(data$ymax, data$ymin),
      alpha = 1,
      common)

    box <- data.frame(
      xmin = data$xmin, 
      xmax = data$xmax, 
      ymin = data$lower, 
      y = data$middle, 
      ymax = data$upper,
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
        alpha = 1,
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
    gp <- with(data, gpar(col=colour, fill=fill, lwd=size * .pt))
    gTree(gp = gp, children = gList(
      linesGrob(0.5, c(0.1, 0.9)),
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
  default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5, alpha = 1, shape = 16, linetype = "solid")
  required_aes <- c("x", "lower", "upper", "middle", "ymin", "ymax")

})
