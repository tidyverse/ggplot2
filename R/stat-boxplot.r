#' Calculate components of box and whisker plot.
#' 
#' @name stat_boxplot
#' @return A data frame with additional columns:
#'   \item{width}{width of boxplot}
#'   \item{ymin}{lower whisker = lower hinge - 1.5 * IQR}
#'   \item{lower}{lower hinge, 25% quantile} 
#'   \item{middle}{median, 50% quantile}
#'   \item{upper}{upper hinge, 75% quantile}
#'   \item{ymax}{upper whisker = upper hinge + 1.5 * IQR}
#' @export
#' @examples
#' # See geom_boxplot for examples
StatBoxplot <- proto(Stat, {
  objname <- "boxplot"
  
  required_aes <- c("x", "y")
  icon <- function(.) GeomBoxplot$icon()
  default_geom <- function(.) GeomBoxplot
  
  calculate_groups <- function(., data, na.rm = FALSE, width = NULL, ...) {
    data <- remove_missing(data, na.rm, c("y", "weight"), name="stat_boxplot")
    data$weight <- nulldefault(data$weight, 1)
    width <- nulldefault(width, resolution(data$x) * 0.75)
        
    .super$calculate_groups(., data, na.rm = na.rm, width = width, ...)
  }
  
  calculate <- function(., data, scales, width=NULL, na.rm = FALSE, coef = 1.5, ...) {
    with(data, {    
      qs <- c(0, 0.25, 0.5, 0.75, 1)
      if (length(unique(weight)) != 1) {
        try_require("quantreg")
        stats <- as.numeric(coef(rq(y ~ 1, weights = weight, tau=qs)))
      } else {
        stats <- as.numeric(quantile(y, qs))
      }
      names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    
      iqr <- diff(stats[c(2, 4)])
      
      outliers <- y < (stats[2] - coef * iqr) | y > (stats[4] + coef * iqr)
      if (any(outliers)) stats[c(1, 5)] <- range(y[!outliers], na.rm=TRUE)
      
      if (length(unique(x)) > 1) width <- diff(range(x)) * 0.9
    
      df <- as.data.frame(as.list(stats))
      df$outliers <- I(list(y[outliers]))

      transform(df,
        x = if (is.factor(x)) x[1] else mean(range(x)),
        width = width
      )
    })
  }
})
