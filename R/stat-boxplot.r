#' Calculate components of box and whisker plot.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "boxplot")}
#'
#' @param coef length of the whiskers as multiple of IQR.  Defaults to 1.5
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams stat_identity
#' @return A data frame with additional columns:
#'   \item{width}{width of boxplot}
#'   \item{ymin}{lower whisker = smallest observation greater than or equal to lower hinge - 1.5 * IQR}
#'   \item{lower}{lower hinge, 25\% quantile}
#'   \item{notchlower}{lower edge of notch = median - 1.58 * IQR / sqrt(n)}
#'   \item{middle}{median, 50\% quantile}
#'   \item{notchupper}{upper edge of notch = median + 1.58 * IQR / sqrt(n)}
#'   \item{upper}{upper hinge, 75\% quantile}
#'   \item{ymax}{upper whisker = largest observation less than or equal to upper hinge + 1.5 * IQR}
#' @seealso See \code{\link{geom_boxplot}} for examples.
#' @export
#' @examples
#' # See geom_boxplot for examples
stat_boxplot <- function (mapping = NULL, data = NULL, geom = "boxplot", position = "dodge",
na.rm = FALSE, coef = 1.5, ...) {
  StatBoxplot$new(mapping = mapping, data = data, geom = geom,
  position = position, na.rm = na.rm, coef = coef, ...)
}

StatBoxplot <- proto(Stat, {
  objname <- "boxplot"

  required_aes <- c("x", "y")
  default_geom <- function(.) GeomBoxplot

  calculate_groups <- function(., data, na.rm = FALSE, width = NULL, ...) {
    data <- remove_missing(data, na.rm, c("y", "weight"), name="stat_boxplot",
      finite = TRUE)
    data$weight <- data$weight %||% 1
    width <- width %||%  resolution(data$x) * 0.75

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
      if (any(outliers)) {
        stats[c(1, 5)] <- range(c(stats[2:4], y[!outliers]), na.rm=TRUE)
      }

      if (length(unique(x)) > 1) width <- diff(range(x)) * 0.9

      df <- as.data.frame(as.list(stats))
      df$outliers <- I(list(y[outliers]))

      if (is.null(weight)) {
        n <- sum(!is.na(y))
      } else {
        # Sum up weights for non-NA positions of y and weight
        n <- sum(weight[!is.na(y) & !is.na(weight)])
      }

      df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
      df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

      transform(df,
        x = if (is.factor(x)) x[1] else mean(range(x)),
        width = width,
        relvarwidth = sqrt(n)
      )
    })
  }
})


#' @rdname stat_boxplot
#' @export
stat_boxploth <- function (mapping = NULL, data = NULL, geom = "boxploth", position = "dodgeh", na.rm = FALSE, coef = 1.5, ...) {
  StatBoxploth$new(mapping = mapping, data = data, geom = geom, position = position, na.rm = na.rm, coef = coef, ...)
}

StatBoxploth <- proto(Stat, {
  objname <- "boxploth"

  required_aes <- c("x", "y")
  default_geom <- function(.) GeomBoxploth

  calculate_groups <- function(., data, na.rm = FALSE, height = NULL, ...) {
    data <- remove_missing(data, na.rm, c("x", "weight"), name = "stat_boxploth",
      finite = TRUE)
    data$weight <- data$weight %||% 1
    height <- height %||%  resolution(data$y) * 0.75

    .super$calculate_groups(., data, na.rm = na.rm, height = height, ...)
  }

  calculate <- function(., data, scales, height = NULL, na.rm = FALSE, coef = 1.5, ...) {
    with(data, {
      qs <- c(0, 0.25, 0.5, 0.75, 1)
      if (length(unique(weight)) != 1) {
        try_require("quantreg")
        stats <- as.numeric(coef(rq(x ~ 1, weights = weight, tau = qs)))
      } else {
        stats <- as.numeric(quantile(x, qs))
      }
      names(stats) <- c("xmin", "lower", "middle", "upper", "xmax")

      iqr <- diff(stats[c(2, 4)])

      outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
      if (any(outliers)) {
        stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
      }

      if (length(unique(y)) > 1) height <- diff(range(y)) * 0.9

      df <- as.data.frame(as.list(stats))
      df$outliers <- I(list(x[outliers]))

      if (is.null(weight)) {
        n <- sum(!is.na(x))
      } else {
        # Sum up weights for non-NA positions of y and weight
        n <- sum(weight[!is.na(x) & !is.na(weight)])
      }

      df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
      df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

      transform(df,
        y = if (is.factor(y)) y[1] else mean(range(y)),
        height = height,
        relvarheight = sqrt(n)
      )
    })
  }
})
