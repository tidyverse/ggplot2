## originally posted by jdnewmil @ https://groups.google.com/forum/#!msg/ggplot2/9cFWHaH1CPs/STwRwSn1v0kJ all credit goes to it.
#' Step ribbon statistic
#'
#' Allows for plotting stepped ribbons. Returns a modified data frame with intermediary points that allows lines to be drawn in a step manner.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "stepribbon")}
#'
#' @seealso \code{\link{geom_ribbon}}, \code{\link{geom_step}}
#' @param mapping The aesthetic mapping, usually constructed with aes or aes_string. Only needs to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override the plot defaults.
#' @param geom The geometric object to use display the data, defaults to ribbon.
#' @param position The position adjustment to use for overlappling points on this layer
#' @param direction Character. Can be "hv" or "vh" for the initial direction of steps.
#' @param additional arguments to be passed.
#' @return A data.frame with additional rows necessary to plot a step function
#' @export
#' @examples
#' \donttest{
#'  time <- 1:50
#'  prob <- sort(runif(50), decreasing=TRUE)
#'  data <- data.frame(time, prob, lci = prob - 0.5, uci = prob + 0.7)
#'
#'  ggplot(data, aes(x = time, y = prob, ymin = lci, ymax = uci)) +
#'  geom_step() +
#'  geom_ribbon(stat="stepribbon", alpha=0.3, direction="hv")
#'
#'  ggplot(data, aes(x = time, y = prob, ymin = lci, ymax = uci)) +
#'  stat_stepribbon(geom="ribbon", alpha=0.3, fill="red", direction="vh")
#' }
stat_stepribbon <- function( mapping = NULL, data = NULL, geom = "ribbon", position = "identity",  direction = c("hv", "vh"), ... ) {
  StatStepribbon$new(mapping = mapping, data = data, geom = geom, position = position, ...)
}

StatStepribbon <- proto(Stat, {
  objname <- "stepribbon"
  desc <- "Stepwise area plot"
  desc_outputs <- list(
    x = "stepped independent variable",
    ymin = "stepped minimum dependent variable",
    ymax = "stepped maximum dependent variable"
  )
  required_aes <- c("x", "ymin", "ymax")
  default_geom <- function(.) GeomRibbon
  default_aes <- function(.) aes(x = ..x.., ymin = ..y.., ymax = Inf)

  calculate <- function(., data, scales, yvars = c("ymin", "ymax"), direction = c("hv", "vh"), ...) {
      direction <- match.arg(direction, c("hv", "vh"))
      data <- as.data.frame(data)[order(data$x),]
      n <- nrow(data)
      if (direction == "vh") {
        xs <- rep(1:n, each = 2 )[-2*n]
        ys <- c(1, rep(2:n, each = 2))
      } else if (direction == "hv") {
        ys <- rep(1:n, each = 2 )[-2*n]
        xs <- c(1, rep(2:n, each = 2))
      } else stop("Direction must be one of hv or vh")
      data.frame(
        x = data$x[xs],
        data[ys, yvars, drop=FALSE],
        data[xs, setdiff(names(data), c("x", yvars)), drop=FALSE]
      )
    }
  }
)

