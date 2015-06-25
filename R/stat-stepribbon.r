## originally posted by jdnewmil @ https://groups.google.com/forum/#!msg/ggplot2/9cFWHaH1CPs/STwRwSn1v0kJ all credit goes to it.
#' Plot stepped ribbons
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "stepribbon")}
#'
#' @seealso \code{\link{geom_ribbon}}, \code{\link{geom_step}}
#' @inheritParams stat_identity
#' @param direction Character. Can be "hv" or "vh" for the initial direction of steps.
#' @return a data.frame with additional rows necessary to plot a step function
#' @export
#' @examples
#' \donttest{
#'  time <- 1:50
#'  prob <- sort(runif(50), decreasing=TRUE)
#'  df <- data.frame(time, prob, lci = prob - 0.5, uci = prob + 0.7)
#'
#'  ggplot(df, aes(x = time, y = prob, ymin = lci, ymax = uci)) +
#'  geom_step() +
#'  geom_ribbon(stat="stepribbon", alpha=0.3)
#' }
stat_stepribbon <- function( mapping = NULL, data = NULL, geom = "ribbon", position = "identity" ) {
  StatStepribbon$new(mapping = mapping, data = data, geom = geom, position = position)
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

  calculate <- function(., data, scales, direction = "hv", yvars = c("ymin", "ymax"), ...) {
      direction <- match.arg(direction, c("hv", "vh"))
      data <- as.data.frame(data)[order(data$x),]
      n <- nrow(data)

      if (direction == "vh") {
        xs <- rep(1:n, each = 2 )[-2*n]
        ys <- c(1, rep(2:n, each = 2))
      } else {
        ys <- rep(1:n, each = 2 )[-2*n]
        xs <- c(1, rep(2:n, each = 2))
      }

      data.frame(
        x = data$x[xs],
        data[ys, yvars, drop=FALSE],
        data[xs, setdiff(names(data), c("x", yvars)), drop=FALSE]
      )
    }
  }
)

