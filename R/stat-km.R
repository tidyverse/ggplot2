#' Kaplan Meier Estimate of Survival
#'
#' @inheritParams stat_identity
#' @param se display confidence interval around KM curve? (TRUE by default, use
#'   \code{conf.int} to control significance level which is 0.95 by default)
#' @param ... Other arguments passed to \link{survival::survfit.formula}
#' @return a data.frame with additional columns: \item{x}{x in data}
#'   \item{y}{Kaplan-Meier Survival Estimate at x} \item{ymin}{Lower confidence
#'   limit of KM curve, if \code{se = TRUE}} \item{ymax}{Upper confidence limit
#'   of KM curve, if \code{se = FALSE}}
#' @export
#' @examples
#' \donttest{
#' ggplot(survival::lung, aes(time, status, color = sex)) + stat_km()
#' qplot(survival::lung, time, status = status, geom = "km")
#' }

stat_km <- function (mapping = NULL, data = NULL, se = TRUE, geom = "km", position = "identity", ...) {
  StatKm$new(mapping = mapping, data = data, se = se, geom = geom, position = position, ...)
}

StatKm <- proto(Stat, {
  objname <- "km"

  calculate <- function(., data, scales, se, ...) {

    try_require("survival")
    sf <- survfit(Surv(data$x, data$status) ~ 1, se.fit = se, ...)
    x <- sf$time
    y <- sf$surv

    se <- all(exists("upper", where = sf), exists("lower", where = sf))

    df.out <- data.frame(x = x, y = y)
    if(se){
      df.out <- cbind(df.out, ymin = sf$lower, ymax = sf$upper)
    }
    df.out

  }

  default_aes <- function(.) aes(y = ..y..)
  required_aes <- c("x", "status")
  default_geom <- function(.) GeomKm

})
