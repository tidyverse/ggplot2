stat_km <- function (mapping = NULL, data = NULL, geom = "km", position = "identity", ...) {
  StatKm$new(mapping = mapping, data = data, geom = geom, position = position, ...)
}

StatKm <- proto(Stat, {
  objname <- "km"

  calculate <- function(., data, scales, mark.time = TRUE, ...) {

    try_require("survival")
    sf <- survfit(Surv(data$x, data$status) ~ 1, ...)
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
