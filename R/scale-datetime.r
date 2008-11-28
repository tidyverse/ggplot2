to_time <- function(x) structure(x, class = c("POSIXt", "POSIXct"))
TransDatetime <- Trans$new("datetime", "as.numeric", "to_time")

ScaleDatetime <- proto(ScaleDate, {
  .major_seq <- NULL
  .minor_seq <- NULL
  tz <- NULL
  
  common <- c("x", "y")
  
  new <- function(., name=NULL, limits=NULL, major=NULL, minor=NULL, format=NULL, variable="x", tz = "") {
    
    trans <- Trans$find("datetime")
    limits <- trans$transform(limits)
    .$proto(.input=variable, .output=variable, major_seq=major, minor_seq=minor, format=format, name=name, .tr=trans, limits = limits, tz=tz)
  }
  
  break_points <- function(.) {
    auto <- time_breaks(diff(range(.$input_set()))) 
    c(
      .$major_seq %||% auto$major,
      .$minor_seq %||% auto$minor,
      .$format %||% auto$format
    )
  }

  input_breaks <- function(.) {
    d <- to_time(.$input_set())
    as.numeric(fullseq_time(d, .$break_points()[1]))
  }
  
  output_breaks <- function(., n) {
    d <- to_time(.$input_set())
    as.numeric(fullseq_time(d, .$break_points()[2]))
  }
  
  labels <- function(.) {
    breaks <- .$.tr$inverse(.$input_breaks())
    attr(breaks, "tzone") <- .$tz
    format(breaks, .$break_points()[3])
  }

  # Documentation -----------------------------------------------

  objname <- "datetime"
  desc <- "Position scale, date time"
  
  icon <- function(.) {
    textGrob("14/10/1979\n10:14am", gp=gpar(cex=0.9))
  }

  examples <- function(.) {
    start <- ISOdate(2001, 1, 1, tz = "")
    df <- data.frame(
      day7  = start + round(runif(100, max = 7 * 86400)),
      day   = start + round(runif(100, max = 86400)),
      hour5 = start + round(runif(100, max = 5 * 3600)),
      hour  = start + round(runif(100, max = 3600)),
      min10 = start + round(runif(100, max = 10 * 60)),
      min5  = start + round(runif(100, max = 5 * 60)),
      min   = start + round(runif(100, max = 60)),
      sec10 = start + round(runif(100, max = 10)),
      y = runif(100)
    )

    qplot(sec10, y, data = df)
    qplot(min, y, data = df)
    qplot(min5, y, data = df)
    qplot(min10, y, data = df)
    qplot(hour, y, data = df)
    qplot(hour5, y, data = df)
    qplot(day, y, data = df)
    qplot(day7, y, data = df)
  }
  
})

