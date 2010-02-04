# To time
# Turn numeric vector into POSIXct vector
# 
# @keyword internal
to_time <- function(x) structure(x, class = c("POSIXt", "POSIXct"))

ScaleDatetime <- proto(ScaleDate, {
  .major_seq <- NULL
  .minor_seq <- NULL
  tz <- NULL
  
  common <- c("x", "y")
  
  new <- function(., name=NULL, limits=NULL, major=NULL, minor=NULL, format=NULL, expand=c(0.05, 0), variable="x", tz = "") {
    
    trans <- Trans$find("datetime")
    limits <- trans$transform(limits)
    .$proto(name=name, .input=variable, .output=variable, 
      major_seq=major, minor_seq=minor, format=format, .expand = expand, 
      .tr=trans, limits = limits, tz=tz)
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
      day30  = start + round(runif(100, max = 30 * 86400)),
      day7  = start + round(runif(100, max = 7 * 86400)),
      day   = start + round(runif(100, max = 86400)),
      hour10 = start + round(runif(100, max = 10 * 3600)),
      hour5 = start + round(runif(100, max = 5 * 3600)),
      hour  = start + round(runif(100, max = 3600)),
      min10 = start + round(runif(100, max = 10 * 60)),
      min5  = start + round(runif(100, max = 5 * 60)),
      min   = start + round(runif(100, max = 60)),
      sec10 = start + round(runif(100, max = 10)),
      y = runif(100)
    )

    # Automatic scale selection
    qplot(sec10, y, data = df)
    qplot(min, y, data = df)
    qplot(min5, y, data = df)
    qplot(min10, y, data = df)
    qplot(hour, y, data = df)
    qplot(hour5, y, data = df)
    qplot(hour10, y, data = df)
    qplot(day, y, data = df)
    qplot(day30, y, data = df)
    
    # Manual scale selection
    qplot(day30, y, data = df)
    last_plot() + scale_x_datetime(major = "2 weeks")
    last_plot() + scale_x_datetime(major = "2 weeks", minor = "1 week")
    last_plot() + scale_x_datetime(major = "10 days")
    # See ?strptime for formatting parameters
    last_plot() + scale_x_datetime(major = "10 days", format = "%d/%m")
    
  }
  
})

