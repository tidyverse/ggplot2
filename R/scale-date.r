#time <- ScaleTime$new(major="months", minor="weeks")

# For an time axis (ie. given two dates indicating the start and end of the time series), you want to be able to specify:
# 
#    * the interval between major and minor ticks. A string (second, minute, hour, day, week, month, quarter, year + all plurals) posibly including multiplier (usually integer, always positive) giving the interval between ticks.  
# 
#   * the position of the first tick, as a date/time.  This should default to a round number of intervals, before first the data point if necessary.
# 
#   * format string which controls how the date is printed (should default to displaying just enough to distinguish each interval).
#
#   * threshold for displaying first/last tick mark outside the data: if the last date point is > threshold * interval (default to 0.9)

ScaleDate <- proto(ScaleContinuous,{
  .major_seq <- NULL
  .minor_seq <- NULL
  
  common <- c("x", "y")
  
  new <- function(., name=NULL, limits=NULL, major=NULL, minor=NULL, format=NULL, variable="x") {
    
    trans <- Trans$find("date")
    limits <- trans$transform(limits)
    
    .$proto(.input=variable, .output=variable, major_seq=major, minor_seq=minor, format=format, name=name, .tr=trans, limits = limits)
  }
  
  train <- function(., values) {
    .$.domain <- range(c(values, .$.domain), na.rm=TRUE)
  }
  
  break_points <- function(.) {
    rng <- diff(range(.$input_set()))
    
    length <- cut(rng, c(0, 10, 56, 500, 5000, Inf), labels=FALSE)
    
    minor <- nulldefault(.$minor_seq, c("10 years", "days", "weeks", "months", "years")[length])
    major <- nulldefault(.$major_seq, c("days", "weeks", "months", "years", "5 years")[length])
    format <- nulldefault(.$format, c("%d-%b", "%d-%b", "%b-%y", "%Y", "%Y")[length])
    
    c(major, minor, format)
  }
  
  input_breaks <- function(.) {
    d <- to_date(.$input_set())
    
    
    
    .$.tr$transform(seq(d[1], d[2], by=.$break_points()[1]))
  }
  input_breaks_n <- function(.) as.numeric(.$input_breaks())
  
  output_breaks <- function(., n) {
    d <- structure(.$input_set(), class="Date")
    .$.tr$transform(seq(d[1], d[2], by=.$break_points()[1]))
  }
  
  
  labels <- function(.) {
    format(.$.tr$inverse(.$input_breaks()), .$break_points()[3])
  }

  # Documentation -----------------------------------------------

  objname <- "date"
  desc <- "Position scale, date"
  
  icon <- function(.) {
    textGrob("14/10/1979", gp=gpar(cex=1))
  }

  details <- "<p>Currently somewhat broken due to lack of support for dates in R.</p>"

  examples <- function(.) {
    # We'll start by creating some nonsense data with dates
    df <- data.frame(
      date = seq(Sys.Date(), len=100, by="1 day")[sample(100, 50)],
      price = runif(50)
    )
    df <- df[order(df$date), ]
    dt <- qplot(date, price, data=df, geom="line")
    dt$aspect.ratio <- 1/4
    
    # We can control the format of the labels, and the frequency of 
    # the major and minor tickmarks.  See ?format.Date and ?seq.Date 
    # for more details.
    dt + scale_x_date()
    dt + scale_x_date(format="%m/%d")
    dt + scale_x_date(format="%W")
    dt + scale_x_date(major="months", minor="weeks", format="%b")
    dt + scale_x_date(major="months", minor="2 days", format="%b")
    dt + scale_x_date(major="years", format="%b-%Y")
    
    # The date scale will attempt to pick sensible defaults for 
    # major and minor tick marks
    qplot(date, price, data=df[1:10,], geom="line")
    qplot(date, price, data=df[1:4,], geom="line")

    df <- data.frame(
      date = seq(Sys.Date(), len=1000, by="1 day"),
      price = runif(500)
    )
    qplot(date, price, data=df, geom="line")
    
    # A real example using economic time series data
    qplot(date, psavert, data=economics) 
    qplot(date, psavert, data=economics, geom="path") 
    
    qplot(date, psavert, data=economics, geom="path", xlim=c(as.Date("2000-1-1"),NA) )
    qplot(date, psavert, data=economics, geom="path", xlim=c(as.Date("2005-1-1"),NA) )
    qplot(date, psavert, data=economics, geom="path", xlim=c(as.Date("2007-1-1"),NA) )
    # cf
    qplot(date, psavert, data=subset(economics, date > "2000-1-1"), geom="path")
    qplot(date, psavert, data=subset(economics, date > "2005-1-1"), geom="path")
    qplot(date, psavert, data=subset(economics, date > "2006-1-1"), geom="path")
  }
  
})

