# Time breaks
# Automatically compute sensible axis breaks for time data
# 
# @arguments range in seconds
# @keyword internal
time_breaks <- function(seconds) {
  days <- seconds / 86400
  if (days > 5) {
    return(date_breaks(days))
  }
  
  # seconds, minutes, hours, days
  length <- cut(seconds, c(0, 60, 3600, 24 * 3600, Inf) * 1.1, labels=FALSE)
  duration <- c(1, 60, 3600, 24 * 3600)
  units <- round(seconds / duration[length])
  
  major_mult <- ceiling(diff(pretty(c(0, units)))[1])
  minor_mult <- ceiling(diff(pretty(c(0, units), n = 15))[1])
  major <-  c("sec", "min",   "hour",  "day")[length]  
  format <-  c("%S", "%M.%S", "%H:%M", "%d-%b")[length]

  list(
    major = paste(major_mult, major), 
    minor = paste(minor_mult, major), 
    format = format
  )
  
}

# Date breaks
# Automatically compute sensible axis breaks for date data
# 
# @arguments range in days
# @keyword internal
date_breaks <- function(days) {
  length <- cut(days, c(0, 10, 56, 365, 730, 5000, Inf), labels=FALSE)

  major <- 
    c("days", "weeks", "months", "3 months", "years", "5 years")[length]
  minor <- 
    c("10 years", "days", "weeks", "months", "months", "years")[length]
  format <- 
    c("%d-%b", "%d-%b", "%b-%y", "%b-%y", "%Y", "%Y")[length]

  list(major = major, minor = minor, format = format)
}

