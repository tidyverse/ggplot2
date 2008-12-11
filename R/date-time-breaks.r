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
  
  length <- cut(seconds, c(0, 10, 60, 600, 3600, 5 * 3600, 24 * 3600, Inf), labels=FALSE)
  
  major <- 
    c("2 sec", "15 sec", "1 min",  "30 min", "1 hour", "6 hour")[length]
  minor <- 
    c("1 sec", "5 sec",  "30 sec", "10 min", "30 min", "3 hour")[length]
  format <- 
    c("%S",    "%S",     "%S",     "%M %S",  "%l:%M",  "%l:%M")[length]

  list(major = major, minor = minor, format = format)
  
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

