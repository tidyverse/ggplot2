# Floor for dates and times
# Round date up to nearest multiple of time
# 
# @arguments date to round
# @arguments unit of time to round to (see \code{\link{cut.Date}}) for valid values
# @keyword internal
# @alias floor_time
floor_date <- function(date, time) {
  prec <- parse_unit_spec(time)
  if (prec$unit == "day") {
    structure(round_any(as.numeric(date), prec$mult), class="Date")
  } else {
    as.Date(cut(date, time, right = TRUE, include.lowest = TRUE))
  }
}
floor_time <- function(date, time) {
  prec <- parse_unit_spec(time)
  if (prec$unit == "sec") {
    to_time(round_any(as.numeric(date), prec$mult))
  } else if (prec$unit == "min") {
    to_time(round_any(as.numeric(date), prec$mult * 60))    
  } else {
    as.POSIXct(
      cut(date, time, right = TRUE, include.lowest = TRUE), 
      tz = attr(date, "tz") %||% ""
    )  
  }
}

# Parse date time unit specification
# Parse the time unit specification used by \code{\link{cut.Date}} into something useful
# 
# @keyword internal
parse_unit_spec <- function(unitspec) {
  parts <- strsplit(unitspec, " ")[[1]]
  if (length(parts) == 1) {
    mult <- 1
    unit <- unitspec
  } else {
    mult <- as.numeric(parts[[1]])
    unit <- parts[[2]]
  }
  unit <- gsub("s$", "", unit)
  
  list(unit = unit, mult = mult)
}

# Ceiling for dates and times
# Round date down to nearest multiple of time
# 
# @arguments date to round
# @arguments unit of time to round to (see \code{\link{cut.Date}}) for valid values
# @keyword internal
# @alias ceiling_time
ceiling_date <- function(date, time) { 
  prec <- parse_unit_spec(time)
  
  up <- c("day" = 1, "week" = 7, "month" = 31, "year" = 365)
  date <- date + prec$mult * up[prec$unit]
  
  floor_date(date, time)
}

ceiling_time <- function(date, time) { 
  prec <- parse_unit_spec(time)
  
  up <- c(
    "sec" = 1, "min" = 60, "hour" = 3600, 
    c("day" = 1, "week" = 7, "month" = 31, "year" = 365) * 3600 * 24
  )
  date <- date + prec$mult * up[prec$unit]
  
  floor_time(date, time)
}

# Fullseq for dates and times
# Analog of \code{link{fullseq}}, but for dates and times
# 
# Use in \code{\link{scale_date}}
# 
# @arguments range of dates
# @argument unit of time to round to
# @keyword internal
# @alias fullseq_time
fullseq_date <- function(range, time) {
  seq.Date(
    floor_date(range[1], time), 
    ceiling_date(range[2], time), 
    by=time
  )
}
fullseq_time <- function(range, time) {
  seq.POSIXt(
    floor_time(range[1], time),
    ceiling_time(range[2], time),
    by=time
  )
}
