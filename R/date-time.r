# Floor for dates
# Round date up to nearest multiple of time
# 
# @arguments date to round
# @arguments unit of time to round to (see \code{\link{cut.Date}}) for valid values
# @keywords internal
floor_date <- function(date, time) {
  prec <- parse_unit_spec(time)
  if (prec$unit == "day") {
    structure(round_any(as.numeric(date), prec$mult), class="Date")
  } else {
    as.Date(cut(date, time))
  }
}

# Parse date time unit specification
# Parse the time unit specification used by \code{\link{cut.Date}} into something useful
# 
# @keywords internal
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

# Ceiling for dates
# Round date down to nearest multiple of time
# 
# @arguments date to round
# @arguments unit of time to round to (see \code{\link{cut.Date}}) for valid values
# @keywords internal
ceiling_date <- function(date, time) { 
  prec <- parse_unit_spec(time)
  
  up <- c("day" = 1, "week" = 7, "month" = 31, "year" = 365)
  date <- date + prec$mult * up[prec$unit]
  
  floor_date(date, time)
}

# Fullseq for dates
# Analog of \code{link{fullseq}}, but for dates
# 
# Use in \code{\link{scale_date}}
# 
# @arguments range of dates
# @argument unit of time to round to
# @keywords internal
fullseq_date <- function(range, time) {
  seq.Date(
    floor_date(range[1], time), 
    ceiling_date(range[2], time), 
    by=time
  )
}
