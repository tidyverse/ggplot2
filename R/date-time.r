floor_date <- function(date, time) {
  prec <- parse_unit_spec(time)
  if (prec$unit == "day") {
    structure(round_any(as.numeric(date), prec$mult), class="Date")
  } else {
    as.Date(cut(date, time))
  }
}

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

ceiling_date <- function(date, time) { 
  prec <- parse_unit_spec(time)
  
  up <- c("day" = 1, "week" = 7, "month" = 31, "year" = 365)
  date <- date + prec$mult * up[prec$unit]
  
  floor_date(date, time)
}

fullseq_date <- function(range, time) {
  seq.Date(
    floor_date(range[1], time), 
    ceiling_date(range[2], time), 
    by=time
  )
}
