# Regex to determine if an identifier refers to a calculated aesthetic
match_calculated_aes <- "^\\.\\.([a-zA-Z._]+)\\.\\.$"

# Determine if aesthetic is calculated
is_calculated_aes <- function(aesthetics) {
  vars <- lapply(aesthetics, find_vars)

  vapply(vars, function(x) any(grepl(match_calculated_aes, x)), logical(1))
}

find_vars <- function(expr) {
  if (is.name(expr)) {
    as.character(expr)
  } else if (is.atomic(expr)) {
    character()
  } else if (is.call(expr)) {
    unlist(lapply(expr[-1], find_vars))
  } else if (is.pairlist(expr)) {
    # In the unlikely event of an anonymous function
    unlist(lapply(expr, find_vars))
  } else {
    stop("Unknown input:", class(expr)[1])
  }
}

# Strip dots from expressions
strip_dots <- function(expr) {
  if (is.atomic(expr)) {
    expr
  } else if (is.name(expr)) {
    expr_ch <- as.character(expr)
    if (nchar(expr_ch) > 0) {
      as.name(gsub(match_calculated_aes, "\\1", expr_ch))
    } else {
      expr
    }
  } else if (is.call(expr)) {
    expr[-1] <- lapply(expr[-1], strip_dots)
    expr
  } else if (is.pairlist(expr)) {
    # In the unlikely event of an anonymous function
    as.pairlist(lapply(expr, strip_dots))
  } else if (is.list(expr)) {
    # For list of aesthetics
    lapply(expr, strip_dots)
  } else {
    stop("Unknown input:", class(expr)[1])
  }
}

