#' Calculated aesthetics
#'
#' Most aesthetics are mapped from variables found in the data. Sometimes,
#' however, you want to map from variables computed by the aesthetic. The
#' most common example of this is the height of bars in [geom_histogram()]:
#' the height does not come from a variable in the underlying data, but
#' is instead mapped to the `count` computed by [stat_bin()]. The `stat()`
#' function is a flag to ggplot2 to it that you want to use calculated
#' aesthetics produced by the statistic.
#'
#' This replaces the older approach of surrounding the variable name with
#' `..`.
#'
#' @export
#' @param x An aesthetic expression using variables calculated by the stat.
#' @examples
#' # Default histogram display
#' ggplot(mpg, aes(displ)) +
#'   geom_histogram(aes(y = stat(count)))
#'
#' # Scale tallest bin to 1
#' ggplot(mpg, aes(displ)) +
#'   geom_histogram(aes(y = stat(count / max(count))))
stat <- function(x) {
  x
}

# Regex to determine if an identifier refers to a calculated aesthetic
match_calculated_aes <- "^\\.\\.([a-zA-Z._]+)\\.\\.$"

is_dotted_var <- function(x) {
  grepl(match_calculated_aes, x)
}

# Determine if aesthetic is calculated
is_calculated_aes <- function(aesthetics) {
  vapply(aesthetics, is_calculated, logical(1), USE.NAMES = FALSE)
}

is_calculated <- function(x) {
  if (is.atomic(x)) {
    FALSE
  } else if (is.symbol(x)) {
    is_dotted_var(as.character(x))
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(stat))) {
      TRUE
    } else {
      any(vapply(x, is_calculated, logical(1)))
    }
  } else if (is.pairlist(x)) {
    FALSE
  } else {
    stop("Unknown input:", class(x)[1])
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
    if (identical(expr[[1]], quote(stat))) {
      strip_dots(expr[[2]])
    } else {
      expr[-1] <- lapply(expr[-1], strip_dots)
      expr
    }
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

# Convert aesthetic mapping into text labels
make_labels <- function(mapping) {
  default_label <- function(aesthetic, mapping) {
    # e.g., geom_smooth(aes(colour = "loess"))
    if (is.atomic(mapping)) {
      aesthetic
    } else {
      x <- rlang::quo_text(strip_dots(mapping))
      if (length(x) > 1) {
        x <- paste0(x[[1]], "...")
      }
      x
    }
  }
  Map(default_label, names(mapping), mapping)
}
