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
#' @seealso [mapped()] for marking aesthetics for evaluation after values has
#' been mapped to the scale.
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
#' Mapped aesthetics
#'
#' Usually you will want the mapping of aesthetics to be based on input data
#' but sometimes, e.g. for colour and fill, you'd want one aesthetic to be a
#' function of another. Using the `mapped()` function you can mark an aesthetic
#' to only be evaluated after all other aesthetics have been calculated and
#' mapped. This allows you to e.g. set fill to be a transparent verison of ¨
#' colour.
#'
#' @param x An aesthetic expression using only variables available after
#'   mapping.
#'
#' @note Using `mapped()` for an aesthetic will not create a guide for that
#' aesthetic, as it essentially a function of one of the other guides.
#'
#' @seealso [stat()] for marking aesthetics for evaluation after statistical
#' transformation
#' @export
#' @examples
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot(aes(colour = class, fill = mapped(alpha(colour, 0.4))))
#'
mapped <- function(x) {
  x
}
#' @export
stage <- function(stat = NULL, geom = NULL, mapped = NULL) {
  stat
}
stage_geom <- function(stat = NULL, geom = NULL, mapped = NULL) {
  geom
}
stage_mapped <- function(stat = NULL, geom = NULL, mapped = NULL) {
  mapped
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
is_mapped_aes <- function(aesthetics) {
  vapply(aesthetics, is_mapped, logical(1), USE.NAMES = FALSE)
}
is_stage_aes <- function(aesthetics) {
  vapply(aesthetics, is_stage, logical(1), USE.NAMES = FALSE)
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
is_mapped <- function(x) {
  if (is.atomic(x)) {
    FALSE
  } else if (is.symbol(x)) {
    FALSE
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(mapped))) {
      TRUE
    } else {
      any(vapply(x, is_mapped, logical(1)))
    }
  } else if (is.pairlist(x)) {
    FALSE
  } else {
    stop("Unknown input:", class(x)[1])
  }
}
is_stage <- function(x) {
  if (is.atomic(x)) {
    FALSE
  } else if (is.symbol(x)) {
    FALSE
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(stage))) {
      TRUE
    } else {
      any(vapply(x, is_stage, logical(1)))
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
    # e.g., geom_smooth(aes(colour = "loess")) or aes(y = NULL)
    if (is.atomic(mapping)) {
      return(aesthetic)
    }

    mapping <- strip_dots(mapping)
    if (is_quosure(mapping) && quo_is_symbol(mapping)) {
      name <- as_string(quo_get_expr(mapping))
    } else {
      name <- quo_text(mapping)
      name <- gsub("\n.*$", "...", name)
    }
    name
  }
  Map(default_label, names(mapping), mapping)
}
