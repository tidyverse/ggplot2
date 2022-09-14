#' Control aesthetic evaluation
#'
#' Most aesthetics are mapped from variables found in the data. Sometimes,
#' however, you want to delay the mapping until later in the rendering process.
#' ggplot2 has three stages of the data that you can map aesthetics from. The
#' default is to map at the beginning, using the layer data provided by the
#' user. The second stage is after the data has been transformed by the layer
#' stat. The third and last stage is after the data has been transformed and
#' mapped by the plot scales. The most common example of mapping from stat
#' transformed data is the height of bars in [geom_histogram()]:
#' the height does not come from a variable in the underlying data, but
#' is instead mapped to the `count` computed by [stat_bin()]. An example of
#' mapping from scaled data could be to use a desaturated version of the stroke
#' colour for fill. If you want to map directly from the layer data you should
#' not do anything special. In order to map from stat transformed data you
#' should use the `after_stat()` function to flag that evaluation of the
#' aesthetic mapping should be postponed until after stat transformation.
#' Similarly, you should use `after_scale()` to flag evaluation of mapping for
#' after data has been scaled. If you want to map the same aesthetic multiple
#' times, e.g. map `x` to a data column for the stat, but remap it for the geom,
#' you can use the `stage()` function to collect multiple mappings.
#'
#' `after_stat()` replaces the old approaches of using either `stat()` or
#' surrounding the variable names with `..`.
#'
#' @note Evaluation after stat transformation will have access to the
#' variables calculated by the stat, not the original mapped values. Evaluation
#' after scaling will only have access to the final aesthetics of the layer
#' (including non-mapped, default aesthetics). The original layer data can only
#' be accessed at the first stage.
#'
#' @param x An aesthetic expression using variables calculated by the stat
#'   (`after_stat()`) or layer aesthetics (`after_scale()`).
#' @param start An aesthetic expression using variables from the layer data.
#' @param after_stat An aesthetic expression using variables calculated by the
#'   stat.
#' @param after_scale An aesthetic expression using layer aesthetics.
#'
#' @rdname aes_eval
#' @name aes_eval
#'
#' @examples
#' # Default histogram display
#' ggplot(mpg, aes(displ)) +
#'   geom_histogram(aes(y = after_stat(count)))
#'
#' # Scale tallest bin to 1
#' ggplot(mpg, aes(displ)) +
#'   geom_histogram(aes(y = after_stat(count / max(count))))
#'
#' # Use a transparent version of colour for fill
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot(aes(colour = class, fill = after_scale(alpha(colour, 0.4))))
#'
#' # Use stage to modify the scaled fill
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot(aes(fill = stage(class, after_scale = alpha(fill, 0.4))))
NULL

#' @rdname aes_eval
#' @export
after_stat <- function(x) {
  x
}
#' @rdname aes_eval
#' @usage NULL
#' @export
stat <- function(x) {
  x
}
#' @rdname aes_eval
#' @export
after_scale <- function(x) {
  x
}
#' @rdname aes_eval
#' @export
stage <- function(start = NULL, after_stat = NULL, after_scale = NULL) {
  start
}
stage_calculated <- function(start = NULL, after_stat = NULL, after_scale = NULL) {
  after_stat
}
stage_scaled <- function(start = NULL, after_stat = NULL, after_scale = NULL) {
  after_scale
}

# Regex to determine if an identifier refers to a calculated aesthetic
match_calculated_aes <- "^\\.\\.([a-zA-Z._]+)\\.\\.$"

is_dotted_var <- function(x) {
  grepl(match_calculated_aes, x)
}

# Determine if aesthetic is calculated
is_calculated_aes <- function(aesthetics, warn = FALSE) {
  vapply(aesthetics, is_calculated, warn = warn, logical(1), USE.NAMES = FALSE)
}
is_scaled_aes <- function(aesthetics) {
  vapply(aesthetics, is_scaled, logical(1), USE.NAMES = FALSE)
}
is_staged_aes <- function(aesthetics) {
  vapply(aesthetics, is_staged, logical(1), USE.NAMES = FALSE)
}
is_calculated <- function(x, warn = FALSE) {
  if (is_call(get_expr(x), "after_stat")) {
    return(TRUE)
  }
  # Support of old recursive behaviour
  if (is.atomic(x)) {
    FALSE
  } else if (is.symbol(x)) {
    res <- is_dotted_var(as.character(x))
    if (res && warn) {
      what <- I(glue("The dot-dot notation (`{x}`)"))
      var <- gsub(match_calculated_aes, "\\1", as.character(x))
      with <- I(glue("`after_stat({var})`"))
      lifecycle::deprecate_warn("3.4.0", what, with, id = "ggplot-warn-aes-dot-dot")
    }
    res
  } else if (is_quosure(x)) {
    is_calculated(quo_get_expr(x), warn = warn)
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(stat))) {
      if (warn) {
        what <- I(glue("`{expr_deparse(x)}`"))
        x[[1]] <- quote(after_stat)
        with <- I(glue("`{expr_deparse(x)}`"))
        lifecycle::deprecate_warn("3.4.0", what, with, id = "ggplot-warn-aes-stat")
      }
      TRUE
    } else {
      any(vapply(x, is_calculated, warn = warn, logical(1)))
    }
  } else if (is.pairlist(x)) {
    FALSE
  } else {
    cli::cli_abort("Unknown input: {.cls {class(x)[1]}}")
  }
}
is_scaled <- function(x) {
  is_call(get_expr(x), "after_scale")
}
is_staged <- function(x) {
  is_call(get_expr(x), "stage")
}

# Strip dots from expressions
strip_dots <- function(expr, env, strip_pronoun = FALSE) {
  if (is.atomic(expr)) {
    expr
  } else if (is.name(expr)) {
    expr_ch <- as.character(expr)
    if (nchar(expr_ch) > 0) {
      as.name(gsub(match_calculated_aes, "\\1", expr_ch))
    } else {
      expr
    }
  } else if (is_quosure(expr)) {
    # strip dots from quosure and reconstruct the quosure
    new_quosure(
      strip_dots(quo_get_expr(expr), env = quo_get_env(expr), strip_pronoun = strip_pronoun),
      quo_get_env(expr)
    )
  } else if (is.call(expr)) {
    if (strip_pronoun && is_call(expr, "$") && is_symbol(expr[[2]], ".data")) {
      strip_dots(expr[[3]], env, strip_pronoun = strip_pronoun)
    } else if (strip_pronoun && is_call(expr, "[[") && is_symbol(expr[[2]], ".data")) {
      tryCatch(
        sym(eval(expr[[3]], env)),
        error = function(e) expr[[3]]
      )
    } else if (is_call(expr, "stat")) {
      strip_dots(expr[[2]], env, strip_pronoun = strip_pronoun)
    } else {
      expr[-1] <- lapply(expr[-1], strip_dots, env = env, strip_pronoun = strip_pronoun)
      expr
    }
  } else if (is.pairlist(expr)) {
    # In the unlikely event of an anonymous function
    as.pairlist(lapply(expr, strip_dots, env = env, strip_pronoun = strip_pronoun))
  } else if (is.list(expr)) {
    # For list of aesthetics
    lapply(expr, strip_dots, env = env, strip_pronoun = strip_pronoun)
  } else {
    cli::cli_abort("Unknown input: {.cls {class(expr)[1]}}")
  }
}

strip_stage <- function(expr) {
  uq_expr <- get_expr(expr)
  if (is_call(uq_expr, c("after_stat", "after_scale"))) {
    uq_expr[[2]]
  } else if (is_call(uq_expr, "stage")) {
    # Prefer stat mapping if present, otherwise original mapping (fallback to
    # scale mapping) but there should always be two arguments to stage()
    uq_expr$after_stat %||% uq_expr$start %||% (if (is.null(uq_expr$after_scale)) uq_expr[[3]]) %||% uq_expr[[2]]
  } else {
    expr
  }
}

# Convert aesthetic mapping into text labels
make_labels <- function(mapping) {
  default_label <- function(aesthetic, mapping) {
    # e.g., geom_smooth(aes(colour = "loess")) or aes(y = NULL)
    if (is.atomic(mapping)) {
      return(aesthetic)
    }
    mapping <- strip_stage(mapping)
    mapping <- strip_dots(mapping, strip_pronoun = TRUE)
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
