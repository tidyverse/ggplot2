
#' Modify fill transparency
#'
#' This works much like [alpha()][scales::alpha] in that it modifies the
#' transparency of fill colours. It differs in that `fill_alpha()` also attempts
#' to set the transparency of `<GridPattern>` objects.
#'
#' @param fill A fill colour given as a `character` or `integer` vector, or as a
#'   (list of) `<GridPattern>` object(s).
#' @param alpha A transparency value between 0 (transparent) and 1 (opaque),
#'   parallel to `fill`.
#'
#' @return A `character` vector of colours, or list of `<GridPattern>` objects.
#' @export
#' @keywords internal
#'
#' @examples
#' # Typical colour input
#' fill_alpha("red", 0.5)
#'
#' if (utils::packageVersion("grid") > "4.2") {
#'   # Pattern input
#'   fill_alpha(list(grid::linearGradient()), 0.5)
#' }
fill_alpha <- function(fill, alpha) {
  if (!is.list(fill)) {
    # Happy path for no patterns
    return(alpha(fill, alpha))
  }
  if (is_pattern(fill) || any(vapply(fill, is_pattern, logical(1)))) {
    check_device("patterns", action = "warn")
    fill <- pattern_alpha(fill, alpha)
    return(fill)
  } else {
    # We are either dealing with faulty fill specification, or we have a legend
    # key that is trying to draw a single colour. It can be given that colour
    # as a list due to patterns in other keys.
    msg <- paste0(
      "{.field fill} must be a vector of colours or list of ",
      "{.cls GridPattern} objects."
    )
    # If single colour list, try applying `alpha()`
    fill <- try_fetch(
      Map(alpha, colour = fill, alpha = alpha),
      error = function(cnd) {
        cli::cli_abort(msg, call = expr(fill_alpha()))
      }
    )
    # `length(input)` must be same as `length(output)`
    if (!all(lengths(fill) == 1)) {
      cli::cli_abort(msg)
    }
    return(unlist(fill))
  }
}

# Similar to grid:::is.pattern
is_pattern <- function(x) {
  inherits(x, "GridPattern")
}

#' Modify transparency for patterns
#'
#' This generic allows you to add your own methods for adding transparency to
#' pattern-like objects.
#'
#' @param x Object to be interpreted as pattern.
#' @param alpha A `numeric` vector between 0 and 1. If `NA`, alpha values
#'   are preserved.
#'
#' @return `x` with modified transparency
#' @export
#' @keywords internal
pattern_alpha <- function(x, alpha) {
  UseMethod("pattern_alpha")
}

#' @export
pattern_alpha.default <- function(x, alpha) {
  if (!is.atomic(x)) {
    cli::cli_abort("Can't apply {.arg alpha} to {obj_type_friendly(x)}.")
  }
  pattern(rectGrob(), gp = gpar(fill = alpha(x, alpha)))
}

#' @export
pattern_alpha.GridPattern <- function(x, alpha) {
  x$colours <- alpha(x$colours, alpha[1])
  x
}

#' @export
pattern_alpha.GridTilingPattern <- function(x, alpha) {
  if (all(is.na(alpha) | alpha == 1)) {
    return(x)
  }
  check_device("alpha_masks", "warn")
  grob <- env_get(environment(x$f), "grob")
  mask <- as.mask(rectGrob(gp = gpar(fill = alpha("white", alpha))))
  if (is.null(grob$vp)) {
    grob$vp <- viewport(mask = mask)
  } else {
    grob$vp <- editViewport(grob$vp, mask = mask)
  }
  new_env <- new.env(parent = environment(x$f))
  env_bind(new_env, grob = grob)
  environment(x$f) <- new_env
  x
}

#' @export
pattern_alpha.list <- function(x, alpha) {
  Map(pattern_alpha, x = x, alpha = alpha)
}

