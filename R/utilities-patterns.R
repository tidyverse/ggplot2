
#' Modify fill transparency
#'
#' This works much like [alpha()][scales::alpha] in that it modifies the
#' transparency of fill colours. It differs in that is also attempts to set
#' the transparency of `<GridPattern>` objects.
#'
#' @param fill A fill colour given as a `character` or `integer` vector, or as a
#'   list of `<GridPattern>` objects.
#' @param alpha A transparency value between 0 (transparent) and 1 (opaque),
#'   parallel to `fill`.
#'
#' @return A `character` vector of colours, or list of `<GridPattern>` objects.
#' @export
#'
#' @examples
#' # Typical colour input
#' fill_alpha("red", 0.5)
#'
#' # Pattern input
#' fill_alpha(list(linearGradient()), 0.5)
fill_alpha <- function(fill, alpha) {
  if (!is.list(fill)) {
    # Happy path for no patterns
    return(alpha(fill, alpha))
  }
  if (any(vapply(fill, is_pattern, logical(1)))) {
    if (utils::packageVersion("grid") < "4.2") {
      # Pattern fills were introduced in R 4.1.0, but *vectorised* patterns
      # were only introduced in R 4.2.0.
      cli::cli_abort(
        "Using patterns in {.pkg ggplot2} requires at least R version 4.2.0."
      )
    }
    fill <- pattern_alpha(fill, alpha)
    return(fill)
  } else {
    # We are either dealing with faulty fill specification, or we have a legend
    # key that is trying to draw a single colour. It can be given that colour
    # as a list due to patterns in other keys.
    msg <- paste0(
      "{.field fill} must be a valid colour or list of ",
      "{.cls GridPattern} objects."
    )
    # If single colour list, try applying `alpha()`
    fill <- try_fetch(
      Map(alpha, colour = fill, alpha = alpha),
      error = function(cnd) {
        cli::cli_abort(msg)
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

# Function that applies alpha to <GridPattern> objects.
# For linear or radial gradients, this is as simple as modifying their `colours`
# slot with an alpha.
# For tiled patterns, we attach an alpha mask in the grobs' viewport.
pattern_alpha <- function(x, alpha) {
  if (!is.list(x)) {
    # If this is a plain colour, convert to pattern because grid doesn't accept
    # mixed patterns and plain colours.
    out <- pattern(rectGrob(), gp = gpar(fill = alpha(x, alpha)))
    return(out)
  }
  if (!is_pattern(x)) {
    out <- Map(pattern_alpha, x = x, alpha = alpha)
    return(out)
  }
  if (inherits(x, c("GridLinearGradient", "GridRadialGradient"))) {
    # Apply alpha to gradient colours
    x$colours <- alpha(x$colours, alpha[1])
    return(x)
  }
  no_alpha <- is.na(alpha[1]) || alpha[1] == 1
  if (inherits(x, "GridTilingPattern") && !no_alpha) {
    # Dig out the grob from the function environment
    grob <- env_get(environment(x$f), "grob")
    # Apply a mask in the grob's viewport
    mask <- as.mask(rectGrob(gp = gpar(fill = alpha("black", alpha[1]))))
    if (is.null(grob$vp)) {
      grob$vp <- viewport(mask = mask)
    } else {
      grob$vp$mask <- mask
    }
    # Re-attach new function environment
    new_env <- new.env(parent = environment(x$f))
    env_bind(new_env, grob = grob)
    environment(x$f) <- new_env
  }
  return(x)
}
