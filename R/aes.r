#' @include utilities.r
NULL

.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color",
  "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower",
  "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape",
  "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax",
  "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")

.base_to_ggplot <- c(
  "col"   = "colour",
  "color" = "colour",
  "pch"   = "shape",
  "cex"   = "size",
  "lty"   = "linetype",
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin",
  "max"   = "ymax"
)

#' Construct aesthetic mappings
#'
#' Aesthetic mappings describe how variables in the data are mapped to visual
#' properties (aesthetics) of geoms. Aesthetic mappings can be set in
#' [ggplot2()] and in individual layers.
#'
#' This function also standardise aesthetic names by performing partial
#' matching, converting color to colour, and translating old style R names to
#' ggplot names (eg. pch to shape, cex to size)
#'
#' @param x,y,... List of name value pairs giving aesthetics to map to
#'   variables. The names for x and y aesthetics are typically omitted because
#'   they are so common; all other aesthetics must be named.
#' @seealso See [aes_()] for a version of `aes` that is
#'   more suitable for programming with.
#' @export
#' @examples
#' aes(x = mpg, y = wt)
#' aes(mpg, wt)
#'
#' # You can also map aesthetics to functions of variables
#' aes(x = mpg ^ 2, y = wt / cyl)
#'
#' # Aesthetic names are automatically standardised
#' aes(col = x)
#' aes(fg = x)
#' aes(color = x)
#' aes(colour = x)
#'
#' # aes is almost always used with ggplot() or a layer
#' ggplot(mpg, aes(displ, hwy)) + geom_point()
#' ggplot(mpg) + geom_point(aes(displ, hwy))
#'
#' # Aesthetics supplied to ggplot() are used as defaults for every layer
#' # you can override them, or supply different aesthetics for each layer
aes <- function(x, y, ...) {
  aes <- structure(as.list(match.call()[-1]), class = "uneval")
  rename_aes(aes)
}
#' @export
print.uneval <- function(x, ...) {
  values <- vapply(x, deparse2, character(1))
  bullets <- paste0("* ", format(names(x)), " -> ", values, "\n")

  cat(bullets, sep = "")
}

#' @export
str.uneval <- function(object, ...) utils::str(unclass(object), ...)
#' @export
"[.uneval" <- function(x, i, ...) structure(unclass(x)[i], class = "uneval")

#' @export
as.character.uneval <- function(x, ...) {
  char <- as.character(unclass(x))
  names(char) <- names(x)
  char
}

# Rename American or old-style aesthetics name
rename_aes <- function(x) {
  # Convert prefixes to full names
  full <- match(names(x), .all_aesthetics)
  names(x)[!is.na(full)] <- .all_aesthetics[full[!is.na(full)]]

  plyr::rename(x, .base_to_ggplot, warn_missing = FALSE)
}

# Look up the scale that should be used for a given aesthetic
aes_to_scale <- function(var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"

  var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  aes_to_scale(vars) %in% c("x", "y")
}

#' Define aesthetic mappings programatically
#'
#' Aesthetic mappings describe how variables in the data are mapped to visual
#' properties (aesthetics) of geoms. [aes()] uses non-standard
#' evaluation to capture the variable names. `aes_` and `aes_string`
#' require you to explicitly quote the inputs either with `""` for
#' `aes_string()`, or with `quote` or `~` for `aes_()`.
#' (`aes_q` is an alias to `aes_`). This makes `aes_` and
#' `aes_string` easy to program with.
#'
#' `aes_string` and `aes_` are particularly useful when writing
#' functions that create plots because you can use strings or quoted
#' names/calls to define the aesthetic mappings, rather than having to use
#' [substitute()] to generate a call to `aes()`.
#'
#' I recommend using `aes_()`, because creating the equivalents of
#' `aes(colour = "my colour")` or \code{aes{x = `X$1`}}
#' with `aes_string()` is quite clunky.
#'
#' @param x,y,... List of name value pairs. Elements must be either
#'   quoted calls, strings, one-sided formulas or constants.
#' @seealso [aes()]
#' @export
#' @examples
#' # Three ways of generating the same aesthetics
#' aes(mpg, wt, col = cyl)
#' aes_(quote(mpg), quote(wt), col = quote(cyl))
#' aes_(~mpg, ~wt, col = ~cyl)
#' aes_string("mpg", "wt", col = "cyl")
#'
#' # You can't easily mimic these calls with aes_string
#' aes(`$100`, colour = "smooth")
#' aes_(~ `$100`, colour = "smooth")
#' # Ok, you can, but it requires a _lot_ of quotes
#' aes_string("`$100`", colour = '"smooth"')
#'
#' # Convert strings to names with as.name
#' var <- "cyl"
#' aes(col = x)
#' aes_(col = as.name(var))
aes_ <- function(x, y, ...) {
  mapping <- list(...)
  if (!missing(x)) mapping["x"] <- list(x)
  if (!missing(y)) mapping["y"] <- list(y)

  as_call <- function(x) {
    if (is.formula(x) && length(x) == 2) {
      x[[2]]
    } else if (is.call(x) || is.name(x) || is.atomic(x)) {
      x
    } else {
      stop("Aesthetic must be a one-sided formula, call, name, or constant.",
        call. = FALSE)
    }
  }
  mapping <- lapply(mapping, as_call)
  structure(rename_aes(mapping), class = "uneval")
}

#' @rdname aes_
#' @export
aes_string <- function(x, y, ...) {
  mapping <- list(...)
  if (!missing(x)) mapping["x"] <- list(x)
  if (!missing(y)) mapping["y"] <- list(y)

  mapping <- lapply(mapping, function(x) {
    if (is.character(x)) {
      parse(text = x)[[1]]
    } else {
      x
    }
  })
  structure(rename_aes(mapping), class = "uneval")
}

#' @export
#' @rdname aes_
aes_q <- aes_

#' Given a character vector, create a set of identity mappings
#'
#' @param vars vector of variable names
#' @keywords internal
#' @export
#' @examples
#' aes_all(names(mtcars))
#' aes_all(c("x", "y", "col", "pch"))
aes_all <- function(vars) {
  names(vars) <- vars
  vars <- rename_aes(vars)

  structure(
    lapply(vars, as.name),
    class = "uneval"
  )
}

#' Automatic aesthetic mapping
#'
#' @param data data.frame or names of variables
#' @param ... aesthetics that need to be explicitly mapped.
#' @keywords internal
#' @export
aes_auto <- function(data = NULL, ...) {
  warning("aes_auto() is deprecated", call. = FALSE)

  # detect names of data
  if (is.null(data)) {
    stop("aes_auto requires data.frame or names of data.frame.")
  } else if (is.data.frame(data)) {
    vars <- names(data)
  } else {
    vars <- data
  }

  # automatically detected aes
  vars <- intersect(.all_aesthetics, vars)
  names(vars) <- vars
  aes <- lapply(vars, function(x) parse(text = x)[[1]])

  # explicitly defined aes
  if (length(match.call()) > 2) {
    args <- as.list(match.call()[-1])
    aes <- c(aes, args[names(args) != "data"])
  }

  structure(rename_aes(aes), class = "uneval")
}

mapped_aesthetics <- function(x) {
  is_null <- vapply(x, is.null, logical(1))
  names(x)[!is_null]

}
