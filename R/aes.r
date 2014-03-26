# all_aes <- function(y) c(names(y$default_aes()), y$required_aes)
# geom_aes <- unlist(lapply(Geom$find_all(), all_aes))
# stat_aes <- unlist(lapply(Stat$find_all(), all_aes))
# all <- sort(unique(c(names(.base_to_ggplot), geom_aes, stat_aes)))
# dput(all)

.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower", "lty", "lwd", "max", "middle", "min", "order", "pch", "radius", "sample", "shape", "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax", "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z")


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

#' Generate aesthetic mappings that describe how variables in the data are
#' mapped to visual properties (aesthetics) of geoms.
#'
#' \code{aes} creates a list of unevaluated expressions.  This function also
#' performs partial name matching, converts color to colour, and old style R
#' names to ggplot names (eg. pch to shape, cex to size)
#'
#' @param x,y,... List of name value pairs giving aesthetics to map.
#' @family aesthetic generators
#' @seealso See
#'    \code{\link{aes_colour_fill_alpha}}, \code{\link{aes_group_order}},
#'    \code{\link{aes_linetype_size_shape}} and \code{\link{aes_position}}
#'    for more specific examples with different aesthetics.
#' @export
#' @examples
#' aes(x = mpg, y = wt)
#' aes(x = mpg ^ 2, y = wt / cyl)
aes <- function(x, y, ...) {
  aes <- structure(as.list(match.call()[-1]), class="uneval")
  rename_aes(aes)
}
#' @export
print.uneval <- function(x, ...) str(unclass(x))
#' @export
str.uneval <- function(object, ...) str(unclass(object), ...)
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

  rename(x, .base_to_ggplot, warn_missing = FALSE)
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

#' Generate aesthetic mappings from a string/quoted objects
#'
#' Aesthetic mappings describe how variables in the data are mapped to visual
#' properties (aesthetics) of geoms. \code{\link{aes}} uses non-standard
#' evaluation to capture the variable names. These two variants use
#' regular evaluation, which is easier to use inside functions.
#'
#' \code{aes_string} and \code{aes_q} are particularly useful when writing
#' functions that create plots because you can use strings or quoted
#' names/calls to define the aesthetic mappings, rather than having to use
#' \code{\link{substitute}} to generate a call to \code{aes()}.
#'
#' @param x,y,... List of name value pairs
#' @family aesthetic generators
#' @seealso \code{\link{aes}}
#' @export
#' @examples
#' # Threee ways of generating the same aesthetics
#' aes(mpg, wt, col = cyl, fill = NULL)
#' aes_string("mpg", "wt", col = "cyl", fill = NULL)
#' aes_q(quote(mpg), quote(wt), col = quote(cyl), fill = NULL)
#'
#' aes(col = cyl, fill = NULL)
#' aes_string(col = "cyl", fill = NULL)
#' aes_q(col = quote(cyl), fill = NULL)
aes_string <- function(x = NULL, y = NULL, ...) {
  mapping <- c(compact(list(x = x, y = y)), list(...))
  mapping[vapply(mapping, is.null, logical(1))] <- "NULL"

  parsed <- lapply(mapping, function(x) parse(text = x)[[1]])
  structure(rename_aes(parsed), class = "uneval")
}

#' @rdname aes_string
#' @export
aes_q <- function(x = NULL, y = NULL, ...) {
  mapping <- c(compact(list(x = x, y = y)), list(...))
  structure(rename_aes(mapping), class = "uneval")
}

#' Given a character vector, create a set of identity mappings
#'
#' @param vars vector of variable names
#' @export
#' @examples
#' aes_all(names(mtcars))
#' aes_all(c("x", "y", "col", "pch"))
aes_all <- function(vars) {
  names(vars) <- vars
  vars <- rename_aes(vars)

  structure(
    lapply(vars, function(x) parse(text=x)[[1]]),
    class = "uneval"
  )
}

#' Automatic aesthetic mapping
#'
#' @param data data.frame or names of variables
#' @param ... aesthetics that need to be explicitly mapped.
#' @export
#' @examples
#' df <- data.frame(x = 1, y = 1, colour = 1, label = 1, pch = 1)
#' aes_auto(df)
#' aes_auto(names(df))
#'
#' df <- data.frame(xp = 1, y = 1, colour = 1, txt = 1, foo = 1)
#' aes_auto(df, x = xp, label = txt)
#' aes_auto(names(df), x = xp, label = txt)
#'
#' df <- data.frame(foo = 1:3)
#' aes_auto(df, x = xp, y = yp)
#' aes_auto(df)
aes_auto <- function(data = NULL, ...) {
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
  aes <- lapply(vars, function(x) parse(text=x)[[1]])

  # explicitly defined aes
  if (length(match.call()) > 2) {
    args <- as.list(match.call()[-1])
    aes <- c(aes, args[names(args) != "data"])
  }

  structure(rename_aes(aes), class = "uneval")
}

# Aesthetic defaults
# Convenience method for setting aesthetic defaults
#
# @param data values from aesthetic mappings
# @param y. defaults
# @param params. user specified values
# @value a data.frame, with all factors converted to character strings
aesdefaults <- function(data, y., params.) {
  updated <- modifyList(y., params. %||% list())

  cols <- tryapply(defaults(data, updated), function(x) eval(x, data, globalenv()))

  # Need to be careful here because stat_boxplot uses a list-column to store
  # a vector of outliers
  cols <- Filter(function(x) is.atomic(x) || is.list(x), cols)
  list_vars <- sapply(cols, is.list)
  cols[list_vars] <- lapply(cols[list_vars], I)

  df <- data.frame(cols, stringsAsFactors = FALSE)

  factors <- sapply(df, is.factor)
  df[factors] <- lapply(df[factors], as.character)
  df
}
