# Scales object encapsultes multiple scales.
# All input and output done with data.frames to facilitate
# multiple input and output variables

#' @importFrom methods setRefClass
Scales <- setRefClass("Scales", fields = "scales", methods = list(
  find = function(aesthetic) {
    vapply(scales, function(x) any(aesthetic %in% x$aesthetics), logical(1))
  },
  has_scale = function(aesthetic) {
    any(find(aesthetic))
  },
  add = function(scale) {
    prev_aes <- find(scale$aesthetics)
    if (any(prev_aes)) {
      # Get only the first aesthetic name in the returned vector -- it can
      # sometimes be c("x", "xmin", "xmax", ....)
      scalename <- scales[prev_aes][[1]]$aesthetics[1]
      message("Scale for '", scalename,
        "' is already present. Adding another scale for '", scalename,
        "', which will replace the existing scale.")
    }

    # Remove old scale for this aesthetic (if it exists)
    scales <<- c(scales[!prev_aes], list(scale))
  },
  clone = function() {
    new_scales <- lapply(scales, scale_clone)
    Scales$new(new_scales)
  },
  n = function() {
    length(scales)
  },
  input = function() {
    unlist(lapply(scales, "[[", "aesthetics"))
  },
  initialize = function(scales = NULL) {
    initFields(scales = scales)
  },
  non_position_scales = function(.) {
    Scales$new(scales[!find("x") & !find("y")])
  },
  get_scales = function(output) {
    scale <- scales[find(output)]
    if (length(scale) == 0) return()
    scale[[1]]
  }
))

# Train scale from a data frame
scales_train_df <- function(scales, df, drop = FALSE) {
  if (empty(df) || length(scales$scales) == 0) return()

  lapply(scales$scales, scale_train_df, df = df)
}

# Map values from a data.frame. Returns data.frame
scales_map_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0) return(df)

  mapped <- unlist(lapply(scales$scales, scale_map_df, df = df), recursive = FALSE)

  quickdf(c(mapped, df[setdiff(names(df), names(mapped))]))
}

# Transform values to cardinal representation
scales_transform_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0) return(df)

  transformed <- unlist(lapply(scales$scales, scale_transform_df, df = df),
    recursive = FALSE)
  quickdf(c(transformed, df[setdiff(names(df), names(transformed))]))
}

# @param aesthetics A list of aesthetic-variable mappings. The name of each
#   item is the aesthetic, and the value of each item is the variable in data.
scales_add_defaults <- function(scales, data, aesthetics, env) {
  if (is.null(aesthetics)) return()
  names(aesthetics) <- unlist(lapply(names(aesthetics), aes_to_scale))

  new_aesthetics <- setdiff(names(aesthetics), scales$input())
  # No new aesthetics, so no new scales to add
  if (is.null(new_aesthetics)) return()

  datacols <- tryapply(
    aesthetics[new_aesthetics], eval,
    envir = data, enclos = env
  )

  for(aes in names(datacols)) {
    type <- scale_type(datacols[[aes]])
    scale_name <- paste("scale", aes, type, sep="_")

    # Skip aesthetics with no scales (e.g. group, order, etc)
    scale_f <- find_global(scale_name, env, mode = "function")
    if (is.null(scale_f)) next

    scales$add(scale_f())
  }

}

# Add missing but required scales.
# @param aesthetics A character vector of aesthetics. Typically c("x", "y").
scales_add_missing <- function(plot, aesthetics, env) {

  # Keep only aesthetics that aren't already in plot$scales
  aesthetics <- setdiff(aesthetics, plot$scales$input())

  for (aes in aesthetics) {
    scale_name <- paste("scale", aes, "continuous", sep="_")

    scale_f <- find_global(scale_name, env, mode = "function")
    plot$scales$add(scale_f())
  }
}


# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}


# Determine default type of a scale
scale_type <- function(x) UseMethod("scale_type")

#' @export
scale_type.default <- function(x) {
  message("Don't know how to automatically pick scale for object of type ",
    paste(class(x), collapse = "/"), ". Defaulting to continuous")
  "continuous"
}

#' @export
scale_type.logical <- function(x) "discrete"

#' @export
scale_type.character <- function(x) "discrete"

#' @export
scale_type.factor <- function(x) "discrete"

#' @export
scale_type.POSIXt <- function(x) "datetime"

#' @export
scale_type.Date <- function(x) "date"

#' @export
scale_type.numeric <- function(x) "continuous"
