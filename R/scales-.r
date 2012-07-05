# Scales object encapsultes multiple scales.
# All input and output done with data.frames to facilitate 
# multiple input and output variables

Scales <- setRefClass("Scales", fields = "scales", methods = list(
  find = function(aesthetic) {
    vapply(scales, function(x) any(aesthetic %in% x$aesthetics), logical(1))
  },
  has_scale = function(aesthetic) {
    any(find(aesthetic))
  },
  add = function(scale) {
    # Remove old scale for this aesthetic (if it exists)
    scales <<- c(scales[!find(scale$aesthetics)], list(scale))
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
  if (empty(df) || length(scales$scales) == 0) return()
  
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
  new_aesthetics <- intersect(new_aesthetics, names(datacols))
  if (length(datacols) == 0) return()

  for(aes in new_aesthetics) {
    type <- scale_type(datacols[[aes]])
    scale_name <- paste("scale", aes, type, sep="_")

    # Skip aesthetics with no scales (e.g. group, order, etc)
    scale_f <- find_global(scale_name)
    if (is.null(scale_f)) next

    scales$add(scale_f())
  }
  
}

# Look for object first in global environment and if not found, then in 
# ggplot2 package environment.  This makes it possible to override default
# scales by setting them in the default environment.
find_global <- function(name) {
  if (exists(name, globalenv())) {
    return(get(name, globalenv()))
  }

  # If loaded with load_all, envname == "package:ggplot2"
  # If installed and loaded in package, envname == "ggplot2"
  # Look in ggplot2 package only if ggplot2 is installed and loaded.
  # This is because getNamespace() always loads the installed package,
  # which can cause conflicts when developing with load_all().
  if (environmentName(parent.env(environment())) == "ggplot2") {
    pkg <- getNamespace("ggplot2")
    if (exists(name, pkg)) {
      return(get(name, pkg))
    }
  }
  
  NULL
}


# Determine default type of a scale
scale_type <- function(x) UseMethod("scale_type")

#' @S3method scale_type default
scale_type.default <- function(x) {
  message("Don't know how to automatically pick scale for object of type ",
    paste(class(x), collapse = "/"), ". Defaulting to continuous")
  "continuous"
}

#' @S3method scale_type logical
scale_type.logical <- function(x) "discrete"

#' @S3method scale_type character
scale_type.character <- function(x) "discrete"

#' @S3method scale_type factor
scale_type.factor <- function(x) "discrete"

#' @S3method scale_type POSIXt
scale_type.POSIXt <- function(x) "datetime"

#' @S3method scale_type Date
scale_type.Date <- function(x) "date"

#' @S3method scale_type numeric
scale_type.numeric <- function(x) "continuous"
