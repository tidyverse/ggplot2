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
    prev_aes <- find(scale$aesthetics)
    if (any(prev_aes)) {
      # Get only the first aesthetic name in the returned vector -- it can
      # sometimes be c("x", "xmin", "xmax", ....)
      prevscale <- scales[prev_aes][[1]]
      scalename <- prevscale$aesthetics[1]

      if(scale$clear){
        message("Scale for '", scalename, "' is present but will be fully overriden by the new scale.")
      }else{
        scale = merge.scales(scale,scalename,prevscale)
        message("Scale for '", scalename,
          "' is already present. Adding another scale for '", scalename,
          "', which will be merged with the existing scale.")
      }
    }

    scales <<- c(scales[!prev_aes], list(scale))
  }, 
  is.identical = function(x1,x2) {
    # recursive function to evaluate nested lists
    if(is.list(x1) && is.list(x2))
      all(mapply(is.identical,x1,x2))
    else if (identical(x1,x2)) # for regular values (i.e. non functions)
      TRUE
    else if (!(is.function(x1) && is.function(x2))) # all non-function values are handled at this point
      FALSE
    else {
      # at this point, x1 and x2 are functions
      # evaluate both functions in case functions are non-parametric
      try(return(identical(x1(),x2())),silent=TRUE)

      # too complicated to evaluate if a function's return value would be the same
      # given the body and its environment 
      # only checks if environments share names with the same values and if bodies are the same
      # TODO:  body() retrieves attributes storing the entire reference, might be beneficial to
      # remove those references prior to calling identical() on it 
      if (!identical(body(x1),body(x2))) return(FALSE) 
      all(mapply(identical, as.list(environment(x1)), as.list(environment(x2))))
    }
  },
  merge.scales = function(scale,scalename,prevscale){
    t1 <- strsplit(as.character(scale$call[[1]]),"_",fixed=TRUE)[[1]]
    default <- get(paste(t1[2],scalename,t1[1],sep="_"))()
    s <- mapply(function(x1,x2,x3){
                 if(is.identical(x2,x3)) x1
                 else x2
                },prevscale,scale,default)
    class(s) <- class(scale)
    s
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
    scale_f <- find_global(scale_name, env)
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

    scale_f <- find_global(scale_name, env)
    plot$scales$add(scale_f())
  }
}


# Look for object first in parent environment and if not found, then in 
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
find_global <- function(name, env) {
  if (exists(name, env)) {
    return(get(name, env))
  }

  nsenv <- asNamespace("ggplot2")
  if (exists(name, nsenv)) {
    return(get(name, nsenv))
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
