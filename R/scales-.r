# Scales object encapsulates multiple scales.
# All input and output done with data.frames to facilitate
# multiple input and output variables

scales_list <- function() {
  ggproto(NULL, ScalesList)
}

ScalesList <- ggproto("ScalesList", NULL,
  scales = NULL,

  find = function(self, aesthetic) {
    vapply(self$scales, function(x) any(aesthetic %in% x$aesthetics), logical(1))
  },

  has_scale = function(self, aesthetic) {
    any(self$find(aesthetic))
  },

  add = function(self, scale) {
    if (is.null(scale)) {
      return()
    }

    prev_aes <- self$find(scale$aesthetics)
    if (any(prev_aes)) {
      # Get only the first aesthetic name in the returned vector -- it can
      # sometimes be c("x", "xmin", "xmax", ....)
      scalename <- self$scales[prev_aes][[1]]$aesthetics[1]
      message_wrap("Scale for '", scalename,
        "' is already present. Adding another scale for '", scalename,
        "', which will replace the existing scale.")
    }

    # Remove old scale for this aesthetic (if it exists)
    self$scales <- c(self$scales[!prev_aes], list(scale))
  },

  n = function(self) {
    length(self$scales)
  },

  input = function(self) {
    unlist(lapply(self$scales, "[[", "aesthetics"))
  },

  # This actually makes a descendant of self, which is functionally the same
  # as a actually clone for most purposes.
  clone = function(self) {
    ggproto(NULL, self, scales = lapply(self$scales, function(s) s$clone()))
  },

  non_position_scales = function(self) {
    ggproto(NULL, self, scales = self$scales[!self$find("x") & !self$find("y")])
  },

  get_scales = function(self, output) {
    scale <- self$scales[self$find(output)]
    if (length(scale) == 0) return()
    scale[[1]]
  }
)

# Train scale from a data frame
scales_train_df <- function(scales, df, drop = FALSE) {
  if (empty(df) || length(scales$scales) == 0) return()

  lapply(scales$scales, function(scale) scale$train_df(df = df))
}

# Map values from a data.frame. Returns data.frame
scales_map_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0) return(df)

  mapped <- unlist(lapply(scales$scales, function(scale) scale$map_df(df = df)), recursive = FALSE)

  plyr::quickdf(c(mapped, df[setdiff(names(df), names(mapped))]))
}

# Transform values to cardinal representation
scales_transform_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0) return(df)

  transformed <- unlist(lapply(scales$scales, function(s) s$transform_df(df = df)),
    recursive = FALSE)
  plyr::quickdf(c(transformed, df[setdiff(names(df), names(transformed))]))
}

# @param aesthetics A list of aesthetic-variable mappings. The name of each
#   item is the aesthetic, and the value of each item is the variable in data.
scales_add_defaults <- function(scales, data, aesthetics, env) {
  if (is.null(aesthetics)) return()
  names(aesthetics) <- unlist(lapply(names(aesthetics), aes_to_scale))

  new_aesthetics <- setdiff(names(aesthetics), scales$input())
  # No new aesthetics, so no new scales to add
  if (is.null(new_aesthetics)) return()

  datacols <- plyr::tryapply(
    aesthetics[new_aesthetics], eval,
    envir = data, enclos = env
  )

  for (aes in names(datacols)) {
    scales$add(find_scale(aes, datacols[[aes]], env))
  }

}

# Add missing but required scales.
# @param aesthetics A character vector of aesthetics. Typically c("x", "y").
scales_add_missing <- function(plot, aesthetics, env) {

  # Keep only aesthetics that aren't already in plot$scales
  aesthetics <- setdiff(aesthetics, plot$scales$input())

  for (aes in aesthetics) {
    scale_name <- paste("scale", aes, "continuous", sep = "_")

    scale_f <- find_global(scale_name, env, mode = "function")
    plot$scales$add(scale_f())
  }
}


