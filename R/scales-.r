# Scales object encapsulates multiple scales.
# All input and output done with data.frames to facilitate
# multiple input and output variables

scales_list <- function() {
  ggproto(NULL, ScalesList)
}

ScalesList <- ggproto("ScalesList", NULL,
  scales = NULL,
  scales_params = list(),

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

  # Save parameters for a scale, to be applied later
  # via `update_scales_params()`. The `params` object
  # should be created with `scales_params()`.
  add_params = function(self, params) {
    aesthetic <- params$aesthetic
    self$scales_params[[aesthetic]] <-
      defaults(params$params, self$scales_params[[aesthetic]])
  },

  # update the parameters for all scales currently stored
  update_scales_params = function(self) {
    for (scale in self$scales) {
      self$update_params_for_scale(scale)
    }
  },

  # update the parameters for one specific scale object
  update_params_for_scale = function(self, scale) {
    # get a list of all the params objects that are possibly relevant
    params_list <- self$scales_params[scale$aesthetics]
    # update the scale with each params object
    lapply(params_list, function(x) {
      if (!is.null(x)) scale$update_params(x)
    })
    invisible()
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

  new_data_frame(c(mapped, df[setdiff(names(df), names(mapped))]))
}

# Transform values to cardinal representation
scales_transform_df <- function(scales, df) {
  if (empty(df) || length(scales$scales) == 0) return(df)

  transformed <- unlist(lapply(scales$scales, function(s) s$transform_df(df = df)),
    recursive = FALSE)
  new_data_frame(c(transformed, df[setdiff(names(df), names(transformed))]))
}

# @param aesthetics A list of aesthetic-variable mappings. The name of each
#   item is the aesthetic, and the value of each item is the variable in data.
scales_add_defaults <- function(scales, data, aesthetics, env) {
  if (is.null(aesthetics)) return()
  names(aesthetics) <- unlist(lapply(names(aesthetics), aes_to_scale))

  new_aesthetics <- setdiff(names(aesthetics), scales$input())
  # No new aesthetics, so no new scales to add
  if (is.null(new_aesthetics)) return()

  datacols <- lapply(aesthetics[new_aesthetics], eval_tidy, data = data)
  datacols <- compact(datacols)

  for (aes in names(datacols)) {
    # find the appropriate scale object
    scale <- find_scale(aes, datacols[[aes]], env)
    # make sure it has the latest available parameters
    scales$update_params_for_scale(scale)
    # add to scales list
    scales$add(scale)
  }

}

# Add missing but required scales.
# @param aesthetics A character vector of aesthetics. Typically c("x", "y").
scales_add_missing <- function(plot, aesthetics, env) {

  # Keep only aesthetics that aren't already in plot$scales
  aesthetics <- setdiff(aesthetics, plot$scales$input())

  for (aes in aesthetics) {
    scale_name <- paste("scale", aes, "continuous", sep = "_")

    # find the appropriate scale object
    scale_fun <- find_global(scale_name, env, mode = "function")
    scale <- scale_fun()
    # make sure it has the latest available parameters
    scales$update_params_for_scale(scale)
    # add to scales list
    plot$scales$add(scale)
  }
}


# create a data structure to store parameters to be added to scales later on
# @param aesthetic A single aesthetic, *not* a vector of aesthetics.
# @param ... Parameters to be provided to the scale.
scales_params <- function(aesthetic, ...) {
  structure(
    list(
      aesthetic = aesthetic,
      params = list(...)
    ),
    class = "scales_params"
  )
}

