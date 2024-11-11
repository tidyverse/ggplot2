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
      cli::cli_inform(c(
        "Scale for {.field {scalename}} is already present.",
        "Adding another scale for {.field {scalename}}, which will replace the existing scale."
      ))
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
  },

  train_df = function(self, df, drop = FALSE) {
    if (empty(df) || length(self$scales) == 0) {
      return()
    }
    lapply(self$scales, function(scale) scale$train_df(df = df))
  },

  map_df = function(self, df) {
    if (empty(df) || length(self$scales) == 0) {
      return(df)
    }

    mapped <- unlist(lapply(
      self$scales,
      function(scale) scale$map_df(df = df)
    ), recursive = FALSE)

    data_frame0(!!!mapped, df[setdiff(names(df), names(mapped))])
  },

  transform_df = function(self, df) {
    if (empty(df)) {
      return(df)
    }

    # If the scale contains to trans or trans is identity, there is no need
    # to transform anything
    idx_skip <- vapply(self$scales, function(x) {
      transformation <- x$get_transformation()
      has_default_transform(x) &&
        (is.null(transformation) || identical(transformation$transform, identity))
    }, logical(1L))
    scales <- self$scales[!idx_skip]

    if (length(scales) == 0) {
      return(df)
    }

    transformed <- unlist(lapply(
      scales,
      function(scale) scale$transform_df(df = df)
    ), recursive = FALSE)

    data_frame0(!!!transformed, df[setdiff(names(df), names(transformed))])
  },

  backtransform_df = function(self, df) {
    # NOTE: no need to check empty(df) because it should be already checked
    # before this method is called.

    # If the scale contains to trans or trans is identity, there is no need
    # to transform anything
    idx_skip <- vapply(self$scales, function(x) {
      transformation <- x$get_transformation()
      has_default_transform(x) &&
        (is.null(transformation) || identical(transformation$transform, identity))
    }, logical(1))
    scales <- self$scales[!idx_skip]

    if (length(scales) == 0) {
      return(df)
    }

    backtransformed <- unlist(lapply(
      scales,
      function(scale) {
        aesthetics <- intersect(scale$aesthetics, names(df))
        if (length(aesthetics) == 0) {
          return()
        }
        inverse <- scale$get_transformation()$inverse
        if (is.null(inverse)) {
          return()
        }
        lapply(df[aesthetics], inverse)
      }
    ), recursive = FALSE)

    data_frame0(
      !!!backtransformed,
      df[setdiff(names(df), names(backtransformed))]
    )
  },

  # `aesthetics` is a list of aesthetic-variable mappings. The name of each
  # item is the aesthetic, and the value of each item is the variable in data.
  add_defaults = function(self, data, env) {
    new_aesthetics <- setdiff(names(data), self$input())
    # No new aesthetics, so no new scales to add
    if (is.null(new_aesthetics)) {
      return()
    }

    for (aes in new_aesthetics) {
      self$add(find_scale(aes, data[[aes]], env))
    }
  },

  # Add missing but required scales
  # `aesthetics` is a character vector of aesthetics. Typically c("x", "y")
  add_missing = function(self, aesthetics, env) {
    aesthetics <- setdiff(aesthetics, self$input())

    for (aes in aesthetics) {
      scale_name <- paste("scale", aes, "continuous", sep = "_")
      self$add(find_global(scale_name, env, mode = "function")())
    }
  },

  set_palettes = function(self, theme) {
    for (scale in self$scales) {
      if (!is.null(scale$palette)) {
        next
      }

      # Resolve palette theme setting for this scale
      type <- if (scale$is_discrete()) "discrete" else "continuous"
      elem <- paste0("palette.", scale$aesthetics, ".", type)
      elem <- compact(lapply(elem, calc_element, theme))[1][[1]]

      # Resolve the palette itself
      elem <- elem %||% fallback_palette(scale)
      palette <- switch(
        type,
        discrete   = as_discrete_pal(elem),
        continuous = as_continuous_pal(elem)
      )
      if (!is.function(palette)) {
        cli::cli_warn(
          "Failed to find palette for {.field {scale$aesthetics[1]}} scale."
        )
      }

      # Set palette to scale
      # Note: while direct assignment is not ideal, we've already cloned the
      # scale at the beginning of the plot build method, so it doesn't affect
      # other plots
      scale$palette <- palette
      invisible()
    }
  }
)

