# Create a new layer
# Layer objects store the layer of an object.
#
# They have the following attributes:
#
#  * data
#  * geom + parameters
#  * statistic + parameters
#  * position + parameters
#  * aesthetic mapping
#  * flag for display guide: TRUE/FALSE/NA. in the case of NA, decision depends on a guide itself.
#
# Can think about grob creation as a series of data frame transformations.
Layer <- proto(expr = {
  geom <- NULL
  geom_params <- NULL
  stat <- NULL
  stat_params <- NULL
  data <- NULL
  mapping <- NULL
  position <- NULL
  params <- NULL
  inherit.aes <- FALSE

  new <- function (., geom=NULL, geom_params=NULL, stat=NULL, stat_params=NULL, data=NULL, mapping=NULL, position=NULL, params=NULL, ..., inherit.aes = TRUE, subset = NULL, show_guide = NA) {

    # now, as for the guide, we can choose only if the layer is included or not in the guide: guide = TRUE or guide = FALSE
    # in future, it may be better if we can choose which aes of this layer is included in the guide, e.g.: guide = c(colour = TRUE, size = FALSE)

    if (!is.na(show_guide) && !is.logical(show_guide)) {
      warning("`show_guide` in geom_XXX and stat_XXX must be logical.")
      show_guide = FALSE
    }


    if (is.null(geom) && is.null(stat)) stop("Need at least one of stat and geom")

    data <- fortify(data)
    if (!is.null(mapping) && !inherits(mapping, "uneval")) stop("Mapping should be a list of unevaluated mappings created by aes or aes_string")

    if (is.character(geom)) geom <- Geom$find(geom)
    if (is.character(stat)) stat <- Stat$find(stat)
    if (is.character(position)) position <- Position$find(position)$new()

    if (is.null(geom)) geom <- stat$default_geom()
    if (is.null(stat)) stat <- geom$default_stat()
    if (is.null(position)) position <- geom$default_pos()$new()

    match.params <- function(possible, params) {
      if ("..." %in% names(possible)) {
        params
      } else {
        params[match(names(possible), names(params), nomatch=0)]
      }
    }

    if (is.null(geom_params) && is.null(stat_params)) {
      params <- c(params, list(...))
      params <- rename_aes(params) # Rename American to British spellings etc

      geom_params <- match.params(geom$parameters(), params)
      stat_params <- match.params(stat$parameters(), params)
      stat_params <- stat_params[setdiff(names(stat_params),
        names(geom_params))]
    } else {
      geom_params <- rename_aes(geom_params)
    }

    proto(.,
      geom=geom, geom_params=geom_params,
      stat=stat, stat_params=stat_params,
      data=data, mapping=mapping, subset=subset,
      position=position,
      inherit.aes = inherit.aes,
      show_guide = show_guide,
    )
  }

  clone <- function(.) as.proto(.$as.list(all.names=TRUE))

  use_defaults <- function(., data) {
    df <- aesdefaults(data, .$geom$default_aes(), NULL)

    # Override mappings with atomic parameters
    gp <- intersect(c(names(df), .$geom$required_aes), names(.$geom_params))
    gp <- gp[unlist(lapply(.$geom_params[gp], is.atomic))]

    # Check that mappings are compatable length: either 1 or the same length
    # as the data
    param_lengths <- vapply(.$geom_params[gp], length, numeric(1))
    bad <- param_lengths != 1L & param_lengths != nrow(df)
    if (any(bad)) {
      stop("Incompatible lengths for set aesthetics: ",
        paste(names(bad), collapse = ", "), call. = FALSE)
    }

    df[gp] <- .$geom_params[gp]
    df
  }

  layer_mapping <- function(., mapping = NULL) {
    # For certain geoms, it is useful to be able to ignore the default
    # aesthetics and only use those set in the layer
    if (.$inherit.aes) {
      aesthetics <- compact(defaults(.$mapping, mapping))
    } else {
      aesthetics <- .$mapping
    }

    # Drop aesthetics that are set or calculated
    set <- names(aesthetics) %in% names(.$geom_params)
    calculated <- is_calculated_aes(aesthetics)

    aesthetics[!set & !calculated]
  }

  pprint <- function(.) {
    if (is.null(.$geom)) {
      cat("Empty layer\n")
      return(invisible());
    }
    if (!is.null(.$mapping)) {
      cat("mapping:", clist(.$mapping), "\n")
    }
    .$geom$print(newline=FALSE)
    cat(clist(.$geom_params), "\n")
    .$stat$print(newline=FALSE)
    cat(clist(.$stat_params), "\n")
    .$position$print()
  }


  compute_aesthetics <- function(., data, plot) {
    aesthetics <- .$layer_mapping(plot$mapping)

    if (!is.null(.$subset)) {
      include <- data.frame(eval.quoted(.$subset, data, plot$env))
      data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
    }

    # Override grouping if set in layer.
    if (!is.null(.$geom_params$group)) {
      aesthetics["group"] <- .$geom_params$group
    }

    scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)

    # Evaluate aesthetics in the context of their data frame
    evaled <- compact(
      eval.quoted(aesthetics, data, plot$plot_env))

    lengths <- vapply(evaled, length, integer(1))
    n <- if (length(lengths) > 0) max(lengths) else 0

    wrong <- lengths != 1 & lengths != n
    if (any(wrong)) {
      stop("Aesthetics must either be length one, or the same length as the data.",
        "\nProblems: ", paste(aesthetics[wrong], collapse = ", "), call. = FALSE)
    }

    if (empty(data) && n > 0) {
      # No data, and vectors suppled to aesthetics
      evaled$PANEL <- 1
    } else {
      evaled$PANEL <- data$PANEL
    }
    data.frame(evaled)
  }


  calc_statistic <- function(., data, scales) {
    if (empty(data))
      return(data.frame())

    check_required_aesthetics(.$stat$required_aes,
      c(names(data), names(.$stat_params)),
      paste("stat_", .$stat$objname, sep = ""))

    args <- c(list(data = quote(data), scales = quote(scales)), .$stat_params)
    tryCatch(do.call(.$stat$calculate_groups, args), error = function(e) {
      warning("Computation failed in `stat_", .$stat$objname, "()`:\n",
        e$message, call. = FALSE)
      data.frame()
    })
  }


  map_statistic <- function(., data, plot) {
    if (empty(data)) return(data.frame())

    # Assemble aesthetics from layer, plot and stat mappings
    aesthetics <- .$mapping
    if (.$inherit.aes) {
      aesthetics <- defaults(aesthetics, plot$mapping)
    }
    aesthetics <- defaults(aesthetics, .$stat$default_aes())
    aesthetics <- compact(aesthetics)

    new <- strip_dots(aesthetics[is_calculated_aes(aesthetics)])
    if (length(new) == 0) return(data)

    # Add map stat output to aesthetics
    stat_data <- as.data.frame(lapply(new, eval, data, baseenv()))
    names(stat_data) <- names(new)

    # Add any new scales, if needed
    scales_add_defaults(plot$scales, data, new, plot$plot_env)
    # Transform the values, if the scale say it's ok
    # (see stat_spoke for one exception)
    if (.$stat$retransform) {
      stat_data <- scales_transform_df(plot$scales, stat_data)
    }

    cunion(stat_data, data)
  }

  reparameterise <- function(., data) {
    if (empty(data)) return(data.frame())
    .$geom$reparameterise(data, .$geom_params)
  }


  adjust_position <- function(., data) {
    ddply(data, "PANEL", function(data) {
      .$position$adjust(data)
    })
  }

  make_grob <- function(., data, scales, cs) {
    if (empty(data)) return(zeroGrob())

    data <- .$use_defaults(data)

    check_required_aesthetics(.$geom$required_aes,
      c(names(data), names(.$geom_params)),
      paste("geom_", .$geom$objname, sep=""))

    do.call(.$geom$draw_groups, c(
      data = list(as.name("data")),
      scales = list(as.name("scales")),
      coordinates = list(as.name("cs")),
      .$geom_params
    ))
  }

  class <- function(.) "layer"
})

#' Create a new layer
#'
#' @export
#' @inheritParams geom_point
#' @param geom Geometric element, as a string.
#' @param geom_params,stat_params,params,... Additional parameters to the
#'   \code{geom} and \code{stat}. If supplied individual in \code{...} or as a
#'   list in \code{params}, \code{layer} does it's best to figure out which
#'   arguments belong to which. To be explicit, supply as individual lists to
#'   \code{geom_param} and \code{stat_param}.
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_string}}. If specified and \code{inherit.aes = TRUE},
#'   is combined with the default mapping at the top level of the plot.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param subset DEPRECATED. An older way of subsetting the dataset used in a
#'   layer.
#' @examples
#' # geom calls are just a short cut for layer
#' ggplot(mpg, aes(displ, hwy)) + geom_point()
#' # shortcut for
#' ggplot(mpg, aes(displ, hwy)) + layer(geom = "point", stat = "identity")
layer <- function(geom = NULL, geom_params = NULL, stat = NULL,
                  stat_params = NULL, data = NULL, mapping = NULL,
                  position = NULL, params = NULL, ...,
                  inherit.aes = TRUE, subset = NULL, show_guide = NA) {
  Layer$new(geom = geom, geom_params = geom_params, stat = stat,
    stat_params = stat_params, data = data, mapping = mapping,
    position = position, params = params, ..., inherit.aes = inherit.aes,
    subset = subset, show_guide = show_guide)
}



LayerR6 <- R6::R6Class("LayerR6",
  lock_objects = FALSE,
  public = list(
    geom = NULL,
    geom_params = NULL,
    stat = NULL,
    stat_params = NULL,
    data = NULL,
    mapping = NULL,
    position = NULL,
    params = NULL,
    inherit.aes = FALSE,

    initialize = function(geom = NULL, geom_params = NULL, stat = NULL,
      stat_params = NULL, data = NULL, mapping = NULL, position = NULL,
      params = NULL, inherit.aes = TRUE, subset = NULL, show_guide = NA)
    {
      # now, as for the guide, we can choose only if the layer is included or not in the guide: guide = TRUE or guide = FALSE
      # in future, it may be better if we can choose which aes of this layer is included in the guide, e.g.: guide = c(colour = TRUE, size = FALSE)

      if (!is.na(show_guide) && !is.logical(show_guide)) {
        warning("`show_guide` in geom_XXX and stat_XXX must be logical.")
        show_guide = FALSE
      }


      if (is.null(geom) && is.null(stat)) stop("Need at least one of stat and geom")

      data <- fortify(data)
      if (!is.null(mapping) && !inherits(mapping, "uneval")) stop("Mapping should be a list of unevaluated mappings created by aes or aes_string")

      if (is.character(geom)) geom <- GeomR6$new()$find(geom)
      if (is.character(stat)) stat <- StatR6$new()$find(stat)
      if (is.character(position)) position <- Position$new()$find(position)$new()

      # Instantiate the geom or stat - do this at run time instead of package
      # build time, so that geoms in external packages set up inheritance with
      # the current version of ggplot2, not whatever version they were built
      # with.
      if (!is.null(geom)) geom <- geom$new()
      if (!is.null(stat)) stat <- stat$new()

      if (is.null(geom)) geom <- stat$default_geom()
      if (is.null(stat)) stat <- geom$default_stat()
      if (is.null(position)) position <- geom$default_pos()$new()

      match.params <- function(possible, params) {
        if ("..." %in% names(possible)) {
          params
        } else {
          params[match(names(possible), names(params), nomatch=0)]
        }
      }

      if (is.null(geom_params) && is.null(stat_params)) {
        params <- rename_aes(params) # Rename American to British spellings etc

        geom_params <- match.params(geom$parameters(), params)
        stat_params <- match.params(stat$parameters(), params)
        stat_params <- stat_params[setdiff(names(stat_params),
          names(geom_params))]
      } else {
        geom_params <- rename_aes(geom_params)
      }

      self$geom        <- geom
      self$geom_params <- geom_params
      self$stat        <- stat
      self$stat_params <- stat_params
      self$data        <- data
      self$mapping     <- mapping
      self$subset      <- subset
      self$position    <- position
      self$inherit.aes <- inherit.aes
      self$show_guide  <- show_guide
    },

    use_defaults = function(data) {
      df <- aesdefaults(data, self$geom$default_aes(), NULL)

      # Override mappings with atomic parameters
      gp <- intersect(c(names(df), self$geom$required_aes), names(self$geom_params))
      gp <- gp[unlist(lapply(self$geom_params[gp], is.atomic))]

      # Check that mappings are compatable length: either 1 or the same length
      # as the data
      param_lengths <- vapply(self$geom_params[gp], length, numeric(1))
      bad <- param_lengths != 1L & param_lengths != nrow(df)
      if (any(bad)) {
        stop("Incompatible lengths for set aesthetics: ",
          paste(names(bad), collapse = ", "), call. = FALSE)
      }

      df[gp] <- self$geom_params[gp]
      df
    },

    layer_mapping = function(mapping = NULL) {
      # For certain geoms, it is useful to be able to ignore the default
      # aesthetics and only use those set in the layer
      if (self$inherit.aes) {
        aesthetics <- compact(defaults(self$mapping, mapping))
      } else {
        aesthetics <- self$mapping
      }

      # Drop aesthetics that are set or calculated
      set <- names(aesthetics) %in% names(self$geom_params)
      calculated <- is_calculated_aes(aesthetics)

      aesthetics[!set & !calculated]
    },

    pprint = function() {
      if (is.null(self$geom)) {
        cat("Empty layer\n")
        return(invisible());
      }
      if (!is.null(self$mapping)) {
        cat("mapping:", clist(self$mapping), "\n")
      }
      self$geom$print(newline=FALSE)
      cat(clist(self$geom_params), "\n")
      self$stat$print(newline=FALSE)
      cat(clist(self$stat_params), "\n")
      self$position$print()
    },


    compute_aesthetics = function(data, plot) {
      aesthetics <- self$layer_mapping(plot$mapping)

      if (!is.null(self$subset)) {
        include <- data.frame(eval.quoted(self$subset, data, plot$env))
        data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
      }

      # Override grouping if set in layer.
      if (!is.null(self$geom_params$group)) {
        aesthetics["group"] <- self$geom_params$group
      }

      scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)

      # Evaluate aesthetics in the context of their data frame
      evaled <- compact(
        eval.quoted(aesthetics, data, plot$plot_env))

      lengths <- vapply(evaled, length, integer(1))
      n <- if (length(lengths) > 0) max(lengths) else 0

      wrong <- lengths != 1 & lengths != n
      if (any(wrong)) {
        stop("Aesthetics must either be length one, or the same length as the data.",
          "\nProblems: ", paste(aesthetics[wrong], collapse = ", "), call. = FALSE)
      }

      if (empty(data) && n > 0) {
        # No data, and vectors suppled to aesthetics
        evaled$PANEL <- 1
      } else {
        evaled$PANEL <- data$PANEL
      }
      data.frame(evaled)
    },


    calc_statistic = function(data, scales) {
      if (empty(data))
        return(data.frame())

      check_required_aesthetics(self$stat$required_aes,
        c(names(data), names(self$stat_params)),
        paste("stat_", self$stat$objname, sep = ""))

      args <- c(list(data = quote(data), scales = quote(scales)), self$stat_params)
      tryCatch(do.call(self$stat$calculate_groups, args), error = function(e) {
        warning("Computation failed in `stat_", self$stat$objname, "()`:\n",
          e$message, call. = FALSE)
        data.frame()
      })
    },


    map_statistic = function(data, plot) {
      if (empty(data)) return(data.frame())

      # Assemble aesthetics from layer, plot and stat mappings
      aesthetics <- self$mapping
      if (self$inherit.aes) {
        aesthetics <- defaults(aesthetics, plot$mapping)
      }
      aesthetics <- defaults(aesthetics, self$stat$default_aes())
      aesthetics <- compact(aesthetics)

      new <- strip_dots(aesthetics[is_calculated_aes(aesthetics)])
      if (length(new) == 0) return(data)

      # Add map stat output to aesthetics
      stat_data <- as.data.frame(lapply(new, eval, data, baseenv()))
      names(stat_data) <- names(new)

      # Add any new scales, if needed
      scales_add_defaults(plot$scales, data, new, plot$plot_env)
      # Transform the values, if the scale say it's ok
      # (see stat_spoke for one exception)
      if (self$stat$retransform) {
        stat_data <- scales_transform_df(plot$scales, stat_data)
      }

      cunion(stat_data, data)
    },

    reparameterise = function(data) {
      if (empty(data)) return(data.frame())
      self$geom$reparameterise(data, self$geom_params)
    },


    adjust_position = function(data) {
      ddply(data, "PANEL", function(data) {
        self$position$adjust(data)
      })
    },

    make_grob = function(data, scales, cs) {
      if (empty(data)) return(zeroGrob())

      data <- self$use_defaults(data)

      check_required_aesthetics(self$geom$required_aes,
        c(names(data), names(self$geom_params)),
        paste("geom_", self$geom$objname, sep=""))

      do.call(self$geom$draw_groups, c(
        data = list(as.name("data")),
        scales = list(as.name("scales")),
        coordinates = list(as.name("cs")),
        self$geom_params
      ))
    },

    class = function() "layer"
  )
)
