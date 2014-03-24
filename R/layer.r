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

  new <- function (., geom=NULL, geom_params=NULL, stat=NULL, stat_params=NULL, data=NULL, mapping=NULL, position=NULL, params=NULL, ..., inherit.aes = TRUE, legend = NA, subset = NULL, show_guide = NA) {

    # now, as for the guide, we can choose only if the layer is included or not in the guide: guide = TRUE or guide = FALSE
    # in future, it may be better if we can choose which aes of this layer is included in the guide, e.g.: guide = c(colour = TRUE, size = FALSE)
    if (!is.na(legend)) {
      gg_dep("0.8.9", "\"legend\" argument in geom_XXX and stat_XXX is deprecated. Use show_guide = TRUE or show_guide = FALSE for display or suppress the guide display.")
      show_guide = legend
    }

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
      stop("Aesthetics must either be length one, or the same length as the data",
        "Problems:", paste(aesthetics[wrong], collapse = ", "), call. = FALSE)
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
    if (empty(data)) return(data.frame())

    check_required_aesthetics(.$stat$required_aes,
      c(names(data), names(.$stat_params)),
      paste("stat_", .$stat$objname, sep=""))

    res <- NULL
    try(res <- do.call(.$stat$calculate_groups, c(
      list(data=as.name("data"), scales=as.name("scales")),
      .$stat_params)
    ))
    if (is.null(res)) return(data.frame())

    res

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
#' @keywords internal
#' @export
layer <- Layer$new
