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
  ignore.extra <- FALSE
  
  new <- function (., geom=NULL, geom_params=NULL, stat=NULL, stat_params=NULL, data=NULL, mapping=NULL, position=NULL, params=NULL, ..., ignore.extra = FALSE, legend = NA) {
    
    if (is.null(geom) && is.null(stat)) stop("Need at least one of stat and geom")
    
    if (!is.null(data) && !is.data.frame(data)) stop("Data needs to be a data.frame")
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
      geom_params <- match.params(geom$parameters(), params)
      stat_params <- match.params(stat$parameters(), params)
      stat_params <- stat_params[setdiff(names(stat_params), names(geom_params))]
    }
    
    proto(., 
      geom=geom, geom_params=geom_params, 
      stat=stat, stat_params=stat_params, 
      data=data, mapping=mapping, 
      position=position,
      ignore.extra = ignore.extra,
      legend = legend
    )
  }
  
  clone <- function(.) as.proto(.$as.list(all.names=TRUE))
  
  use_defaults <- function(., data) {
    df <- aesdefaults(data, .$geom$default_aes(), compact(.$mapping))
    
    # Override mappings with parameters
    gp <- intersect(c(names(df), .$geom$required_aes), names(.$geom_params))
    if (length(.$geom_params[gp])) 
      gp <- gp[sapply(.$geom_params[gp], is.atomic)]
    df[gp] <- .$geom_params[gp]
    df
  }
  
  aesthetics_used <- function(., plot_aesthetics) {
    aes <- defaults(.$mapping, plot_aesthetics)
    aes <- defaults(.$stat$default_aes(), aes)
    aesthetics <- names(compact(aes))
    aesthetics <- intersect(aesthetics, names(.$geom$default_aes()))
    parameters <- names(.$geom_params)
    setdiff(aesthetics, parameters)
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
  
  
  # Produce data.frame of evaluated aesthetics
  # Depending on the construction of the layer, we may need
  # to stitch together a data frame using the defaults from plot\$mapping 
  # and overrides for a given geom.
  #
  make_aesthetics <- function(., plot) {
    data <- nulldefault(.$data, plot$data)
    if (is.null(data)) stop("No data for layer", call.=FALSE)

    aesthetics <- compact(defaults(.$mapping, plot$mapping))
    # Override grouping if specified in layer
    if (!is.null(.$geom_params$group)) {
      aesthetics["group"] <- .$geom_params$group
    } 
    
    # Drop aesthetics that are set manually
    aesthetics <- aesthetics[setdiff(names(aesthetics), names(.$geom_params))]
    plot$scales$add_defaults(plot$data, aesthetics, plot$plot_env)
    
    calc_aesthetics(plot, data, aesthetics, .$ignore.extra)
  }

  calc_statistics <- function(., data, scales) {
    gg_apply(data, function(x) .$calc_statistic(x, scales))  
  }
  
  calc_statistic <- function(., data, scales) {
    if (is.null(data) || nrow(data) == 0) return(data.frame())
    
    res <- do.call(.$stat$calculate_groups, c(
      list(data=as.name("data"), scales=as.name("scales")), 
      .$stat_params)
    )
    if (is.null(res)) return(data.frame())
    
    res
    
  }

  # Map new aesthetic names
  # After the statistic transformation has been applied, a second round
  # of aesthetic mappings occur.  This allows the mapping of variables 
  # created by the statistic, for example, height in a histogram, levels
  # on a contour plot.
  # 
  # This also takes care of applying any scale transformations that might
  # be necessary
  map_statistics <- function(., data, plot) {
    gg_apply(data, function(x) .$map_statistic(x, plot=plot))
  }
  
  map_statistic <- function(., data, plot) {
    if (is.null(data) || length(data) == 0 || nrow(data) == 0) return()
    aesthetics <- defaults(.$mapping, 
      defaults(plot$mapping, .$stat$default_aes()))

    new <- strip_dots(aesthetics[is_calculated_aes(aesthetics)])
    if (length(new) == 0) return(data)

    # Add map stat output to aesthetics
    stat_data <- as.data.frame(lapply(new, eval, data, baseenv()))
    names(stat_data) <- names(new)
    
    # Add any new scales, if needed
    plot$scales$add_defaults(data, new, plot$plot_env)
    stat_data <- plot$scales$transform_df(stat_data)
    
    cunion(stat_data, data)
  }

  reparameterise <- function(., data) {
    gg_apply(data, function(df) {
      if (!is.null(df)) {
        .$geom$reparameterise(df, .$geom_params) 
      } else {
        data.frame()
      }
    })
  }

  adjust_position <- function(., data, scales) {
    gg_apply(data, function(x) {
      .$position$adjust(x, scales)
    })
  }
  
  make_grob <- function(., data, scales, cs) {
    if (is.null(data) || nrow(data) == 0) return(nullGrob())
    data <- .$use_defaults(data)
    
    check_required_aesthetics(.$geom$required_aes, c(names(data), names(.$geom_params)), paste("geom_", .$geom$objname, sep=""))
    
    if (is.null(data$order)) data$order <- data$group
    data <- data[order(data$order), ]
    
    do.call(.$geom$draw_groups, c(
      data = list(as.name("data")), 
      scales = list(as.name("scales")), 
      coordinates = list(as.name("cs")), 
      .$geom_params
    ))
  }

  class <- function(.) "layer"

  # Methods that probably belong elsewhere ---------------------------------
  
  # Stamp data.frame into list of matrices
  
  scales_transform <- function(., data, scales) {
    gg_apply(data, scales$transform_df)
  }

  # Train scale for this layer
  scales_train <- function(., data, scales) {
    gg_apply(data, scales$train_df)
  }

  
  # Map data using scales.
  scales_map <- function(., data, scale) {
    gg_apply(data, function(x) scale$map_df(x))
  }  
})

# Apply function to plot data components
# Convenience apply function for facets data structure
# 
# @keyword internal
gg_apply <- function(gg, f, ...) {
  apply(gg, c(1,2), function(data) {
    f(data[[1]], ...)
  })
}
layer <- Layer$new

# Build data frame
# Build data frome for a plot with given data and ... (dots) arguments
#
# Depending on the layer, we need
# to stitch together a data frame using the defaults from plot\$mapping 
# and overrides for a given geom.
#
# Arguments in dots are evaluated in the context of \\code{data} so that
# column names can easily be references. 
#
# Also makes sure that it contains all the columns required to correctly
# place the output into the row+column structure defined by the formula,
# by using \\code{\\link[reshape]{expand.grid.df}} to add in extra columns if needed.
#
# @arguments plot object
# @arguments data frame to use
# @arguments extra arguments supplied by user that should be used first
# @keyword hplot
# @keyword internal
calc_aesthetics <- function(plot, data = plot$data, aesthetics, ignore.extra = FALSE, env = plot$plot_env) {
  if (is.null(data)) data <- plot$data
  
  if (!is.data.frame(data)) {
    data <- fortify(data)
  }
  
  err <- if (ignore.extra) tryNULL else force
  eval.each <- function(dots) compact(lapply(dots, function(x.) err(eval(x., data, env))))
  # Conditioning variables needed for facets
  cond <- plot$facet$conditionals()
  
  aesthetics <- aesthetics[!is_calculated_aes(aesthetics)]
  evaled <- eval.each(aesthetics)
  if (length(evaled) == 0) return(data.frame())
  
  evaled <- evaled[sapply(evaled, is.atomic)]
  
  df <- data.frame(evaled)
  facet_vars <- data[, intersect(names(data), cond), drop=FALSE]
  if (nrow(facet_vars) > 0) {
    df <- cbind(df, facet_vars)  
  }
  
  if (is.null(plot$data)) return(df)
  expand.grid.df(df, unique(plot$data[, setdiff(cond, names(df)), drop=FALSE]), unique=FALSE)
}

# Is calculated aesthetic?
# Determine if aesthetic is calculated from the statistic
# 
# @keywords internal
is_calculated_aes <- function(aesthetics) {
  match <- "\\.\\.([a-zA-z._]+)\\.\\."
  stats <- rep(F, length(aesthetics))
  stats[grep(match, sapply(aesthetics, as.character))] <- TRUE
  stats
}

# Strip dots
# Strip dots from expressions that represent mappings of aesthetics to output from statistics
# 
# @keywords internal
strip_dots <- function(aesthetics) {
  match <- "\\.\\.([a-zA-z._]+)\\.\\."
  strings <- lapply(aesthetics, deparse)
  strings <- lapply(strings, gsub, pattern = match, replacement = "\\1")
  lapply(strings, function(x) parse(text = x)[[1]]) 
}