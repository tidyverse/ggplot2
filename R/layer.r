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
  inherit.aes <- FALSE
  
  new <- function (., geom=NULL, geom_params=NULL, stat=NULL, stat_params=NULL, data=NULL, mapping=NULL, position=NULL, params=NULL, ..., inherit.aes = TRUE, legend = NA, subset = NULL) {
    
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
    
    if (!is.null(geom_params)) {
      set_aesthetics <- geom_params[intersect(names(geom_params), .all_aesthetics)]
      # Check that all set aesthetics have length 1
      if (length(set_aesthetics) > 0) {
        lengths <- sapply(set_aesthetics, length)
        if (any(lengths > 1)) {
          stop("When _setting_ aesthetics, they may only take one value. ", 
            "Problems: ",
            paste(names(set_aesthetics)[lengths > 1], collapse = ","), 
            call. = FALSE)
        }
        
      }
    }
    
    proto(., 
      geom=geom, geom_params=geom_params, 
      stat=stat, stat_params=stat_params, 
      data=data, mapping=mapping, subset=subset,
      position=position,
      inherit.aes = inherit.aes,
      legend = legend
    )
  }
  
  clone <- function(.) as.proto(.$as.list(all.names=TRUE))
  
  use_defaults <- function(., data) {
    mapped_vars <- .$mapping[!sapply(.$mapping, is.null)]
    df <- aesdefaults(data, .$geom$default_aes(), mapped_vars)
    
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
    data <- if(empty(.$data)) plot$data else .$data

    # Apply subsetting, if used
    if (!is.null(.$subset)) {
      include <- data.frame(eval.quoted(.$subset, data))
      data <- data[rowSums(include) == ncol(include), ]
    }
    
    # For certain geoms, it is useful to be able to ignore the default
    # aesthetics and only use those set in the layer
    if (.$inherit.aes) {
      aesthetics <- compact(defaults(.$mapping, plot$mapping))
    } else {
      aesthetics <- .$mapping
    }
    
    # Override grouping if specified in layer
    if (!is.null(.$geom_params$group)) {
      aesthetics["group"] <- .$geom_params$group
    } 
    
    # Drop aesthetics that are set manually
    aesthetics <- aesthetics[setdiff(names(aesthetics), names(.$geom_params))]
    plot$scales$add_defaults(plot$data, aesthetics, plot$plot_env)
    
    # Evaluate aesthetics in the context of their data frame
    eval.each <- function(dots) 
      compact(lapply(dots, function(x.) eval(x., data, plot$plot_env)))

    aesthetics <- aesthetics[!is_calculated_aes(aesthetics)]
    evaled <- eval.each(aesthetics)
    if (length(evaled) == 0) return(data.frame())

    evaled <- evaled[sapply(evaled, is.atomic)]
    df <- data.frame(evaled)

    # Add Conditioning variables needed for facets
    cond <- plot$facet$conditionals()
    facet_vars <- data[, intersect(names(data), cond), drop=FALSE]
    if (nrow(facet_vars) > 0) {
      df <- cbind(df, facet_vars)  
    }

    if (empty(plot$data)) return(df)
    facet_vars <- unique(plot$data[, setdiff(cond, names(df)), drop=FALSE])
    
    if (empty(data)) return(facet_vars)
    expand.grid.df(df, facet_vars, unique = FALSE)
  }

  calc_statistics <- function(., data, scales) {
    gg_apply(data, function(x) .$calc_statistic(x, scales))  
  }
  
  calc_statistic <- function(., data, scales) {
    if (empty(data)) return(data.frame())
    
    check_required_aesthetics(.$stat$required_aes, 
      c(names(data), names(.$stat_params)), 
      paste("stat_", .$stat$objname, sep=""))

    
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
    if (empty(data)) return(data.frame())

    aesthetics <- defaults(.$mapping, 
      defaults(plot$mapping, .$stat$default_aes()))

    new <- strip_dots(aesthetics[is_calculated_aes(aesthetics)])
    if (length(new) == 0) return(data)

    # Add map stat output to aesthetics
    stat_data <- as.data.frame(lapply(new, eval, data, baseenv()))
    names(stat_data) <- names(new)
    
    # Add any new scales, if needed
    plot$scales$add_defaults(data, new, plot$plot_env)
    # Transform the values, if the scale say it's ok 
    # (see stat_spoke for one exception)
    if (.$stat$retransform) {
      stat_data <- plot$scales$transform_df(stat_data)
    }
    
    cunion(stat_data, data)
  }

  reparameterise <- function(., data) {
    gg_apply(data, function(df) {
      if (empty(df)) return(data.frame())

      .$geom$reparameterise(df, .$geom_params) 
    })
  }

  adjust_position <- function(., data, scales) {
    gg_apply(data, function(x) {
      .$position$adjust(x, scales)
    })
  }
  
  make_grob <- function(., data, scales, cs) {
    if (empty(data)) return(zeroGrob())
    
    data <- .$use_defaults(data)
    
    check_required_aesthetics(.$geom$required_aes, c(names(data), names(.$geom_params)), paste("geom_", .$geom$objname, sep=""))
    
    if (is.null(data$group)) data$group <- 1
    
    # If ordering is set, modify group variable according to this order
    if (!is.null(data$order)) {
      data$group <- ninteraction(list(data$group, data$order))
      data$order <- NULL
    }
    
    data <- data[order(data$group), ]
    
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



# Is calculated aesthetic?
# Determine if aesthetic is calculated from the statistic
# 
# @keywords internal
is_calculated_aes <- function(aesthetics) {
  match <- "\\.\\.([a-zA-z._]+)\\.\\."
  stats <- rep(F, length(aesthetics))
  stats[grep(match, sapply(aesthetics, deparse))] <- TRUE
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