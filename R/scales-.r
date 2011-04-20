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
    scales <<- c(scales[!find(scale)], list(scale))
  }, 
  clone = function() {
    Scales$new(scales)
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
  if (empty(df) || length(scales$scales) == 0) return()
  
  transformed <- unlist(lapply(scales$scales, scale_transform_df, df = df),
    recursive = FALSE)
  quickdf(c(transformed, df[setdiff(names(df), names(transformed))]))
}

scales_add_defaults <- function(scales, data, aesthetics, env) {
  if (is.null(aesthetics)) return()
  names(aesthetics) <- unlist(lapply(names(aesthetics), aes_to_scale))
  
  new_aesthetics <- setdiff(names(aesthetics), scales$input())
  # No new aesthetics, so no new scales to add
  if(is.null(new_aesthetics)) return()
  
  # Determine variable type for each column -------------------------------
  vartype <- function(x) {
    if (inherits(x, "Date")) return("date")
    if (inherits(x, "POSIXt")) return("datetime")
    if (is.numeric(x)) return("continuous")
    
    "discrete"
  }

  datacols <- tryapply(
    aesthetics[new_aesthetics], eval, 
    envir=data, enclos=env
  )
  new_aesthetics <- intersect(new_aesthetics, names(datacols))
  if (length(datacols) == 0) return()
  
  vartypes <- sapply(datacols, vartype)
  
  # Work out scale names
  scale_name_type <- paste("scale", new_aesthetics, vartypes, sep="_")

  for(i in 1:length(new_aesthetics)) {
    if (exists(scale_name_type[i])) {
      scale <- get(scale_name_type[i], globalenv())()
      scales$add(scale)
    }
  }
}

scales_legend_desc <- function(scales, theme) {
  # Loop through all scales, creating a list of titles, and a list of keys
  keys <- titles <- vector("list", length(scales$scales))
  hash <- character(length(scales$scales))
  guide <- character(length(scales$scales))
  
  for(i in seq_along(hash)) {
    scale <- scales$scales[[i]]
    if (!scale$legend || is.null(scale_limits(scale))) next
    
    # Figure out legend title
    output <- scale$aesthetics[1]
    titles[[i]] <- scale$name %||% theme$labels[[output]]

    guide[i] <- nulldefault(scale$guide, "default")

    if (guide[i] == "colorbar") {
      
      if (is.null(scale$breaks)) {
        breaks <- pretty(scale_limits(scale), nulldefault(scale$nbreak, 5))
      } else if (is.function(scale$breaks)) {
        breaks <- scale$breaks(limits)
      } else {
        breaks <- scale$breaks
      }
      breaks <- discard(breaks, scale_limits(scale))
      key <- data.frame(
        scale_map(scale, breaks), I(scale_labels(scale, breaks)), breaks,
        stringsAsFactors = FALSE)
      names(key) <- c(output, ".label", ".value")

      bar <- discard(pretty(scale_limits(scale), n = nulldefault(theme$legend.colorbar.nbin, 20)), scale_limits(scale))
      attr(key, "bar") <- data.frame(colour=scale_map(scale, bar), value=bar, stringsAsFactors = FALSE)

      hash[i] <- digest(list(titles[[i]], key$.label, key[[output]], guide[i]))
      
    } else if (guide[i] == "default") {
      
      key <- data.frame(
        scale_map(scale, scale_breaks(scale)), I(scale_labels(scale)), 
        stringsAsFactors = FALSE)
        names(key) <- c(output, ".label")

      hash[i] <- digest(list(titles[[i]], key$.label, guide[i]))
    } else {
      warning("Unknown guide type:", guide[i], call. = FALSE)
    }

    keys[[i]] <- key
  }

  empty <- sapply(titles, is.null)
  
  list(titles = titles[!empty], keys = keys[!empty], hash = hash[!empty], guide=guide[!empty])
}
