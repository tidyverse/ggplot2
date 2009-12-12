# Functions to allow each ggplot2 plot to describe itself, with code 
# that could be rerun to recreate the original plot.

Layer$call <- function(.) {
  params <- list()
  
  # Aesthetic mapping
  if (length(.$mapping)) {
    params[[1]] <- call_list(list(name = "aes", params = .$mapping))    
  }
  
  # Always show geom 
  params$geom <- .$geom$objname
  
  # Show stat & position if non-default
  if (!identical(.$stat, .$geom$default_stat())) {
    params$stat <- .$stat$objname
  }

  default_pos <- .$geom$default_pos()$call()
  pos <- .$position$call()
  if (!identical(pos, default_pos)) {
    params$position <- call_list(pos)
  }
  
  # Look for data in global namespace, otherwise create unique name from hash
  if (!is.null(.$data)) {
    params$data <- as.name(find_data(.$data))
  }
  

  # Combine all other parameters for geom and stat
  params <- c(params, .$geom_params, .$stat_params)
  
  list(
    name = "layer",
    params = params
  )
}

Position$call <- function(.) {
  list(
    name = paste("position", .$objname, sep = "_"),
    params = compact(list(width = .$width, height = .$height))
  )
}

Scale$call <- function(.) {
  name <- paste("scale", .$.input, .$objname, sep = "_")
  list(name = name, params = compact(.$params()))
}

Coord$call <- function(.) {
  name <- paste("coord", .$objname, sep = "_")
  params <- compact(.$params())
  
  # if (name == "coord_cartesian" & length(params) == 0) return()
  list(name = name, params = params)
}

find_data <- function(data) {
  names <- ls(".GlobalEnv")
  res <- NULL
  for (name in names) {
    obj <- get(name, ".GlobalEnv")
    if (is.data.frame(obj) && identical(obj, data)) {
      res <- name
      break
    }
  }
  if (is.null(res)) { 
    res <- paste("data", substr(digest(data), 1, 10), sep = "")
  }
  
  res
}

call_list <- function(x) {
  if (is.null(x)) return()
  
  if (length(x$params) > 0) {
    x$params <- sapply(x$params, deparse)
    fcall <- paste(x$name, "(", clist(x$params), ")", sep = "")
    parse(text = fcall)[[1]]
  } else {
    call(x$name)
  }
}

clist <- function(l) {
  suffix <- ifelse(names(l) != "", paste(names(l), " = ", sep = ""), "")
  paste(suffix, l, collapse=", ", sep = "")
}
