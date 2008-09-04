mplot <- qplot <- function(x, y = NULL, z=NULL, ..., data, facets = . ~ ., geom = "point", stat=list(NULL), position=list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA) {

  argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  
  aesthetics <- compact(arguments[.all_aesthetics])
  aesthetics <- aesthetics[!is.constant(aesthetics)]
  aes_names <- names(aesthetics)
  aesthetics <- rename_aes(aesthetics)
  class(aesthetics) <- "uneval"
  
  # Create data if not explicitly specified
  if (missing(data)) {
    var_string <- unique(unlist(lapply(drop_calculated_aes(aesthetics), function(x) all.vars(asOneSidedFormula(x)))))
    var_names <- unlist(lapply(var_string, as.name))
    
    data <- as.data.frame(lapply(var_names, eval, parent.frame(n=2)))
    names(data) <- var_string

    facetvars <- all.vars(facets)
    facetvars <- facetvars[facetvars != "."]
    facetsdf <- as.data.frame(sapply(facetvars, get))
    if (nrow(facetsdf)) data <- cbind(data, facetsdf)
  } else {
    if (!is.data.frame(data)) stop("data is not a data.frame")
    if (ncol(data) == 0) stop("data has no columns")
    if (nrow(data) == 0) stop("data has no rows")
  }

  p <- ggplot(data, aesthetics) + facet_grid(facets=deparse(facets), margins=margins)
  
  if (!is.null(main)) p <- p + opts("title" = main)

  # Add geoms/statistics
  if (is.proto(position)) position <- list(position)
  
  mapply(function(g, s, ps) {
    if(is.character(g)) g <- Geom$find(g)
    if(is.character(s)) s <- Stat$find(s)
    if(is.character(ps)) ps <- Position$find(ps)

    params <- arguments[setdiff(names(arguments), c(aes_names, argnames))]
    params <- lapply(params, eval, parent.frame(n=1))
    
    p <<- p + layer(geom=g, stat=s, geom_params=params, stat_params=params, position=ps)
  }, geom, stat, position)
  
  logv <- function(var) var %in% strsplit(log, "")[[1]]

  if (logv("x")) p <- p + scale_x_log10()
  if (logv("y")) p <- p + scale_y_log10()
  
  if (!is.na(asp)) p <- p + opts(aspect.ratio = asp)
  
  if (!missing(xlab)) assign("name", xlab, envir=p$scales$get_scales("x"))
  if (!missing(ylab)) assign("name", ylab, envir=p$scales$get_scales("y"))
  
  if (!missing(xlim)) assign("limits", xlim, envir=p$scales$get_scales("x"))
  if (!missing(ylim)) assign("limits", ylim, envir=p$scales$get_scales("y"))
  
  p
}
