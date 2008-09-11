mplot <- function(vars, ..., data = NULL, facets = .xvar ~ .yvar, geom = "point", stat=list(NULL), position=list(NULL), margins = FALSE) {
  if (!is.data.frame(data)) stop("data is not a data.frame")
  if (ncol(data) == 0) stop("data has no columns")
  if (nrow(data) == 0) stop("data has no rows")
  
  argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  
  aesthetics <- compact(arguments[.all_aesthetics])
  aesthetics <- aesthetics[!is.constant(aesthetics)]
  aes_names <- names(aesthetics)
  aesthetics <- rename_aes(aesthetics)
  aesthetics <- defaults(aesthetics, aes_string(x="x", y="y"))
  class(aesthetics) <- "uneval"

  gridvars <- eval.quoted(vars, data)
  p <- length(gridvars)
  scaled <- rescaler(data, "range")
  grid <- expand.grid(x = seq_along(gridvars), y = seq_along(gridvars))
  grid <- subset(grid, x != y)
  
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]

    data.frame(
      .xvar = names(gridvars)[ycol], 
      .yvar = names(gridvars)[xcol],
      x = scaled[, xcol], y = scaled[, ycol], 
      data
    )
  }))
  all$.xvar <- factor(all$.xvar, levels=names(gridvars))
  all$.yvar <- factor(all$.yvar, levels=names(gridvars))
  
  densities <- do.call("rbind", lapply(seq_along(gridvars), function(i) {
    data.frame(
      .xvar = names(gridvars)[i], 
      .yvar = names(gridvars)[i],
      x = scaled[, i]
    )
  }))

  breaks <- seq(0,1, length=4)
  p <- ggplot(all, aesthetics) + 
    facet_grid(facets=deparse(facets), margins=margins) +
    scale_x_continuous("", limits=c(0, 1), breaks = breaks, labels = "") +
    scale_y_continuous("", limits=c(0, 1), breaks = breaks, labels = "") 
    # stat_density(aes_string(x="x", y = "..scaled.."), data=densities, fill="grey60", colour=NA)
    
  mapply(function(g, s, ps) {
    if(is.character(g)) g <- Geom$find(g)
    if(is.character(s)) s <- Stat$find(s)
    if(is.character(ps)) ps <- Position$find(ps)

    params <- arguments[setdiff(names(arguments), c(aes_names, argnames))]
    params <- lapply(params, eval, parent.frame(n=1))
    
    p <<- p + layer(geom=g, stat=s, geom_params=params, stat_params=params, position=ps)
  }, geom, stat, position)

  p
}
