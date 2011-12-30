#' Make a parallel coordinates plot.
#' 
#' One way to think about a parallel coordinates plot, is as plotting 
#' the data after it has been transformed to gain a new variable.  This
#' function does this using \code{\link[reshape]{melt}}.
#' 
#' This gives us enormous flexibility as we have separated out the 
#' type of drawing (lines by tradition) and can now use any of the existing
#' geom functions.  In particular this makes it very easy to create parallel
#' boxplots, as shown in the example.
#' 
#' @param data data frame
#' @param vars variables to include in parallel coordinates plot
#' @param ... other arguments passed on plot creation
#' @export
#' @examples
#' ggpcp(mtcars) + geom_line()
#' ggpcp(mtcars, vars=names(mtcars[2:6])) + geom_line()
#' ggpcp(mtcars) + geom_boxplot(aes(group=variable))
#'
#' p <- ggpcp(mtcars, vars=names(mtcars[2:6]))
#' p + geom_line()
#' p + geom_line(aes(colour=mpg)) 
ggpcp <- function(data, vars=names(data), ...) {
  
  scaled <- as.data.frame(lapply(data[, vars], rescale01))
  data <- cunion(scaled, data)
  
  data$ROWID <- 1:nrow(data)
  molten <- melt(data, m=vars)

  ggplot(molten, aes_string(x = "variable", y = "value", group = "ROWID"),
    ...)
}

#' Create a fluctuation plot.
#' 
#' A fluctutation diagram is a graphical representation of a contingency
#' table.  This fuction currently only supports 2D contingency tabless
#' but extension to more should be relatively straightforward.
#' 
#' With the default size fluctuation diagram, area is proportional to the 
#' count (length of sides proportional to sqrt(count))
#' 
#' @param table a table of values, or a data frame with three columns, 
#'   the last column being frequency
#' @param type "size", or "colour" to create traditional heatmap
#' @param floor don't display cells smaller than this value
#' @param ceiling round cells to at most this value
#' @param na.rm If \code{TRUE}, silently remove missing values.
#' @export
#' @examples
#' ggfluctuation(table(movies$Action, movies$Comedy))
#' ggfluctuation(table(movies$Action, movies$mpaa))
#' ggfluctuation(table(movies$Action, movies$Comedy), type="colour")
#' ggfluctuation(table(warpbreaks$breaks, warpbreaks$tension))
ggfluctuation <- function(table, type="size", floor=0, ceiling=max(table$freq, na.rm=TRUE)) {
  if (is.table(table)) table <- as.data.frame(t(table))
  
  oldnames <- names(table)
  names(table) <- c("x","y", "result")
  
  table <- transform(table,
    x = as.factor(x),
    y = as.factor(y),
    freq = result
 )

  if (type =="size") {
    table <- transform(table, 
      freq = sqrt(pmin(freq, ceiling) / ceiling),
      border = ifelse(is.na(freq), "grey90", ifelse(freq > ceiling, "grey30", "grey50"))
    )
    table[is.na(table$freq), "freq"] <- 1
    table <- subset(table, freq * ceiling >= floor)
  }

  if (type=="size") {
    nx <- length(levels(table$x))
    ny <- length(levels(table$y))
    
    p <- ggplot(table, 
      aes_string(x="x", y="y", height="freq", width="freq", fill="border")) +
      geom_tile(colour="white") + 
      scale_fill_identity() + 
      opts(aspect.ratio = ny / nx)

      # geom_rect(aes(xmin = as.numeric(x), ymin = as.numeric(y), xmax = as.numeric(x) + freq, ymax = as.numeric(y) + freq), colour="white") + 
    
  } else {
    p <- ggplot(table, aes_string(x="x", y="y", fill="freq")) + 
      geom_tile(colour="grey50") +
      scale_fill_gradient2(low="white", high="darkgreen")
  }

  p$xlabel <- oldnames[1]
  p$ylabel <- oldnames[2]
  p
}

#' Create a plot to illustrate patterns of missing values.
#' 
#' The missing values plot is a useful tool to get a rapid
#' overview of the number of missings in a dataset.  It's strength
#' is much more apparent when used with interactive graphics, as you can
#' see in Mondrian (\url{http://rosuda.org/mondrian}) where this plot was
#' copied from.
#' 
#' @param data input data.frame
#' @param avoid whether missings should be stacked or dodged, see
#'    \code{\link{geom_bar}} for more details
#' @param order if \code{TRUE}, order variables by number of missings
#' @param missing.only if \code{TRUE}, only display variables with some
#'   missing data
#' @seealso \code{\link{ggstructure}}, \code{\link{ggorder}}
#' @export
#' @examples
#' mmissing <- movies
#' mmissing[sample(nrow(movies), 1000), sample(ncol(movies), 5)] <- NA
#' ggmissing(mmissing)
#' ggmissing(mmissing, order=FALSE, missing.only = FALSE)
#' ggmissing(mmissing, avoid="dodge") + scale_y_sqrt()
ggmissing <- function(data, avoid="stack", order=TRUE, missing.only = TRUE) {
  missings <- mapply(function(var, name) cbind(as.data.frame(table(missing=factor(is.na(var), levels=c(TRUE, FALSE), labels=c("yes", "no")))), variable=name), 
    data, names(data), SIMPLIFY=FALSE
  )
  df <- do.call("rbind", missings)
  
  prop <- df[df$missing == "yes", "Freq"] / (df[df$missing == "no", "Freq"] + df[df$missing == "yes", "Freq"])
  df$prop <- rep(prop, each=2)
  
  if (order) {
    var <- df$variable
    var <- factor(var, levels = levels(var)[order(1 - prop)])
    df$variable <- var
  }

  if (missing.only) {
    df <- df[df$prop > 0 & df$prop < 1, , drop=FALSE]
    df$variable <- factor(df$variable)
  }
  
  ggplot(df, aes_string(y="Freq", x="variable", fill="missing")) + geom_bar(position=avoid)
}

#' A plot which aims to reveal gross structural anomalies in the data.
#' 
#' @param data data set to plot
#' @param scale type of scaling to use.  See \code{\link[reshape]{rescaler}}
#'   for options
#' @export
#' @examples
#' ggstructure(mtcars)
ggstructure <- function(data, scale = "rank") {
  ggpcp(data, scale=scale) + 
    aes_string(y="ROWID", fill="value", x="variable") +
    geom_tile() +
    scale_y_continuous("row number", expand = c(0, 1)) +
    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0)
}

#' A plot to investigate the order in which observations were recorded.
#' 
#' @param data data set to plot
#' @param scale type of scaling to use.  See \code{\link[reshape]{rescaler}}
#'   for options
#' @export
ggorder <- function(data, scale="rank") {
  ggpcp(data, scale="rank") +
    aes_string(x="ROWID", group="variable", y="value") +
    facet_grid(. ~ variable) +
    geom_line() +
    scale_x_continuous("row number")
}

# Distribution plot.
ggdist <- function(data, vars=names(data), facets = . ~ .) {
  cat <- sapply(data[vars], is.factor)
  facets <- deparse(substitute(facets))
  
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(ncol = ncol(data))))
  
  mapply(function(name, cat, i) {
    p <- ggplot(data) + 
      facet_grid(facets) +
      aes_string(x=name, y=1) +
      geom_bar()

    pushViewport(viewport(layout.pos.col=i))
    grid.draw(ggplotGrob(p))
    popViewport()
  }, names(data[vars]), cat, 1:ncol(data[vars]))
  invisible()
  
}
