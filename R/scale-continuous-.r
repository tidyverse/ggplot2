ScaleContinuous <- proto(Scale, {
  .domain <- c()
  .range <- c()
  .expand <- c(0.05, 0)
  limits <- c(NA, NA)
  .labels <- NULL

  new <- function(., name=NULL, limits=c(NA,NA), breaks=NULL, labels=NULL, variable, trans="identity", expand=c(0.05, 0)) {
    if (is.null(breaks) && !is.null(labels)) stop("Labels can only be specified in conjunction with breaks")
    
    .$proto(name=name, .input=variable, .output=variable, limits=limits, .breaks = breaks, .labels = labels, expand=expand)
  }

  domain <- function(.) {
    c(
      if(is.na(.$limits[1])) .$.domain[1] else .$.tr$transform(.$limits[1]),
      if(is.na(.$limits[2])) .$.domain[2] else .$.tr$transform(.$limits[2])
    )
  }
  
  stransform <- function(., values) {
    .$.tr$transform(values)
  }
  
  map <- function(., values) {
    rescale(values, .$frange(), .$domain())
  }
  
  train <- function(., x) {
    if (!is.numeric(x)) 
      warning("Non-numeric variable supplied to continuous scale ", .$name, ".", call.=FALSE)
    if (all(is.na(x))) return()
      
    .$.domain <- range(range(x, na.rm=TRUE, finite=TRUE), .$.domain, na.rm=TRUE, finite=TRUE)
  }

  # Scale range
  frange <- function(.) {
    nulldefault(.$.range, .$domain())
  }
  
  # Breaks are regular on the transformed scale
  .breaks <- NULL
  breaks <- function(.) {
    if (!is.null(.$.breaks)) return(.$.tr$transform(.$.breaks))
    grid.pretty(.$domain())
  }

  rbreaks <- function(.) {
    if (!is.null(.$.breaks)) return(.$.breaks)
    grid.pretty(.$frange())
  }

  .minor_breaks <- 2
  # Minor breaks are regular on the original scale
  # and need to cover entire range of plot
  minor_breaks <- function(., n = .$.minor_breaks, b = .$breaks()) {
    if (length(b) == 1) return(b)

    bd <- diff(b)[1]
    b <- c(b[1] - bd, b, b[length(b)] + bd)
    unlist(mapply(.$.tr$seq, b[-length(b)], b[-1], length=n+1, SIMPLIFY=F))
  }
  
  labels <- function(.) {
    if (!is.null(.$.labels)) return(.$.labels)
    b <- .$breaks()

    l <- .$.tr$label(b)
    numeric <- sapply(l, is.numeric)
    l[numeric] <- format(unlist(l[numeric]))
    l
  }
  
  test <- function(.) {
    m <- .$minor_breaks(10)
    b <- .$breaks()
    
    plot(x=0,y=0,xlim=range(c(b,m)), ylim=c(1,5), type="n", axes=F,xlab="", ylab="")
    for(i in 1:(length(b))) axis(1, b[[i]], as.expression(.$labels()[[i]]))
    
    abline(v=m)
    abline(v=b, col="red")
  }
  
  objname <- "continuous"
  common <- c("x", "y", "z", "xend", "yend")
  desc <- "Continuous position scale"
  seealso <- list(
    "scale_discrete" = "Discrete position scales"
  )
  examples <- function(.) {
    (m <- qplot(rating, votes, data=movies))
    
    # Manipulating the default position scales lets you:

    #  * change the axis labels
    m + scale_y_continuous("number of votes")
    m + scale_y_continuous(expression(votes^alpha))
    
    #  * modify the axis limits
    m + scale_y_continuous(limits=c(NA, 5000))
    m + scale_y_continuous(limits=c(1000, NA))
    m + scale_x_continuous(limits=c(7, 8))

    #  * choose where the ticks appear
    m + scale_x_continuous(breaks=1:10)
    m + scale_x_continuous(breaks=c(1,3,7,9))

    #  * manually label the ticks
    m + scale_x_continuous(breaks=c(1,5,10), labels=c("one", "five", "ten"))
    m + scale_x_continuous(breaks=c(1,5,10), labels=c("horrible", "ok", "awesome"))
    m + scale_x_continuous(breaks=c(1,5,10), labels=expression(Alpha, Beta, Omega))
    
    # There are also a wide range of transformations you can use:
    m + scale_y_log10()
    m + scale_y_log()
    m + scale_y_log2()
    m + scale_y_sqrt()
    # see ?transformer for a full list
    
    # qplot allows you to do some of this with a little less typing:
    #   * axis limits
    qplot(rating, votes, data=movies, xlim=c(5,10), ylim=c(50000, NA))
    #   * axis labels
    qplot(rating, votes, data=movies, xlab="My x axis", ylab="My y axis")
    #   * log scaling
    qplot(rating, votes, data=movies, log="xy")
  }
})

