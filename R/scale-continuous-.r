ScaleContinuous <- proto(Scale, funEnvir = globalenv(), {  
  .domain <- c()
  .range <- c()
  .expand <- c(0.05, 0)
  .labels <- NULL
  discrete <- function(.) FALSE
  
  tr_default <- "identity"

  new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, variable, trans = NULL, expand=c(0.05, 0), ...) {
    if (is.null(breaks) && !is.null(labels)) stop("Labels can only be specified in conjunction with breaks")
    
    if (is.null(trans))      trans <- .$tr_default
    if (is.character(trans)) trans <- Trans$find(trans)
    
    # Transform limits and breaks
    limits <- trans$transform(limits)
    breaks <- trans$transform(breaks)
    
    .$proto(name=name, .input=variable, .output=variable, limits=limits, breaks = breaks, .labels = labels, .expand=expand, .tr = trans, ...)
  }

  
  # Transform each 
  transform_df <- function(., df) {
    input <- .$input()
    output <- .$output()
    transform <- function(var) .$.tr$transform(df[, var])
    
    # If y transformed, also need to transform min and max
    if (length(input) == 1) {
      if (input == "y") {
        input <- output <- intersect(c("y","min", "max", "yend"), names(df))
      } else if (input == "x") {
        input <- output <- intersect(c("x", "xend"), names(df))        
      }
    }
    df <- do.call("data.frame", lapply(input, transform))
    if (ncol(df) == 0) return(NULL)
    names(df) <- output      
    df
  }
  
  train <- function(., x) {
    if (is.null(x)) return()
    if (!is.numeric(x)) 
      warning("Non-continuous variable supplied to continuous ", .$my_name(), ".", call.=FALSE)
    if (all(is.na(x))) return()
    
    .$.domain <- range(x, .$.domain, na.rm=TRUE, finite=TRUE)
  }
    
  # By default, a continuous scale does no transformation in the mapping stage
  # See scale_size for an exception
  map <- function(., values)  values

  # By default, the range of a continuous scale is the same as its
  # (transformed) domain
  output_set <- function(.) .$input_set()
  
  # By default, breaks are regularly spaced along the (transformed) domain
  breaks <- NULL
  input_breaks <- function(.) {
    nulldefault(.$breaks, grid.pretty(.$input_set()))
  }
  output_breaks <- function(.) .$map(.$input_breaks())

  .minor_breaks <- 2
  # Minor breaks are regular on the original scale
  # and need to cover entire range of plot
  minor_breaks <- function(., n = .$.minor_breaks, b = .$input_breaks(), r = .$output_set()) {
    if (length(b) == 1) return(b)
    
    bd <- diff(b)[1]
    if (min(r) < min(b)) b <- c(b[1] - bd, b)
    if (max(r) > max(b)) b <- c(b, b[length(b)] + bd)
    unique(unlist(mapply(.$.tr$seq, b[-length(b)], b[-1], length=n+1, SIMPLIFY=F)))
  }
  
  labels <- function(.) {
    if (!is.null(.$.labels)) return(.$.labels)
    b <- .$input_breaks()

    l <- .$.tr$label(b)
    numeric <- sapply(l, is.numeric)
    l[numeric] <- format(unlist(l[numeric]))
    l
  }
  
  test <- function(.) {
    m <- .$minor_breaks(10)
    b <- .$input_breaks()
    
    plot(x=0,y=0,xlim=range(c(b,m)), ylim=c(1,5), type="n", axes=F,xlab="", ylab="")
    for(i in 1:(length(b))) axis(1, b[[i]], as.expression(.$labels()[[i]]))
    
    abline(v=m)
    abline(v=b, col="red")
  }
  
  objname <- "continuous"
  common <- c("x", "y", "z")
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
    m + scale_y_continuous(limits=c(0, 5000))
    m + scale_y_continuous(limits=c(1000, 10000))
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

# Set x limits
# Convenience function to set the limits of the x axis.
# 
# Works by creating a new continuous scale, so will only work for 
# continuous variables.
# 
# @arguments lower limit
# @arguments upper limit
# @keyword hplot
#X qplot(mpg, wt, data=mtcars) + xlim(15, 20)
xlim <- function(min=NA, max=NA) {
  scale_x_continuous(limits = c(min, max))
}

# Set y limits
# Convenience function to set the limits of the y axis.
# 
# Works by creating a new continuous scale, so will only work for 
# continuous variables.
# 
# @arguments lower limit
# @arguments upper limit
# @keyword hplot
#X qplot(mpg, wt, data=mtcars) + ylim(15, 20)
ylim <- function(min=NA, max=NA) {
  scale_y_continuous(limits = c(min, max))
}

# Set z limits
# Convenience function to set the limits of the z axis.
# 
# Works by creating a new continuous scale, so will only work for 
# continuous variables.
# 
# @arguments lower limit
# @arguments upper limit
# @keyword hplot
zlim <- function(min=NA, max=NA) {
  scale_z_continuous(limits = c(min, max))
}
