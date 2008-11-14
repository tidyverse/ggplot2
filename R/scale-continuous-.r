ScaleContinuous <- proto(Scale, funEnvir = globalenv(), {  
  .domain <- c()
  .range <- c()
  .expand <- c(0.05, 0)
  .labels <- NULL
  discrete <- function(.) FALSE
  
  tr_default <- "identity"

  new <- function(., name=NULL, limits=NULL, breaks=NULL, labels=NULL, variable, trans = NULL, expand=c(0.05, 0), minor_breaks = NULL, formatter = scientific, ...) {
    if (is.null(breaks) && !is.null(labels)) stop("Labels can only be specified in conjunction with breaks")
    
    if (is.null(trans))      trans <- .$tr_default
    if (is.character(trans)) trans <- Trans$find(trans)
    
    # Transform limits and breaks
    limits <- trans$transform(limits)
    breaks <- trans$transform(breaks)
    minor_breaks <- trans$transform(minor_breaks)
    
    .$proto(name=name, .input=variable, .output=variable, limits=limits, breaks = breaks, .labels = labels, .expand=expand, .tr = trans, minor_breaks = minor_breaks, formatter = formatter, ...)
  }

  
  # Transform each 
  transform_df <- function(., df) {
    input <- .$input()
    output <- .$output()
    
    if (length(input) == 1 && input %in% c("x", "y")) {
      matches <- aes_to_scale(names(df)) == input
      input <- output <- names(df)[matches]
    }
    input <- intersect(input, names(df))

    df <- colwise(.$.tr$transform)(df[input])
    if (ncol(df) == 0) return(NULL)
    names(df) <- output      
    df
  }
  
  train <- function(., x) {
    if (is.null(x)) return()
    if (!is.numeric(x)) {
      stop(
        "Non-continuous variable supplied to ", .$my_full_name(), ".",
        call.=FALSE
      )
    }
    if (all(is.na(x))) return()
    
    .$.domain <- range(x, .$.domain, na.rm=TRUE, finite=TRUE)
  }
    
  # By default, a continuous scale does no transformation in the mapping stage
  # See scale_size for an exception
  map <- function(., values) {
    as.numeric(ifelse(values %inside% .$output_set(), values, NA))
  }

  # By default, the range of a continuous scale is the same as its
  # (transformed) domain
  output_set <- function(.) .$input_set()
  
  # By default, breaks are regularly spaced along the (transformed) domain
  input_breaks <- function(.) {
    nulldefault(.$breaks, .$.tr$input_breaks(.$input_set()))
  }
  input_breaks_n <- function(.) .$input_breaks()


  minor_breaks <- NULL
  output_breaks <- function(., n = 2, b = .$input_breaks(), r = .$output_set()) {
    nulldefault(.$minor_breaks, .$.tr$output_breaks(n, b, r))
  }
  
  labels <- function(.) {
    if (!is.null(.$.labels)) return(.$.labels)
    b <- .$input_breaks()

    l <- .$.tr$label(b)
    numeric <- sapply(l, is.numeric)
    
    f <- match.fun(get("formatter", .))
    l[numeric] <- f(unlist(l[numeric]))
    l
  }
  
  test <- function(.) {
    m <- .$output_breaks(10)
    b <- .$input_breaks()
    
    plot(x=0,y=0,xlim=range(c(b,m)), ylim=c(1,5), type="n", axes=F,xlab="", ylab="")
    for(i in 1:(length(b))) axis(1, b[[i]], as.expression(.$labels()[[i]]))
    
    abline(v=m)
    abline(v=b, col="red")
  }
  
  objname <- "continuous"
  common <- c("x", "y")
  desc <- "Continuous position scale"
  seealso <- list(
    "scale_discrete" = "Discrete position scales"
  )
  examples <- function(.) {
    (m <- qplot(rating, votes, data=subset(movies, votes > 1000)))
    
    # Manipulating the default position scales lets you:

    #  * change the axis labels
    m + scale_y_continuous("number of votes")
    m + scale_y_continuous(expression(votes^alpha))
    
    #  * modify the axis limits
    m + scale_y_continuous(limits=c(0, 5000))
    m + scale_y_continuous(limits=c(1000, 10000))
    m + scale_x_continuous(limits=c(7, 8))
    
    # you can also use the short hand functions xlim and ylim
    m + ylim(0, 5000)
    m + ylim(1000, 10000)
    m + xlim(7, 8)

    #  * choose where the ticks appear
    m + scale_x_continuous(breaks=1:10)
    m + scale_x_continuous(breaks=c(1,3,7,9))

    #  * manually label the ticks
    m + scale_x_continuous(breaks=c(2,5,8), labels=c("two", "five", "eight"))
    m + scale_x_continuous(breaks=c(2,5,8), labels=c("horrible", "ok", "awesome"))
    m + scale_x_continuous(breaks=c(2,5,8), labels=expression(Alpha, Beta, Omega))
    
    # There are also a wide range of transformations you can use:
    m + scale_y_log10()
    m + scale_y_log()
    m + scale_y_log2()
    m + scale_y_sqrt()
    m + scale_y_reverse()
    # see ?transformer for a full list
    
    # You can control the formatting of the labels with the formatter
    # argument.  Some common formats are built in:
    x <- rnorm(10) * 100000
    y <- seq(0, 1, length = 10)
    p <- qplot(x, y)
    p + scale_y_continuous(formatter = "percent")
    p + scale_y_continuous(formatter = "dollar")
    p + scale_x_continuous(formatter = "comma")
    
    # qplot allows you to do some of this with a little less typing:
    #   * axis limits
    qplot(rating, votes, data=movies, ylim=c(1e4, 5e4))
    #   * axis labels
    qplot(rating, votes, data=movies, xlab="My x axis", ylab="My y axis")
    #   * log scaling
    qplot(rating, votes, data=movies, log="xy")
  }
})

