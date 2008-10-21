GeomPath <- proto(Geom, {
  draw_groups <- function(., ...) .$draw(...)

  draw <- function(., data, scales, coordinates, arrow = NULL, ...) {
    if (nrow(data) < 2) return()

    munched <- coordinates$munch(data, scales)

    # Work out whether we should use lines or segments
    g <- split(munched, munched$group)
    g <- g[sapply(g, nrow) >= 2]
    munched <- do.call(rbind, g[sapply(g, nrow) >= 2])
    if (is.null(munched)) return()

    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)  
    
    solid_lines <- all(sapply(g, function(df) identical(unique(df$linetype), 1)))
    constant <- all(sapply(g, function(df) nrow(unique(df[, c("colour","size","linetype")])) == 1))
    
    if (!solid_lines && !constant) {
      stop("geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line", call.=FALSE)
    }
    
    if (!constant) {
      with(munched, 
        segmentsGrob(x[!end], y[!end], x[!start], y[!start],
        default.units="native", arrow = arrow, 
        gp = gpar(
          col = colour[!end], fill = colour[!end], lwd = size[!end] * .pt, 
          lty = linetype[!end], lineend = "butt"
        ))
      )
    } else {
      with(munched, 
        polylineGrob(
          x, y, id = as.integer(factor(group)), 
          default.units="native", arrow = arrow, 
          gp = gpar(
            col = colour[start], fill = colour[start], lwd = size[start] * .pt, 
            lty = linetype[start], lineend = "butt")
        )
      )
    }
  }

  draw_legend <- function(., data, ...) {
    data$arrow <- NULL
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, 
      ggname(.$my_name(), segmentsGrob(0, 0.5, 1, 0.5, default.units="npc",
      gp=gpar(col=colour, lwd=size * .pt, lty=linetype, lineend="butt")))
    )
  }
  
  objname <- "path"
  desc <- "Connect observations, in original order"

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1)
  icon <- function(.) linesGrob(c(0.2, 0.4, 0.8, 0.6, 0.5), c(0.2, 0.7, 0.4, 0.1, 0.5))
  guide_geom <- function(.) "path"
  
  seealso <- list(
    geom_line = "Functional (ordered) lines", 
    geom_polygon = "Filled paths (polygons)",
    geom_segment = "Line segments"
  )

  examples <- function(.) {
    # Generate data
    myear <- do.call(rbind, by(movies, movies$year, function(df) data.frame(year=df$year[1], mean.length = mean(df$length), mean.rating=mean(df$rating))))
    p <- ggplot(myear, aes(x=mean.length, y=mean.rating))
    p + geom_path()

    # Add aesthetic mappings
    p + geom_path(aes(size=year))
    p + geom_path(aes(colour=year))
    
    # Change scale
    p + geom_path(aes(size=year)) + scale_size(to=c(1, 3))

    # Set aesthetics to fixed value
    p + geom_path(colour = "green")
    
    # Use qplot instead
    qplot(mean.length, mean.rating, data=myear, geom="path")
    
    # Using economic data:
    # How is unemployment and personal savings rate related?
    qplot(unemploy/pop, psavert, data=economics)
    qplot(unemploy/pop, psavert, data=economics, geom="path")
    qplot(unemploy/pop, psavert, data=economics, geom="path", size=as.numeric(date))

    # How is rate of unemployment and length of unemployment?
    qplot(unemploy/pop, uempmed, data=economics)
    qplot(unemploy/pop, uempmed, data=economics, geom="path")
    qplot(unemploy/pop, uempmed, data=economics, geom="path") +
      geom_point(data=head(economics, 1), colour="red") + 
      geom_point(data=tail(economics, 1), colour="blue")
    qplot(unemploy/pop, uempmed, data=economics, geom="path") +
      geom_text(data=head(economics, 1), label="1967", colour="blue") + 
      geom_text(data=tail(economics, 1), label="2007", colour="blue")
    
    # Setting line type vs colour/size
    # Line type needs to be applied to a line as a whole, so it can
    # not be used with colour or size that vary across a line
    
    x <- seq(0.01, .99, length=100)
    df <- data.frame(x = rep(x, 2), y = c(qlogis(x), 2 * qlogis(x)), group = rep(c("a","b"), each=100))
    p <- ggplot(df, aes(x=x, y=y, group=group))

    # Should work
    p + geom_line(linetype = 2)
    p + geom_line(aes(colour = group), linetype = 2)
    p + geom_line(aes(colour = x))
    
    # Should fail
    p + geom_line(aes(colour = x), linetype=2)
    
  }  
})

