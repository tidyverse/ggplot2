GeomSegment <- proto(Geom, {
  draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
    if (!coordinates$muncher()) {
      return(with(coordinates$transform(data), 
        segmentsGrob(x, y, xend, yend, default.units="native",
        gp=gpar(col=colour, lwd=size * .pt, lty=linetype), arrow = arrow)
      ))
    }

    data$group <- 1:nrow(data)
    starts <- subset(data, select = c(-xend, -yend))
    ends <- rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"))
    
    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group),]
    
    GeomPath$draw_groups(pieces, scales, coordinates, ...)
  }

  
  objname <- "segment"
  desc <- "Single line segments"
  icon <- function(.) segmentsGrob(c(0.1, 0.3, 0.5, 0.7), c(0.3, 0.5, 0.1, 0.9), c(0.2, 0.5, 0.7, 0.9), c(0.8, 0.7, 0.4, 0.3))

  desc_params <- list(
    arrow = "specification for arrow heads, as created by arrow()"
  )

  seealso <- list(
    geom_path = GeomPath$desc,
    geom_line = GeomLine$desc
  )

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "xend", "yend")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1)
  guide_geom <- function(.) "path"
  
  examples <- function(.) {
    require("maps")
    
    xlim <- range(seals$long)
    ylim <- range(seals$lat)
    usamap <- data.frame(map("world", xlim = xlim, ylim = ylim, plot =
    FALSE)[c("x","y")])
    usamap <- rbind(usamap, NA, data.frame(map('state', xlim = xlim, ylim
    = ylim, plot = FALSE)[c("x","y")]))
    names(usamap) <- c("long", "lat")
    
    p <- ggplot(seals, aes(x = long, y = lat))
    (p <- p + geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat), arrow=arrow(length=unit(0.1,"cm"))))
    p + geom_path(data = usamap) + scale_x_continuous(limits=xlim)
    
  }
  
})

