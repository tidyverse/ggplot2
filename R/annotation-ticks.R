#' @title Annotation: tick marks
#' @description  This annotation adds tick marks to an axis
#' @export
#' @inheritParams ggplot2::annotation_logticks
#' @param ticks_per_base integer, number of minor ticks between each pair of major ticks, Default: base-1
#' @param delog boolean, if an idenity transformation is needed use set to TRUE, Default: FALSE
#' @examples
#' 
#' minorUtil=function(scale,side,ticks){
#'   data.frame(
#'     base =c('log'=exp(1),'log10'=10,'identity'=10)[scale],
#'     delog=scale=='identity',
#'     ticks_per_base=ticks+1,
#'     stringsAsFactors = FALSE
#'   )
#' }
#' 
#' minorTicks<-c('blt')
#' minorTickNum<-c(10,3,4)
#' yScale<-'identity'
#' xScale<-'log10'
#' 
#' tickMat=mapply(minorUtil,
#'                side=strsplit(minorTicks,'')[[1]],
#'                scale=c(b=xScale,t=xScale,l=yScale,r=yScale)[strsplit(minorTicks,'')[[1]]],
#'                ticks=minorTickNum,SIMPLIFY = FALSE)%>%
#'   plyr::ldply(.id='sides')%>%dplyr::mutate(sides=as.character(sides))
#' 
#' p<-ggplot(msleep, aes(bodywt, brainwt)) + geom_point() + scale_x_log10()+theme_bw()
#' 
#' p+annotation_ticks(base = tickMat$base, ticks_per_base = tickMat$ticks, sides = tickMat$sides, delog  = tickMat$delog, colour=c('red'))
#' 
#' yScale<-'log10'
#' 
#' tickMat=mapply(minorUtil,
#'                side=strsplit(minorTicks,'')[[1]],
#'                scale=c(b=xScale,t=xScale,l=yScale,r=yScale)[strsplit(minorTicks,'')[[1]]],
#'                ticks=minorTickNum,SIMPLIFY = FALSE)%>%
#'   plyr::ldply(.id='sides')%>%dplyr::mutate(sides=as.character(sides))
#' 
#' p <- p+ scale_y_log10()
#' 
#' p+annotation_ticks(base = tickMat$base, ticks_per_base = tickMat$ticks, sides = tickMat$sides, delog  = tickMat$delog, colour=c('red'))
#' @import ggplot2
#' @importFrom grid unit convertUnit gpar segmentsGrob gTree
annotation_ticks <- function (base = 10, sides = 'b', scaled = TRUE, short = grid::unit(0.1,"cm"),
                              mid = grid::unit(0.2, "cm"), long = grid::unit(0.3, "cm"), colour = "black", 
                              size = 0.5, linetype = 1, alpha = 1, color = NULL, ticks_per_base=base-1,delog=FALSE,...) 
{
  if (!is.null(color)) 
    colour <- color
  ggplot2::layer(data = data.frame(x = NA), mapping = NULL, stat = ggplot2::StatIdentity, 
                 geom = GeomTicks, position = ggplot2::PositionIdentity, show.legend = FALSE, 
                 inherit.aes = FALSE, params = list(base = base, sides = sides, 
                                                    scaled = scaled, short = short, mid = mid, long = long, 
                                                    colour = colour, size = size, linetype = linetype, 
                                                    alpha = alpha,ticks_per_base=ticks_per_base,delog=delog,...))
}

environment(annotation_ticks)=asNamespace('ggplot2')

#' @export
GeomTicks<- ggplot2::ggproto("GeomTicks", ggplot2::Geom,
                             extra_params = "",
                             handle_na = function(data, params) {
                               data
                             },
                             
                             draw_panel = function(data, panel_scales, coord, base = c(10,10), sides = c('b','l'),
                                                   scaled = TRUE, 
                                                   short = grid::unit(0.1, "cm"),
                                                   mid = grid::unit(0.2, "cm"),
                                                   long = grid::unit(0.3, "cm"),
                                                   ticks_per_base=base-1,delog=c(x=FALSE,y=FALSE))
                             {
                               ticks <- list()
                               
                               # Convert these units to numbers so that they can be put in data frames
                               short <- grid::convertUnit(short, "cm", valueOnly = TRUE)
                               mid   <- grid::convertUnit(mid,   "cm", valueOnly = TRUE)
                               long  <- grid::convertUnit(long,  "cm", valueOnly = TRUE)
                               
                               for(s in 1:length(sides)){
                                 
                                 if (grepl("[b|t]", sides[s])) {
                                   # Get positions of x tick marks
                                   
                                   xticks <- calc_ticks(
                                     base = base[s],
                                     minpow = floor(panel_scales$x.range[1]),
                                     maxpow = ceiling(panel_scales$x.range[2]),
                                     majorTicks=panel_scales$x.major_source,
                                     start = 0,
                                     shortend = short,
                                     midend = mid,
                                     longend = long,
                                     ticks_per_base = ticks_per_base[s],
                                     delog=delog[s]
                                   )
                                   
                                   
                                   if (scaled){
                                     if(!delog[s]) xticks$value <- log(xticks$value, base[s])
                                   }
                                   
                                   
                                   names(xticks)[names(xticks) == "value"] <- "x"   # Rename to 'x' for coordinates$transform
                                   xticks <- coord$transform(xticks, panel_scales)
                                   
                                   # Make the grobs
                                   if (grepl("b", sides[s])) {
                                     ticks$x_b <- with(data, segmentsGrob(
                                       x0 = grid::unit(xticks$x, "native"), x1 = grid::unit(xticks$x, "native"),
                                       y0 = grid::unit(xticks$start, "cm"), y1 = grid::unit(xticks$end, "cm"),
                                       gp = grid::gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
                                     ))
                                   }
                                   if (grepl("t", sides[s])) {
                                     ticks$x_t <- with(data, 
                                                       grid::segmentsGrob(
                                                         x0 = grid::unit(xticks$x, "native"), 
                                                         x1 = grid::unit(xticks$x, "native"),
                                                         y0 = grid::unit(1, "npc") - grid::unit(xticks$start, "cm"), 
                                                         y1 = unit(1, "npc") - grid::unit(xticks$end, "cm"),
                                                         gp = grid::gpar(col = ggplot2::alpha(colour, alpha), lty = linetype, lwd = size * .pt)
                                                       ))
                                   }
                                 }
                                 
                                 
                                 if (grepl("[l|r]", sides[s])) {
                                   yticks <- calc_ticks(
                                     base = base[s],
                                     minpow = floor(panel_scales$y.range[1]),
                                     maxpow = ceiling(panel_scales$y.range[2]),
                                     majorTicks=panel_scales$y.major_source,
                                     start = 0,
                                     shortend = short,
                                     midend = mid,
                                     longend = long,
                                     ticks_per_base = ticks_per_base[s],
                                     delog=delog[s]
                                   )
                                   
                                   if (scaled){
                                     if(!delog[s]) yticks$value <- log(yticks$value, base[s])
                                   }
                                   
                                   
                                   names(yticks)[names(yticks) == "value"] <- "y"   # Rename to 'y' for coordinates$transform
                                   yticks <- coord$transform(yticks, panel_scales)
                                   
                                   # Make the grobs
                                   if (grepl("l", sides[s])) {
                                     ticks$y_l <- with(data, grid::segmentsGrob(
                                       y0 = grid::unit(yticks$y, "native"), y1 = grid::unit(yticks$y, "native"),
                                       x0 = grid::unit(yticks$start, "cm"), x1 = grid::unit(yticks$end, "cm"),
                                       gp = grid::gpar(col = ggplot2::alpha(colour, alpha), lty = linetype, lwd = size * .pt)
                                     ))
                                   }
                                   if (grepl("r", sides[s])) {
                                     ticks$y_r <- with(data, 
                                                       grid::segmentsGrob(
                                                         y0 = grid::unit(yticks$y, "native"), 
                                                         y1 = grid::unit(yticks$y, "native"),
                                                         x0 = grid::unit(1, "npc") - grid::unit(yticks$start, "cm"), 
                                                         x1 = grid::unit(1, "npc") - grid::unit(yticks$end, "cm"),
                                                         gp = grid::gpar(col = ggplot2::alpha(colour, alpha), lty = linetype, lwd = size * .pt)
                                                       ))
                                   }
                                 }
                               }
                               grid::gTree(children = do.call("gList", ticks))
                             },
                             
                             default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1, alpha = 1)
)


# Calculate the position of log tick marks
# Returns data frame with:
# - value: the position of the log tick on the data axis, for example 1, 2, ..., 9, 10, 20, ...
# - start: on the other axis, start position of the line (usually 0)
# - end: on the other axis, end position of the line (for example, .1, .2, or .3)
#' @export
calc_ticks <- function(base = 10, 
                       ticks_per_base = base - 1,
                       minpow = 0, maxpow = minpow + 1,
                       majorTicks=0, 
                       start = 0, shortend = .1, midend = .2, longend = .3,
                       delog=FALSE) {
  
  # Number of blocks of tick marks
  reps <- maxpow - minpow
  
  # For base 10: 1, 2, 3, ..., 7, 8, 9, 1, 2, ...
  ticknums  <- rep(seq(1, base - 1, length.out = ticks_per_base), reps)
  
  # For base 10: 1, 1, 1, ..., 1, 1, 1, 2, 2, ... (for example)
  powers <- rep(seq(minpow, maxpow - 1), each = ticks_per_base)
  
  
  ticks  <- ticknums * base ^ powers
  ticks  <- c(ticks, base ^ maxpow)  # Add the last tick mark    
  
  
  # Set all of the ticks short
  tickend <- rep(shortend, length(ticks))
  
  # Get the position within each cycle, 0, 1, 2, ..., 8, 0, 1, 2. ...
  cycleIdx <- ticknums - 1
  
  # Set the "major" ticks long
  tickend[cycleIdx == 0] <- longend
  
  # Where to place the longer tick marks that are between each base
  # For base 10, this will be at each 5
  longtick_after_base <- floor(ticks_per_base/2)
  tickend[ cycleIdx == longtick_after_base ] <- midend 
  
  if(delog){
    ticksCopy=ticks
    regScale=log(ticks,base)
    majorTicks=sort(unique(c(minpow,regScale[which(regScale%in%majorTicks)],maxpow,majorTicks)))
    expandScale=c()
    if(length(majorTicks)>1){
      for(i in 1:(length(majorTicks)-1)) {
        expandScale=c(expandScale,seq(majorTicks[i],majorTicks[i+1],length.out=(ticks_per_base+1)))
      }
      ticks=unique(expandScale)
      # Set all of the ticks short
      tickend <- rep(shortend, length(ticks))
      # Set the "major" ticks long
      tickend[which(ticks%in%majorTicks)] <- longend
    }
  }
  
  tickdf <- data.frame(value = ticks, start = start, end = tickend)
  
  return(tickdf)
}

environment(GeomTicks)=asNamespace('ggplot2')