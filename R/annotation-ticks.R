#' Annotation: tick marks
#'
#' This annotation adds tick marks to an axis
#'
#' @export
#' @inheritParams annotation_logticks
#' @param scale character, vector of type of scale attributed to the side, Default: rep('identity',length(sides))
#' @param ticks_per_base integer, number of minor ticks between each pair of major ticks, Default: rep(10,length(sides))
#' @param delog boolean, if an idenity transformation is needed use set to TRUE, Default: rep(FALSE,length(sides))
#' @examples
#'
#' p<-ggplot(msleep, aes(bodywt, brainwt)) + geom_point()
#'
#' #Default behavior
#'
#' #add identity scale minor ticks on y axis
#' p+annotation_ticks(sides=c('l'))
#'
#' #add identity scale minor ticks on x,y axis
#' p+annotation_ticks(sides=c('lb'))
#'
#' #Control number of minor ticks of each side independently
#'
#' #add identity scale minor ticks on x,y axis
#' p+annotation_ticks(sides=c('l','b'),ticks_per_base=c(10,5))
#'
#' #log10 scale
#' p1 <- p + scale_x_log10()
#'
#' #add minor ticks on log10 scale
#' p1+annotation_ticks(sides=c('b'),scale='log10')
#'
#' #add minor ticks on both scales
#' p1+annotation_ticks(sides=c('l','b'),scale=c('identity','log10'))
#'
#' #add minor ticks on both scales, but force x axis to be identity
#' p1+annotation_ticks(sides=c('l','b'),scale=c('identity','log10'),delog=c(TRUE,TRUE))
#'
#'
#' #log scale
#' p2 <- p + scale_x_continuous(trans='log')
#'
#' #add minor ticks on log scale
#' p2+annotation_ticks(sides=c('b'),scale=c('log'))
#'
#' #add minor ticks on both scales
#' p2+annotation_ticks(sides=c('l','b'),scale=c('identity','log'))
#'
#' #add minor ticks on both scales, but force x axis to be identity
#' p2+annotation_ticks(sides=c('l','b'),scale=c('identity','log'),delog=c(TRUE,TRUE))
#'
#' @import grid
annotation_ticks <- function(sides = "b",
                             scale = rep('identity',length(sides)) ,
                             scaled = TRUE,
                             short = grid::unit(0.1,"cm"),
                             mid = grid::unit(0.2, "cm"),
                             long = grid::unit(0.3, "cm"),
                             colour = "black",
                             size = 0.5,
                             linetype = 1,
                             alpha = 1,
                             color = NULL,
                             ticks_per_base = rep(10,length(sides)),
                             delog = rep(FALSE,length(sides)),
                             ...) {

    if (!is.null(color))
        colour <- color

    # if(!'scale'%in%names(match.call())){
    #
    #   scale <- sapply(sides,function(x){
    #     if(grepl('[b|t]',x))
    #
    #          # is there a way to learn what trans is set to at this stage
    #          # from the object annotation is be applied to?
    #          # if so then user wont need to supply param to specify non identity scale
    #
    #   })
    #
    # }

    base<-sapply(scale,function(x) switch(x,'identity'=10,'log10'=10,'log'=exp(1)),USE.NAMES = FALSE)

    if(!'delog'%in%names(match.call())) delog<-scale%in%'identity'


    layer(
      data = data.frame(x = NA),
      mapping = NULL,
      stat = StatIdentity,
      geom = GeomTicks,
      position = PositionIdentity,
      show.legend = FALSE,
      inherit.aes = FALSE,
      params = list(base = base,
                    sides = sides,
                    scaled = scaled,
                    short = short,
                    mid = mid,
                    long = long,
                    colour = colour,
                    size = size,
                    linetype = linetype,
                    alpha = alpha,
                    ticks_per_base = ticks_per_base,
                    delog = delog,
                    ...)
      )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTicks <- ggproto("GeomTicks", Geom,
    extra_params = "",
    handle_na = function(data,params) {
    data
},

draw_panel = function(
  data,
  panel_scales,
  coord,
  base = c(10, 10),
  sides = c("b","l"),
  scaled = TRUE,
  short = grid::unit(0.1, "cm"),
  mid = grid::unit(0.2, "cm"),
  long = grid::unit(0.3, "cm"),
  ticks_per_base = base - 1,
  delog = c(x = TRUE, y = TRUE)){

    ticks <- list()

    # Convert these units to numbers so that they can be put in data frames
    short <- grid::convertUnit(short, "cm", valueOnly = TRUE)
    mid <- grid::convertUnit(mid, "cm", valueOnly = TRUE)
    long <- grid::convertUnit(long, "cm", valueOnly = TRUE)

    for (s in 1:length(sides)) {
        if (grepl("[b|t]", sides[s])) {

            # Get positions of x tick marks
            xticks <- calc_ticks(
              base = base[s],
              minpow = floor(panel_scales$x.range[1]),
              maxpow = ceiling(panel_scales$x.range[2]),
              majorTicks = panel_scales$x.major_source,
              start = 0,
              shortend = short,
              midend = mid,
              longend = long,
              ticks_per_base = ticks_per_base[s],
              delog = delog[s])

            if (scaled) {
                if (!delog[s])
                  xticks$value <- log(xticks$value, base[s])
            }

            names(xticks)[names(xticks) == "value"] <- "x"  # Rename to 'x' for coordinates$transform

            xticks <- coord$transform(xticks, panel_scales)

            # Make the grobs
            if (grepl("b", sides[s])) {
                ticks$x_b <- with(data, segmentsGrob(x0 = grid::unit(xticks$x, "native"),
                  x1 = grid::unit(xticks$x, "native"), y0 = grid::unit(xticks$start,
                    "cm"), y1 = grid::unit(xticks$end, "cm"), gp = grid::gpar(col = alpha(colour,
                    alpha), lty = linetype, lwd = size * .pt)))
            }
            if (grepl("t", sides[s])) {
                ticks$x_t <- with(data, grid::segmentsGrob(x0 = grid::unit(xticks$x,
                  "native"), x1 = grid::unit(xticks$x, "native"), y0 = grid::unit(1,
                  "npc") - grid::unit(xticks$start, "cm"), y1 = unit(1, "npc") - grid::unit(xticks$end,
                  "cm"), gp = grid::gpar(col = alpha(colour, alpha), lty = linetype,
                  lwd = size * .pt)))
            }
        }


        if (grepl("[l|r]", sides[s])) {
            yticks <- calc_ticks(
              base = base[s],
              minpow = floor(panel_scales$y.range[1]),
              maxpow = ceiling(panel_scales$y.range[2]),
              majorTicks = panel_scales$y.major_source,
              start = 0,
              shortend = short,
              midend = mid,
              longend = long,
              ticks_per_base = ticks_per_base[s],
              delog = delog[s])

            if (scaled) {
                if (!delog[s])
                  yticks$value <- log(yticks$value, base[s])
            }

            names(yticks)[names(yticks) == "value"] <- "y"  # Rename to 'y' for coordinates$transform
            yticks <- coord$transform(yticks, panel_scales)

            # Make the grobs
            if (grepl("l", sides[s])) {
                ticks$y_l <- with(data, grid::segmentsGrob(y0 = grid::unit(yticks$y,
                  "native"), y1 = grid::unit(yticks$y, "native"), x0 = grid::unit(yticks$start,
                  "cm"), x1 = grid::unit(yticks$end, "cm"), gp = grid::gpar(col = alpha(colour,
                  alpha), lty = linetype, lwd = size * .pt)))
            }
            if (grepl("r", sides[s])) {
                ticks$y_r <- with(data, grid::segmentsGrob(y0 = grid::unit(yticks$y,
                  "native"), y1 = grid::unit(yticks$y, "native"), x0 = grid::unit(1,
                  "npc") - grid::unit(yticks$start, "cm"), x1 = grid::unit(1, "npc") -
                  grid::unit(yticks$end, "cm"), gp = grid::gpar(col = alpha(colour,
                  alpha), lty = linetype, lwd = size * .pt)))
            }
        }
    }
    grid::gTree(children = do.call("gList", ticks))
}, default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = 1))


# Calculate the position of log tick marks Returns data frame with: - value: the
# position of the log tick on the data axis, for example 1, 2, ..., 9, 10, 20, ...
# - start: on the other axis, start position of the line (usually 0) - end: on the
# other axis, end position of the line (for example, .1, .2, or .3)
calc_ticks <- function(
  base = 10,
  ticks_per_base = base - 1,
  minpow = 0,
  maxpow = minpow + 1,
  majorTicks = 0,
  start = 0,
  shortend = 0.1,
  midend = 0.2,
  longend = 0.3,
  delog = FALSE){

    # Number of blocks of tick marks
    reps <- maxpow - minpow

    # For base 10: 1, 2, 3, ..., 7, 8, 9, 1, 2, ...
    ticknums <- rep(seq(1, base - 1, length.out = ticks_per_base), reps)

    # For base 10: 1, 1, 1, ..., 1, 1, 1, 2, 2, ... (for example)
    powers <- rep(seq(minpow, maxpow - 1), each = ticks_per_base)

    ticks <- ticknums * base^powers

    ticks <- c(ticks, base^maxpow)  # Add the last tick mark

    # Set all of the ticks short
    tickend <- rep(shortend, length(ticks))

    # Get the position within each cycle, 0, 1, 2, ..., 8, 0, 1, 2. ...
    cycleIdx <- ticknums - 1

    # Set the 'major' ticks long
    tickend[cycleIdx == 0] <- longend

    # Where to place the longer tick marks that are between each base For base 10, this
    # will be at each 5
    longtick_after_base <- floor(ticks_per_base/2)
    tickend[cycleIdx == longtick_after_base] <- midend

    if (delog) {

        ticksCopy = ticks

        regScale = log(ticks, base)

        majorTicks = sort(unique(c(minpow,
                                   regScale[which(regScale %in% majorTicks)],
                                   maxpow,
                                   majorTicks)
                                 )
                          )

        expandScale <- c()

        if (length(majorTicks) > 1) {

            for (i in 1:(length(majorTicks) - 1)) {
                expandScale = c(expandScale,
                                seq(majorTicks[i], majorTicks[i + 1], length.out = (ticks_per_base + 1))
                                )
            }

            ticks = unique(expandScale)

            # Set all of the ticks short
            tickend <- rep(shortend, length(ticks))

            # Set the 'major' ticks long
            tickend[which(ticks %in% majorTicks)] <- longend
        }
    }

    tickdf <- data.frame(value = ticks, start = start, end = tickend)

    tickdf
}
