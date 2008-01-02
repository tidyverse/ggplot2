# Use diamonds data instead
# # this is a horrible example dataset, but illustrates the point
# p <- qplot(wt, mpg, data=mtcars, weight=cyl)
# # Alpha blending
# scalpha(p)
# scalpha(p, maxalpha=0.5) # set maximum alpha level (for highest weights)
# # Conservation of ink
# sconserve(p)
# sconserve(p, size.co(2, 10), alpha.to=c(1,0.5)) # set size and alpha ranges
# sconserve(p, colour="red") # set colour
# sconserve(p, max=1) # set maximum weight instead of retrieving for data
# 
# # Density estimation
# qplot(wt, mpg, data=mtcars, weight=cyl, geom="wdensity")
# qplot(wt, mpg, data=mtcars, weight=cyl, geom="wdensity", grid=FALSE) # use points instead
# qplot(wt, mpg, data=mtcars, weight=cyl, geom="wdensity", low="yellow", high="blue")
# qplot(wt, mpg, data=mtcars, weight=cyl, geom="wdensity", radius=1/20) # radius in proportion of data range, defaults to 1%
# qplot(wt, mpg, data=mtcars, weight=cyl, geom="wdensity", trans=function(x) sqrt(x))
# 
# scalpha <- function(plot = .last_plot, name=NULL, colour="black", maxalpha=0.5) {
#   add_scale(plot, scale_gradient(name=name, low=alpha(colour, 0), mid=alpha(colour, 0), high=alpha(colour, maxalpha), variable="weight", range=c(0,NA)))
# }
# 
# scconserve <- function(plot = .last_plot, name=NULL, max=NA, colour="black", size.to = c(0.2, 5), alpha.to = c(1, 0)) {
#   add_scale(plot, scale_conserve(name=name, max=max, colour=colour, size.to=size.to, alpha.to=alpha.to))
# }
# 
# scale_conserve <- function(name=NULL, max=NA, colour="black", size.to = c(3, 0.1), alpha.to = c(0.1, 1), ...) {
#   structure(
#     list(name=name, range=c(0, max), colour=colour, size.to=size.to, alpha.to=alpha.to, transform=trans_none), 
#     class = c("conserve", "continuous", "scale")
#   )
# }
# 
# input.conserve <- function(scale) "weight"
# output.conserve <- function(scale) c("size", "colour")
# 
# map_aesthetic.conserve <- function(scale, data, ...) {
# if (is.null(data) || nrow(data) == 0) return()
#   
#   data.frame(
#     size = rescale(data$weight, sqrt(scale$size.to), scale$range),
#     colour = alpha(scale$colour, rescale(data$weight, scale$alpha.to, scale$range))
#   )
# }
# 
# breaks.conserve <- function(scale) {
#   r <- ggpretty(scale$range)
#   data.frame(
#     colour = alpha(scale$colour, rescale(r, scale$alpha.to, scale$range)),
#     size = rescale(r, sqrt(scale$size.to), scale$range)
#   )
# }
# 
# labels.conserve <- function(scale) as.character(ggpretty(scale$range))
# 
# #sm.density(mtcars[,c("wt", "mpg")], display = "none", ngrid=200, h=c(0.1,0.1), eval.points=as.matrix((mtcars[,c("wt", "mpg")])))
# 
# ggwdensity <- function(plot = .last_plot, aes=aes(), ..., data=NULL) {
#   gg_add("wdensity", plot, aesthetics, ..., data=data)
# }
# 
# geom_wdensity <- function(aesthetics, radius=1/100, grid=TRUE, low="white", high="black", trans=force, ngrid=100, threshold=0, ...) {
#   calccol <- function(x) {
#     map_colour_gradient(x, low=low, high=high, from=range(c(0, x), na.rm=TRUE))
#   }
#   df <- data.frame(aesthetics[, c("x", "y")])
#   w <- nulldefault(aesthetics$weights, 1:nrow(df))
#   
#   h <- apply(df, 2, function(x) diff(range(x), na.rm=TRUE)) * radius
#   
#   df <- df[complete.cases(df), ]
#   if (grid) {
#     dens <- sm.density(df, h, weights=w, display="none", ngrid=ngrid)
#     df <- expand.grid(x = dens$eval.points[, 1], y = dens$eval.points[, 2])
#     col <- calccol(trans(dens$estimate))
# 
#     geom_tile(cbind(df, fill=col), colour=NA, ...)
#   } else {
#     dens <- sm.density(df, h, weights=w, eval.points=df, eval.grid=FALSE, display="none")
#     col <- calccol(trans(dens$estimate))
#     
#     geom_point(cbind(df, colour=col), ...)
#   }
# }
# 
# wq <- function(x, weights=rep(1, length(x)))
#   as.numeric(coef(rq(x ~ 1, weight = weights, tau=c(0.25, 0.5, 0.75))))
#   
# q <- function(x) as.numeric(quantile(x, c(0.25, 0.5, 0.75)))