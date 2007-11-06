StatDensity2d <- proto(Stat, {
  objname <- "density_2d" 
  desc <- "Density estimation, 2D"
  
  default_geom <- function(.) GeomDensity2d
  default_aes <- function(.) aes(group = ..piece..)

  desc_outputs <- list(
    level = "Computed density"
  )
  icon <- function(.) GeomDensity2d$icon()

  calculate <- function(., data, scales, ...) {
    df <- data.frame(data[, c("x", "y")])
    df <- df[complete.cases(df), ]
    dens <- do.call(MASS::kde2d, c(df, n=100, ...))
    df <- with(dens, data.frame(expand.grid(x = x, y = y), z = as.vector(z)))
    
    z_scale <- scale_z_continuous()
    z_scale$train(df$z)
    scales$add(z_scale)
    
    StatContour$calculate(df, scales, ...)
  }
  
  examples <- function(.) {
    m <- ggplot(movies, aes(x=rating, y=length)) + geom_point() + scale_y_continuous(limits=c(1, 500))
    m + geom_density_2d()

    dens <- MASS::kde2d(movies$rating, movies$length, n=100)
    densdf <- data.frame(expand.grid(rating = dens$x, length = dens$y), z=as.vector(dens$z))
    m + geom_contour(aes(z=z), data=densdf)

    m + geom_density_2d() + scale_y_log10()
    m + geom_density_2d() + coord_trans(y="log10")
    
    m + stat_density_2d(aes(fill = ..level..), geom="polygon")

    qplot(rating, length, data=movies, geom=c("point","density2d"), ylim=c(1, 500))
  }  
  
})
