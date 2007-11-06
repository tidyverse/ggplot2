GeomInterval <- proto(Geom, {
  
  required_aes <- c("x", "min", "max")
  adjust_scales_data <- function(., scales, data) {
    if (is.null(data) || nrow(data) == 0) return()
    if (is.null(data$width)) data$width <- 0
    if (is.null(data$min)) data$min <- 0
    if (is.null(data$max)) data$max <- data$y
    
    scale_x <- scales$get_scales("x")
    if (scale_x$discrete()) {
      scale_x$.expand[2] <- max(scale_x$.expand[2], max(data$width / 2) + 0.1)
      parts <- compact(list(
        tryNULL(transform(data, y=min, x = x)),
        tryNULL(transform(data, y=max, x = x))
      ))
    } else {
      parts <- compact(list(
        tryNULL(transform(data, y=min, x = x + width/2)),
        tryNULL(transform(data, y=max, x = x - width/2))
      ))
    }
    if (length(parts) == 0) return(data)
    do.call("rbind", parts)
  }
  
  objname <- "interval"
  desc <- "Base for all interval (range) geoms"
  
})
