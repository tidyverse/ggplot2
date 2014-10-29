#' Fortify method for classes from the raster package.
#'
#' @param x \code{Raster*} object to convert into a dataframe.
#' @param maxpixels maximal number of pixels to display
#' @param data not used by this method
#' @param ... not used by this method
#' @return Returns a data.frame with coordinates (x,y) and corresponding raster values.
#' @name fortify.raster
#' @examples
#' if (require("raster")) {
#' br <- brick(system.file("external/rlogo.grd", package="raster"))
#' br_df <- fortify(br)
#' }
#' 
NULL

#' @rdname fortify.raster
#' @export
#' @method fortify RasterLayer
fortify.RasterLayer <- function(x, maxpixels = 500000){
    data.frame(sampleRegular(x, maxpixels, xy = TRUE))     
}


#' @rdname fortify.raster
#' @export
#' @method fortify RasterBrick
fortify.RasterBrick <- function(...){
    fortify.RasterLayer(...)    
}

#' @rdname fortify.raster
#' @export
#' @method fortify RasterStack
fortify.RasterStack <- function(...){
    fortify.RasterLayer(...)    
}

