#' Jitter-dodge points to align them with a boxplot including fill aesthetic
#'
#' @family position adjustments
#' @param width degree of jitter in x direction. Defaults to 40\% of the
#'   resolution of the data.
#' @param height degree of jitter in y direction. Defaults to 40\% of the
#'   resolution of the data
#' @export
#' @examples
#' dsub <- diamonds[ sample(1:nrow(diamonds), 1000), ]
#' ggplot(dsub, aes(x=cut, y=carat, fill=clarity)) +
#'   geom_boxplot(outlier.size=0) +
#'   geom_point( pch=21, position=position_jitterdodge() )
position_jitterdodge <- function (width = NULL, height = NULL) {
  PositionJitterDodge$new(width = width, height = height)
}

#' @rdname position_jitterdodge
#' @export
position_jd <- position_jitterdodge

PositionJitterDodge <- proto(Position, {
  objname <- "jitterdodge"

  adjust <- function(., data) {

    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y", "fill"), names(data), "position_jitterdodge")

    ## Workaround to avoid this warning:
    ## ymax not defined: adjusting position using y instead
    if (!("ymax" %in% names(data))) {
      data$ymax <- data$y
    }

    ## Adjust the x transformation based on the number of 'fill' variables
    nfill <- length( levels(data$fill) )

    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
    if (is.null(.$height)) .$height <- resolution(data$y, zero = FALSE) * 0.4

    trans_x <- NULL
    trans_y <- NULL
    if(.$width > 0) {
      trans_x <- function(x) jitter(x, amount = .$width / (nfill + 2))
    }
    if(.$height > 0) {
      trans_y <- function(x) jitter(x, amount = .$height)
    }

    ## dodge, then jitter
    data <- collide(data, 0.75, .$my_name(), pos_dodge, check.width = FALSE)
    transform_position(data, trans_x, trans_y)
  }

})
