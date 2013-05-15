#' Supply a position offset
#'
#' @family position adjustments
#' @param x Defaults to 40\% of the resolution of the data.
#' @param y Defaults to 40\% of the resolution of the data.
#' @export
#' @examples
#' # Place labels relative to the middle of categorical axes
#' 
#' confusion <- structure(list(truth = c(FALSE, TRUE, FALSE, TRUE),
#'                             detected = c(FALSE,  FALSE, TRUE, TRUE),
#'                             value = c(51L, 3L, 3L, 43L)),
#'                        .Names = c("truth",  "detected", "value"),
#'                        row.names = c(NA, -4L), class = "data.frame")
#' ggplot(confusion, aes(truth, detected)) + geom_point(aes(size=value)) +
#'   geom_text(label="middle", aes(x=0, y=0), position = position_offset(1.5,1.5))
#' 
#' # Here's an illustration of how modern test theory turns a categorical
#' # variable into a conditionally continuous variable.
#' 
#' width <- 5
#' skill <- sort(runif(500,-width,width))
#' item.p <- .4
#' empirical.bins <- 20
#' correct <- rep(TRUE, length(skill))
#' skill.o <- skill + rnorm(length(skill), sd=1)
#' correct[order(skill.o)[seq(1,(1-item.p) * length(skill))]] <- FALSE
#' grid <- list()
#' grid$correct <- correct
#' grid$skill <- skill
#' breaks <- seq(min(skill)-.001, max(skill)+.001, length.out=empirical.bins)
#' bin <- cut(skill, breaks, labels=FALSE)
#' bin.correct <- data.frame(at=breaks[-1] - diff(breaks)/2,
#'  pct=vapply(1:max(bin), function(l) sum(correct[bin==l])/sum(bin==l), 0))
#' bin.correct$pct <- sprintf("%.0f", 100 * bin.correct$pct)
#' ggplot(as.data.frame(grid), aes(skill, correct)) +
#'   geom_point(position=position_jitter(0,.05), size=1) +
#'   geom_segment(data=data.frame(thresh=breaks),
#'                aes(x=thresh, xend=thresh, y=TRUE, yend=FALSE), color="red") +
#'   geom_text(data=bin.correct, aes(x=at,y=TRUE,label=pct), color="blue",
#'             angle=90, position = position_offset(0,-.5)) +
#'   labs(y="% correct")

position_offset <- function (x = NULL, y = NULL) {
  PositionOffset$new(width = x, height = y)
}

PositionOffset <- proto(Position, {
  objname <- "offset"
 
  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_offset")
    
    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
    if (is.null(.$height)) .$height <- resolution(data$y, zero = FALSE) * 0.4
    
    trans_x <- function(x) x + .$width
    trans_y <- function(x) x + .$height
    
    transform_position(data, trans_x, trans_y)
  }
})
