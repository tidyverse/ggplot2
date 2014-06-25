#' Adjust position by simultaneously dodging and jittering
#'
#' This is primarily used for aligning points generated through
#' \code{geom_point()} with dodged boxplots (e.g., a \code{geom_boxplot()} with
#' a fill aesthetic supplied).
#'
#' @family position adjustments
#' @param jitter.width degree of jitter in x direction. Defaults to 40\% of the
#'   resolution of the data.
#' @param jitter.height degree of jitter in y direction. Defaults to 0.
#' @param dodge.width the amount to dodge in the x direction. Defaults to 0.75,
#'   the default \code{position_dodge()} width.
#' @export
#' @examples
#' dsub <- diamonds[ sample(nrow(diamonds), 1000), ]
#' ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) +
#'   geom_boxplot(outlier.size = 0) +
#'   geom_point(pch = 21, position = position_jitterdodge())
position_jitterdodge <- function (jitter.width = NULL,
                                  jitter.height = NULL,
                                  dodge.width = NULL) {

  PositionJitterdodge$new(jitter.width = jitter.width,
                          jitter.height = jitter.height,
                          dodge.width = dodge.width)
}

PositionJitterdodge <- proto(Position, {

  jitter.width <- NULL
  jitter.height <- NULL
  dodge.width <- NULL

  new <- function(.,
                  jitter.width = NULL,
                  jitter.height = NULL,
                  dodge.width = NULL) {

    .$proto(jitter.width=jitter.width,
            jitter.height=jitter.height,
            dodge.width=dodge.width)

  }

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
    nfill <- length(levels(data$fill))

    if (is.null(.$jitter.width)) {
      .$jitter.width <- resolution(data$x, zero = FALSE) * 0.4
    }

    if (is.null(.$jitter.height)) {
      .$jitter.height <- 0
    }

    trans_x <- NULL
    trans_y <- NULL
    if (.$jitter.width > 0) {
      trans_x <- function(x) jitter(x, amount = .$jitter.width / (nfill + 2))
    }
    if (.$jitter.height > 0) {
      trans_y <- function(x) jitter(x, amount = .$jitter.height)
    }

    if (is.null(.$dodge.width)) {
      .$dodge.width <- 0.75
    }

    ## dodge, then jitter
    data <- collide(data, .$dodge.width, .$my_name(), pos_dodge, check.width = FALSE)
    transform_position(data, trans_x, trans_y)
  }

})


position_jitterdodgeh <- function(jitter.width = NULL,
                          jitter.height = NULL,
                          dodge.height = NULL) {

  PositionJitterDodgeh$new(jitter.width = jitter.width,
                           jitter.height = jitter.height,
                           dodge.height = dodge.height)
}

PositionJitterDodgeh <- proto(Position, {

  jitter.width <- NULL
  jitter.height <- NULL
  dodge.height <- NULL

  new <- function(., jitter.width = NULL,
             jitter.height = NULL,
             dodge.height = NULL) {

    .$proto(jitter.width = jitter.width,
            jitter.height = jitter.height,
            dodge.height = dodge.height)

  }

  objname <- "jitterdodgeh"

  adjust <- function(., data) {

    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y", "fill"), names(data), "position_jitterdodgeh")

    ## Workaround to avoid this warning:
    ## xmax not defined: adjusting position using x instead
    if (!("xmax" %in% names(data))) {
      data$xmax <- data$x
    }

    ## Adjust the y transformation based on the number of 'fill' variables
    nfill <- length(levels(data$fill))

    if (is.null(.$jitter.height)) {
      .$jitter.height <- resolution(data$y, zero = FALSE) * 0.4
    }

    if (is.null(.$jitter.width)) {
      .$jitter.width <- 0
    }

    trans_x <- NULL
    trans_y <- NULL
    if (.$jitter.height > 0) {
      trans_y <- function(y) jitter(y, amount = .$jitter.height / (nfill + 2))
    }
    if (.$jitter.width > 0) {
      trans_x <- function(y) jitter(y, amount = .$jitter.width)
    }

    if (is.null(.$dodge.height)) {
      .$dodge.height <- 0.75
    }

    ## dodge, then jitter
    data <- collideh(data, .$dodge.height, .$my_name(), pos_dodgev, check.height = FALSE)
    transform_position(data, trans_x, trans_y)
  }

})
