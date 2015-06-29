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

  PositionJitterDodge$new(jitter.width = jitter.width,
                          jitter.height = jitter.height,
                          dodge.width = dodge.width)
}

PositionJitterDodge <- proto2(  
  inherit = Position,
  members = list(
    jitter.width = NULL,
    jitter.height = NULL,
    dodge.width = NULL,

    new = function(jitter.width = NULL,
      jitter.height = NULL, dodge.width = NULL)
    {
      proto(
        inherit = self,
        members = list(
          jitter.width = jitter.width,
          jitter.height = jitter.height,
          dodge.width = dodge.width
        )
      )
    },

    objname = "jitterdodge",

    adjust = function(data) {

      if (empty(data)) return(data.frame())
      check_required_aesthetics(c("x", "y", "fill"), names(data), "position_jitterdodge")

      ## Workaround to avoid this warning:
      ## ymax not defined: adjusting position using y instead
      if (!("ymax" %in% names(data))) {
        data$ymax <- data$y
      }

      ## Adjust the x transformation based on the number of 'fill' variables
      nfill <- length(levels(data$fill))

      if (is.null(self$jitter.width)) {
        self$jitter.width <- resolution(data$x, zero = FALSE) * 0.4
      }

      if (is.null(self$jitter.height)) {
        self$jitter.height <- 0
      }

      trans_x <- NULL
      trans_y <- NULL
      if (self$jitter.width > 0) {
        trans_x <- function(x) jitter(x, amount = self$jitter.width / (nfill + 2))
      }
      if (self$jitter.height > 0) {
        trans_y <- function(x) jitter(x, amount = self$jitter.height)
      }

      if (is.null(self$dodge.width)) {
        self$dodge.width <- 0.75
      }

      ## dodge, then jitter
      data <- collide(data, self$dodge.width, self$my_name(), pos_dodge, check.width = FALSE)
      transform_position(data, trans_x, trans_y)
    }
  )
)
