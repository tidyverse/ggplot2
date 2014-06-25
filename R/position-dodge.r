#' Adjust position by dodging overlaps to the side.
#'
#' @inheritParams position_identity
#' @family position adjustments
#' @export
#' @examples
#' \donttest{
#' ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) +
#'   geom_bar(position="dodge")
#' ggplot(diamonds, aes(x=price, fill=cut)) + geom_bar(position="dodge")
#' # see ?geom_boxplot and ?geom_bar for more examples
#'
#' # Dodging things with different widths is tricky
#' df <- data.frame(x=c("a","a","b","b"), y=1:4, g = rep(1:2, 2))
#' (p <- ggplot(df, aes(x, y)) +
#'       geom_bar(stat = "identity", position = "dodge", aes(group = g)))
#'
#' p + geom_linerange(aes(ymin = y-1, ymax = y+1), position="dodge")
#' # You need to explicitly specify the width for dodging
#' p + geom_linerange(aes(ymin = y-1, ymax = y+1),
#'   position = position_dodge(width = 0.9))
#'
#' # Similarly with error bars:
#' p + geom_errorbar(aes(ymin = y-1, ymax = y+1), width = 0.2,
#'   position="dodge")
#' p + geom_errorbar(aes(ymin = y-1, ymax = y+1, width = 0.2),
#'   position = position_dodge(width = 0.90))
#' }
position_dodge <- function (width = NULL, height = NULL) {
  PositionDodge$new(width = width, height = height)
}

PositionDodge <- proto(Position, {
  objname <- "dodge"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics("x", names(data), "position_dodge")

    collide(data, .$width, .$my_name(), pos_dodge, check.width = FALSE)
  }

})


position_dodgeh <- function (width = NULL, height = NULL) {
  PositionDodgeh$new(width = width, height = height)
}

PositionDodgeh <- proto(Position, {
  objname <- "dodgeh"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics("y", names(data), "position_dodgeh")

    collideh(data, .$height, .$my_name(), pos_dodgeh, check.height = FALSE)
  }

})

