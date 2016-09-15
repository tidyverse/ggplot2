#' ggplot2: An Implementation of the Grammar of Graphics
#'
#' ggplot2 is a system for declaratively creating graphics. You provide the
#' data and tell ggplot2 how to map variables to aesthetics. ggplot2 is a
#' complete plotting system and is quite different to other graphics systems in
#' R like base graphics and lattice.  If you've never used ggplot2 before, I
#' recomend starting with a comprehensive introduction such as
#' \url{http://r4ds.had.co.nz/data-visualisation.html}. For more
#' documentation and examples, see see \url{http://ggplot2.org}.
#'
#' @examples
#' ggplot(mpg, aes(displ, cty)) +
#'   geom_point()
"_PACKAGE"

#' @import scales grid gtable
#' @importFrom plyr defaults
#' @importFrom stats setNames
NULL
