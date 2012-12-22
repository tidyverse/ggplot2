theme_foundation_null <- function(base_size=12, base_family="") {
        theme(
            line = element_line(colour = "black", size = 0.5,
            linetype = 1, lineend = "butt"),
            rect = element_rect(fill = "white", colour = "black",
            size = 0.5, linetype = 1),
            text =  element_text(family = base_family, face = "plain",
            colour = "black", size = base_size,
            hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
            axis.text = element_text(),
            strip.text = element_text(),
            axis.line = element_blank(),
            axis.title= element_text(),
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            axis.text = element_text(),
            axis.text.x = element_text(),
            axis.text.y = element_text(),
            axis.ticks = element_line(),
            axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.ticks.length =  unit(0.15, "cm"),
            axis.ticks.margin =  unit(0.1, "cm"),
            axis.line = element_line(),
            axis.line.x = element_line(),
            axis.line.y = element_line(),
            legend.background = element_rect(colour = NA),
            legend.margin = unit(0.2, "cm"),
            legend.key = element_rect(),
            legend.key.size = unit(1.2, "lines"),
            legend.key.height = NULL,
            legend.key.width = NULL,
            legend.text = element_text(),
            legend.text.align = NULL,
            legend.title = element_text(),
            legend.title.align = NULL,
            legend.position = "right",
            legend.direction = NULL,
            legend.justification = "center",
            legend.box = NULL,
            ## Must have colour=NA or covers the plot
            panel.background = element_rect(),
            panel.border = element_rect(fill=NA),
            panel.margin = unit(0.25, "lines"),
            panel.grid = element_line(),
            panel.grid.major = element_line(),
            panel.grid.major.x = element_line(),
            panel.grid.major.y = element_line(),
            panel.grid.minor = element_line(),
            panel.grid.minor.x = element_line(),
            panel.grid.minor.y = element_line(),
            plot.background = element_rect(),
            plot.title = element_text(),
            plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
            strip.background = element_rect(),
            strip.text = element_text(),
            strip.text.x = element_text(),
            strip.text.y = element_text(angle = -90),
            complete = TRUE)
}

theme_foundation_sizes <- function() {
    theme(
        axis.text.y = element_text(hjust = 1),
        strip.text = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        axis.text.x = element_text(vjust = 1),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8), hjust = 0),
        panel.grid.minor = element_line(size = 0.25),
        plot.title = element_text(size = rel(1.2)),
        strip.text.y = element_text(angle = -90)
        )
}


##' Foundational Theme
##'
##' This theme is designed to be a foundation from which to build new
##' themes, and not meant to be used directly. \code{theme_foundation}
##' is a complete theme with only minimal number of elements defined.
##'
##' It is easier to create new themes by extending this one rather
##' than \code{theme_gray} or \code{theme_bw}, because those themes
##' those themes define elements deep in the hierarchy.
##'
##' @param base_size Base font size.
##' @param base_family Base font family.
##' @param use_sizes If \code{TRUE}, then define sizes and locations
##' with reasonable defaults taken from \code{\link{theme_gray}}.
##'
##' @family themes
##' @export
##' @examples
##' # Simple theme with blue text and gray background
##' new_theme <- (theme_foundation()
##'  + theme(text = element_text(colour = "blue"),
##'          plot.background = element_rect("gray")))
##' DF <- data.frame(x = rnorm(400))
##' m <- ggplot(DF, aes(x = x)) + geom_histogram()
##' m + new_theme
##'
theme_foundation <- function(base_size=12, base_family="", use_sizes=TRUE) {
    thm <- theme_foundation_null(base_size=base_size, base_family=base_family)
    if (use_sizes) {
        thm + theme_foundation_sizes()
    }
    thm
}

