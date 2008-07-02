background <- function(grob, fill = NA, colour = NA, padding = unit(0, "lines"), margin = unit(0, "lines"), size = 1, linetype = 1) {
  padding <- rep(list(padding), length = 4)
  margin <- rep(list(margin), length = 4)
  names(padding) <- names(margin) <- c("top", "right", "bottom", "left")
  
  width.in   <- sum(padding$left, grobWidth(grob), padding$right)
  width.out  <- sum(margin$left, width.in, margin$right)
  height.in  <- sum(padding$top, grobHeight(grob), padding$bottom)
  height.out <- sum(margin$top, height.in, margin$bottom)
  
  vp <- viewport(width = width.out, height = height.out)
  bg <- rectGrob(
    x = grobX(grob, "west") - padding$left, hjust = 0,
    y = grobY(grob, "south") - padding$top, vjust = 0,
    width = width.in, height = height.in, 
    gp = gpar(col = NA, fill = fill)
  )
  border <- rectGrob(
    x = grobX(grob, "west") - padding$left, hjust = 0,
    y = grobY(grob, "south") - padding$top, vjust = 0,
    width = width.in, height = height.in, 
    gp = gpar(col = colour, fill = NA, lty = linetype, lwd = size)
  )
  margin <- rectGrob(
    x = grobX(grob, "west") - padding$left - margin$left, hjust = 0,
    y = grobY(grob, "south") - padding$top - margin$top, vjust = 0,
    width = width.out, height = height.out, 
    gp = gpar(fill = NA, col = "grey90", size = 0.5, lty = 3)
  )
  
  grobTree(margin, bg, grob, border)
}

# bg.test <- function(grob) {
#   background(grob, fill=sample(colors(), 1), colour="grey50", padding=unit(1, "lines"), margin=unit(1, "lines"), size=2)
# }
# 
# r <- rectGrob(height=unit(3, "cm"), width=unit(2, "cm"), x=0.8, gp=gpar(fill="red"))
# grid.newpage(); grid.draw(bg.test(r))
# grid.newpage(); grid.draw(bg.test(bg.test(r)))