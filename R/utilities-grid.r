ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

dev.save <- function(file, width=6, height= 4) {
  dev.copy(device=pdf, file=file, width=width, height=height)
  dev.off()
  cat("\\includegraphics[scale=1]{", file, "}", "\n", sep="")
}
