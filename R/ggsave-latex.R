#'Saves a ggplot graphic to a file and creates the code to include it in a
#'LaTeX document.
#'
#'Saves a ggplot graphic to a file and creates the code to include it in a
#'LaTeX document. The code is inspired by xtable() from the xtable-package.
#'
#'
#'@param ...  arguments passed to the (\code{\link{ggsave}}) function
#'@param caption The caption. Default to NULL, indicating no caption.
#'@param label The label. Default to NULL, indicating no label.
#'@param figure.placement The placement of the figure. Default to "hbt".
#'@param floating Logical. Indicates if the figure should be placed in a
#'floating environment. Default to TRUE
#'@param caption.placement Should the caption be on top or bottom of the
#'figure. Default to "bottom"
#'@param latex.environments Alignment of the figure. Default to "center".
#'@return The graphic will be saved to a plot and the relevant LaTeX code is
#'printed.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{ggsave}}
#'@keywords hplot graphs
#'@examples
#'
#'  require(ggplot2)
#'  data(cars)
#'	p <- ggplot(cars, aes(x = speed, y = dist)) + geom_point()
#'	ggsave.latex(p, filename = "test.pdf", label = "fig:Cars", 
#'    caption = "A demo plot", height = 5, width = 4)
#'
#'@export
ggsave.latex <- function(..., caption = NULL, label = NULL, figure.placement = "hbt", floating = TRUE, caption.placement="bottom", latex.environments="center"){
	ggsave(...)
	
	cat("\n\n")
	if(floating){
		cat("\\begin{figure}[", figure.placement, "]\n", sep = "")
	}
	cat("    \\begin{", latex.environments,"}\n", sep = "")
	if(!is.null(caption) && caption.placement == "top"){
		cat("        \\caption{", caption, "}\n", sep = "")
	}
	args <- list(...)
	if(is.null(args[["filename"]])){
		if(is.null(args[["plot"]])){
			names(args)[which(names(args) == "")[1]] <- "plot"
		}
		args[["filename"]] <- paste(args[["path"]], digest.ggplot(args[["plot"]]), ".pdf", sep="")
	} else {
		args[["filename"]] <- paste(args[["path"]], args[["filename"]], sep="")
	}
	
	if(is.null(args[["width"]])){
		if(is.null(args[["height"]])){
			cat("        \\includegraphics[height = 7in, width = 7in]{", args[["filename"]], "}\n", sep = "")
		} else {
			cat("        \\includegraphics[height = ", args[["height"]], "in, width = 7in]{", args[["filename"]], "}\n", sep = "")
		}
	} else {
		if(is.null(args[["height"]])){
			cat("        \\includegraphics[height = 7in, width = ", args[["width"]], "in]{", args[["filename"]], "}\n", sep = "")
		} else {
			cat("        \\includegraphics[height = ", args[["height"]], "in, width = ", args[["width"]], "in]{", args[["filename"]], "}\n", sep = "")
		}
	}
	if(!is.null(caption) && caption.placement == "bottom"){
		cat("        \\caption{", caption, "}\n", sep = "")
	}
	if(!is.null(label)){
		cat("        \\label{", label, "}\n", sep = "")
	}
	cat("    \\end{", latex.environments,"}\n", sep = "")
	if(floating){
		cat("\\end{figure}\n")
	}
	cat("\n\n")
}
