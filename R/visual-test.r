# Set the context of the visual tests
# This creates vis_context and vis_info in the Global environment
vcontext <- function(context) {
  assign("vis_context", context, envir = .GlobalEnv)
  assign("vis_info", list(), envir = .GlobalEnv)
  message("\n", context, appendLF = FALSE)
}


# Save a test to file, and record information to vis_info
save_vtest <- function(name = "", desc = NULL, subdir = NULL, width = 4, height = 4, dpi = 72, device = "Cairo_png") {

  # Cairo_png from cairoDevice package seems to be only good cross-platform output device
  if (device == "Cairo_png") require(cairoDevice)
  if (is.character(device)) device <- match.fun(device)

  if (is.null(subdir))
    subdir <-  get("vis_context", envir = .GlobalEnv)

  destdir <- file.path("visual_test", subdir)
  dir.create(destdir, showWarnings = FALSE)

  filename <- paste(name, ".png", sep="")
  ggsave(file.path(destdir, filename), width = width, height = height, dpi = dpi, device = device)

  testinfo <- list(filename = filename, name = name, desc = desc)
  # Append the info for this test in the vis_info list
  assign("vis_info", c(get("vis_info", envir = .GlobalEnv), list(testinfo)), envir = .GlobalEnv)

  message(".", appendLF = FALSE)
}


# Make the web page for the current test context
# Reads from vis_context and vis_info
make_vtest_webpage <- function() {
  context <- get("vis_context", envir = .GlobalEnv)

  outfile <- file.path("visual_test", context, "index.html")

  # Write HTML code to show a single test
  item_html <- function(t) {
    paste("<TR><TD ALIGN='center'>", t$name, "</TD></TR>\n",
          "<TR><TD ALIGN='center'>", t$desc, "<BR>\n",
          "  <IMG SRC='", t$filename , "'></TD></TR>\n",
          "<TR><TD BGCOLOR='#000000'> &nbsp; </TD></TR>\n", sep="")
  }

  write(paste("<HTML><HEAD><TITLE>ggplot2 tests: ", context,
              "</TITLE></HEAD><BODY><H1>ggplot2 tests: ", context,
              "</H1>\n", sep = ""), outfile)

  write("<TABLE border='1'>\n", outfile, append = TRUE)
  # Get the list of info about all tests
  info <- get("vis_info", envir = .GlobalEnv)
  # Write information about all the items in vis_info
  write(sapply(info, item_html), outfile, sep = "\n", append = TRUE)

  write("</TABLE></BODY></HTML>", outfile, append = TRUE)

}
