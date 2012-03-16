# Set the context of the visual tests
# This creates vis_context and vis_info in the Global environment
vcontext <- function(context) {
  .GlobalEnv$vis_context <- context
  .GlobalEnv$vis_info <- list()
  message("\n", context, appendLF = FALSE)
}

# Save a test to file, and record information to vis_info
# This presently only works with pdf; other file types will fail
save_vtest <- function(name = "", desc = NULL, subdir = NULL, width = 4, height = 4,
                       dpi = 72, device = "pdf") {

  subdir <-  .GlobalEnv$vis_context

  destdir <- file.path("visual_test", subdir)
  dir.create(destdir, showWarnings = FALSE)    # Create dir if missing

  # Save it to vistest.pdf in the temp dir
  ggsave(file.path(tempdir(), "vistest.pdf"), width = width, height = height,
         dpi = dpi, device = match.fun(device), compress = FALSE)

  # TODO: Be more careful about these lines
  # Load vistest.pdf and modify the CreationDate and ModDate (lines 5 and 6)
  temppdf <- file(file.path(tempdir(), "vistest.pdf"), "r")
  pdftext <- readLines(temppdf)
  close(temppdf)
  pdftext[5] <- "/CreationDate (D:00000000000000)"
  pdftext[6] <- "/ModDate (D:00000000000000)"

  # Write the modified PDF file to the final destination file
  filename <- paste(name, device , sep=".")
  outpdf <- file(file.path(destdir, filename), "w")
  writeLines(pdftext, outpdf)
  close(outpdf)

  testinfo <- list(filename = filename, name = name, desc = desc)
  # Append the info for this test in the vis_info list
  .GlobalEnv$vis_info <- c(.GlobalEnv$vis_info, list(testinfo))

  message(".", appendLF = FALSE)
}


finish_vcontext <- function() {
  make_vtest_webpage()
  dput(.GlobalEnv$vis_info, file.path("visual_test", .GlobalEnv$vis_context, "testinfo.dat"))
  message("")  # Print a newline
}


# Make the web page for the current test context
# Reads from vis_context and vis_info
make_vtest_webpage <- function() {
  context <- .GlobalEnv$vis_context

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

  write("<TABLE BORDER='1'>\n", outfile, append = TRUE)
  # Get the list of info about all tests
  # Write information about all the items in vis_info
  write(sapply(.GlobalEnv$vis_info, item_html), outfile, sep = "\n", append = TRUE)

  write("</TABLE></BODY></HTML>", outfile, append = TRUE)

}



# Make visual diff from two refs
vdiff <- function(ref1 = "HEAD", ref2 = "") {

  # A function for checking out visual_test from a commit ref, or "" for current state
  checkout_vtests <- function(ref = "", dir = "temp", checkoutdir = "visual_test") {

    unlink(dir, recursive = TRUE)          # Delete existing directory
    dir.create(dir, showWarnings = FALSE)  # Create the new directory

    if (ref == "") {
      # If blank, simply copy the existing files over
      files <- file.path(checkoutdir, list.files(checkoutdir, recursive=TRUE))

      # Find which directories need to be created, and then create them
      newdirs <- unique(file.path(dir, dirname(files)))
      sapply(newdirs, dir.create, recursive = TRUE, showWarnings = FALSE)
      # Copy the files over
      file.copy(files, file.path(dir, files))

    } else {
      # Checkout the git ref into dir
      system2("git", c("--work-tree", dir, "checkout", ref, "--", checkoutdir))
    }
  }

  # Check we're in top level of the repo
  if (getwd() != system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE))
    stop("This must be run from the top level of the git tree.")

  # Checkout copies of the visual tests into diff/1 and diff/2
  checkout_vtests(ref1, dir = file.path("diff", "1"), checkoutdir = "visual_test")
  checkout_vtests(ref2, dir = file.path("diff", "2"), checkoutdir = "visual_test")

  # Find different files using git diff
  dfiles <- system2("git", c("diff", "--name-only", "HEAD"), stdout = TRUE)

  dfiles <- dfiles[grepl("\\.pdf$", dfiles)]
  message("Changed test images:\n", paste(dfiles, collapse="\n"))
  dfiles1 <- file.path("diff", "1", dfiles)
  dfiles2 <- file.path("diff", "2", dfiles)

  unlink(file.path("diff", "diff"), recursive = TRUE)                # Remove old diff files
  dfilesout <- file.path("diff", "diff", dfiles)   # Where the diff files go
  # Find which directories need to be created, and then create them
  newdirs <- unique(dirname(dfilesout))
  sapply(newdirs, dir.create, recursive = TRUE, showWarnings = FALSE)

  # Convert the changed files to png and run image diffs on each
  for (i in seq_along(dfiles)) {
    message("Generating diff image for ", dfiles2[i])
    f1    <- dfiles1[i]
    f2    <- dfiles2[i]
    f1png <- sub(".pdf$", ".png", dfiles1[i])
    f2png <- sub(".pdf$", ".png", dfiles2[i])

    system2("convert", c("-density", "72x72", f1, f1png))
    system2("convert", c("-density", "72x72", f2, f2png))
    system2("compare", c(f1png, f2png, sub(".pdf$", ".png", dfilesout[i])))
  }

  # Find the subdirs that have testinfo.dat, and generate diff webpages for them
  testdirs <- dirname(list.files(file.path("diff", "2", "visual_test"),
                                 pattern = "testinfo.dat", recursive = TRUE))

  # Make diff pages for each of these directories
  sapply(testdirs, make_diffpage)

  invisible()
}


make_diffpage <- function(dir) {
  outfile <- file.path("diff", "diff", "visual_test", dir, "index.html")
  message("Writing ", outfile)

  dir1 <- file.path("diff", "1", "visual_test", dir)    # Files from ref1
  dir2 <- file.path("diff", "2", "visual_test", dir)    # Files from ref2
  dird <- file.path("diff", "diff", "visual_test", dir) # Files for diff

  dir.create(dird, recursive = TRUE, showWarnings = FALSE) # Create diff dir if needed

  # Get the information about the tests
  testinfo <- dget(file.path(dir2, "testinfo.dat"))

  # Write HTML code to show a single test
  item_html <- function(t) {
    # TODO: change to use div
    #<div class="float">name <img src="…"/ > </div>
    #.float {float: left; width: 200px}
    
    # The diff image (if it exists) is a png, not a pdf
    dfilename <- sub("\\.pdf$", ".png", t$filename)
    
    paste("<table border=1>\n",
          "<tr><td align='center' colspan='3'>", t$name, "</td></tr>\n",
          "<tr><td align='center' colspan='3'>", t$desc, "</td></tr>\n",
          "<tr>\n",
          "  <td><img src='", paste("../../../..", dir1, t$filename, sep="/"), "'></td>\n",
          "  <td><img src='", paste("../../../..", dir2, t$filename, sep="/") , "'></td>\n",
          "  <td><img src='", dfilename, "'></td>\n",
          "</tr>\n",
          "</table>\n", sep="")
  }

  write(paste("<html><head><title>ggplot2 tests: ", dir,
              "</title></head><body><h1>ggplot2 tests: ", dir,
              "</h1>\n", sep = ""), outfile)

  write("<table border='1'>\n", outfile, append = TRUE)

  # Write information about all the test items in testinfo
  write(sapply(testinfo, item_html), outfile, sep = "\n", append = TRUE)

  write("</table></body></html>", outfile, append = TRUE)

}
