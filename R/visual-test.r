# Set the context of the visual tests
# This creates vis_context and vis_info in the Global environment


get_vcontext <- NULL
set_vcontext <- NULL
get_vtestinfo <- NULL
append_vtestinfo <- NULL

local({
  context <- NULL
  testinfo <- NULL

  get_vcontext <<- function() context
  set_vcontext <<- function(value) {
    context <<- value
    testinfo <<- list()
  }
  get_vtestinfo <<- function() testinfo
  append_vtestinfo <<- function(value) testinfo <<- c(testinfo, list(value))
})


# ISSUE: move this into the local block?
vcontext <- function(context) {
  if (!is.null(get_vcontext()))
    stop("Can't open new context while current context is still open. Use finish_vcontext().")
  set_vcontext(context)
  message(context, appendLF = FALSE)
}

# ISSUE: move this into the local block?
finish_vcontext <- function() {
  message("")         # Print a newline
  make_vtest_webpage()
  dput(get_vtestinfo(), file.path("visual_test", get_vcontext(), "testinfo.dat"))
  set_vcontext(NULL)  # Reset the context
}


# Save an individual test to file, and record information using apped_vtestinfo
# This presently only works with pdf; other file types will fail
# * desc: a short description of the test
# * filename: output filename (not including extension, like ".pdf"). If NULL, use MD5
#     hash of `desc` as the filename.
# * width: width in inches
# * height: height in inches
# * dpi: pixels per inch (OK, it really should be ppi)
# * device: string with name of output device. Only "pdf" is supported now.
save_vtest <- function(desc = NULL, filename = NULL, width = 4, height = 4,
                       dpi = 72, device = "pdf") {
  require(digest)
  if (is.null(get_vcontext())) stop("Must have active vcontext")
  if (is.null(desc))           stop("desc must not be NULL")
  if (device != "pdf")         stop('Only "pdf" device supported at this time')

  # Put files in visual_test/<subdir>, where subdir is the vcontext
  destdir <- file.path("visual_test", get_vcontext())
  dir.create(destdir, showWarnings = FALSE)    # Create dir if missing

  # Save it to vistest.pdf in the temp dir
  ggsave(file.path(tempdir(), "vistest.pdf"), width = width, height = height,
         dpi = dpi, device = match.fun(device), compress = FALSE)

  # TODO: Be more careful about editing these lines in the PDF
  # Load vistest.pdf and modify the CreationDate and ModDate (lines 5 and 6)
  temppdf <- file(file.path(tempdir(), "vistest.pdf"), "r")
  pdftext <- readLines(temppdf)
  close(temppdf)
  pdftext[5] <- "/CreationDate (D:00000000000000)"
  pdftext[6] <- "/ModDate (D:00000000000000)"

  # Write the modified PDF file to the final destination file
  hash <- digest(desc)
  filename <- paste(hash, device, sep=".")
  outpdf <- file(file.path(destdir, filename), "w")
  writeLines(pdftext, outpdf)
  close(outpdf)

  # Append the info for this test in the vis_info list
  append_vtestinfo(list(filename = filename, desc = desc, hash = hash, type = device, 
                        width = width, height = height, dpi = dpi))

  message(".", appendLF = FALSE)
}


# Make the web page for the current test context
# Reads from vis_context and vis_info
make_vtest_webpage <- function(subdir = NULL, convertpng = FALSE) {

  if (is.null(subdir)) {
    # By default, use the current vcontext (subdir is same as context)
    subdir <- get_vcontext()
    testinfo <- get_vtestinfo()
  } else {
    # If subdir is specified, read the testinfo from the file
    testinfo <- dget(file.path("visual_test", subdir, "testinfo.dat"))
  }

  # Write HTML code to show a single test
  item_html <- function(t, convertPNG = FALSE) {
    if (convertPNG) filename <- sub("\\.pdf$", "\\.png", t$filename)
    else            filename <- t$filename

    paste("<table border='1'>",
          "<tr><td align='center'>", t$hash, "</td></tr>\n",
          "<tr><td align='center'>", t$desc, "<br>\n",
          "  <img src='", filename , "'></td></tr>\n",
          "</table>\n", sep="")
  }

  if (convertpng) {
    # Conversion to PNG
    # Set the input and output directories
    indir  <- file.path("visual_test", subdir)
    outdir <- file.path("visual_test", "png", subdir)
    unlink(outdir, recursive= TRUE)
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

    convert_pdf2png(testinfo, indir, outdir)  # Convert the images to PNG
  } else {
    outdir <- file.path("visual_test", subdir)
  }

  outfile <- file.path(outdir, "index.html")
  message("Writing ", outfile)

  write(paste("<html><head><title>ggplot2 tests: ", subdir,
              "</title></head><body><h1>ggplot2 tests: ", subdir,
              "</h1>\n", sep = ""), outfile)

  # Get the list of info about all tests, then write information about each of the items
  write(sapply(testinfo, item_html, convertpng), outfile, sep = "\n", append = TRUE)

  write("</body></html>", outfile, append = TRUE)
}


# Generate the PNG images for a directory
convert_pdf2png <- function(t, indir, outdir) {
  filenames <- sapply(t, "[[", "filename")           # Filenames without path
  infiles <- filenames[grepl("\\.pdf$", filenames)]  # Keep the .pdf files only
  outfiles <- sub("\\.pdf$", ".png", infiles)

  # Prepend paths
  infiles  <- file.path(indir, infiles)
  outfiles <- file.path(outdir, outfiles)

  message("Converting ", length(infiles), " PDF files in ", indir, " to PNG in ", outdir)

  # Convert multiple PNGs by building a command string like this:
  # convert \( a.pdf -write a.png +delete \) \( b.pdf -write b.png +delete \) null:

  cmd <- NULL
  for (i in seq_along(infiles)) {
    cmd <- c(cmd, "\\(", infiles[i], "-density", "72x72", "-write", outfiles[i],
             "+delete", "\\)")
  }

  # Need the these "null:" to suppress convert warnings, for some reason
  cmd <- c(cmd, "null:", "null:")

  system2("convert", cmd)
}





# Make visual diff from two refs
# TODO: when convertpng==TRUE, don't convert PDFs twice if they're identical
vdiff <- function(ref1 = "HEAD", ref2 = "", convertpng = FALSE) {
  # TODO: check color space in conversion
  # TODO: Check 'git' in path, or allow passing in git path
  # TODO: Check imagemagick in path
  # TODO: print message about png option, and slow png vs safari-only pdf
  # TODO: Add subdir option
  # TODO: deal with different file sets in ref1 and ref2
  # TODO: allow ^C termination somehow

  # A function for checking out visual_test from a commit ref, or "" for current state
  checkout_vtests <- function(ref = "", dir = "temp", checkoutdir = "visual_test") {

    unlink(dir, recursive = TRUE)      # Delete existing directory
    dir.create(dir, recursive = TRUE)  # Create the new directory

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
      # Need to reset git index status (this is a little confusing)
      invisible(system2("git", c("reset", "--mixed"), stdout = TRUE))
    }
  }

  # Check we're in top level of the repo
  if (getwd() != system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE))
    stop("This must be run from the top level of the git tree.")

  # The directories for ref1, ref2, and the diffs
  path1 <- file.path("visual_test", "diff", "1")
  path2 <- file.path("visual_test", "diff", "2")
  pathd <- file.path("visual_test", "diff", "diff")

  # Checkout copies of the visual tests into diff/1 and diff/2
  checkout_vtests(ref1, dir = path1, checkoutdir = "visual_test")
  checkout_vtests(ref2, dir = path2, checkoutdir = "visual_test")

  # Find different files using git diff
  dfiles <- system2("git", c("diff", "--name-only", "HEAD"), stdout = TRUE)

  dfiles <- dfiles[grepl("\\.pdf$", dfiles)]
  message("Changed test images:\n", paste(dfiles, collapse="\n"))
  dfiles1 <- file.path(path1, dfiles)
  dfiles2 <- file.path(path2, dfiles)

  unlink(pathd, recursive = TRUE)         # Remove old diff files
  dfilesout <- file.path(pathd, dfiles)   # Where the diff files go
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
    system2("compare", c("-dissimilarity-threshold", "1", f1png, f2png,
                         sub(".pdf$", ".png", dfilesout[i])))
  }

  # Find the subdirs that have testinfo.dat, and generate diff webpages for them
  testdirs <- dirname(list.files(file.path(path2, "visual_test"),
                                 pattern = "testinfo.dat",
                                 recursive = TRUE))

  testdirs <- testdirs[!grepl("^diff", testdirs)]  # Ignore subdirs in diff/

  # Make diff pages for each of these directories
  sapply(testdirs, make_diffpage,
    file.path(path1, "visual_test"), file.path(path2, "visual_test"),
    file.path(pathd, "visual_test"), convertpng = convertpng)

  invisible()
}


make_diffpage <- function(subdir, path1, path2, pathd, convertpng = FALSE) {
  dir1 <- file.path(path1, subdir)  # Files from ref1
  dir2 <- file.path(path2, subdir)  # Files from ref2
  dird <- file.path(pathd, subdir)  # Files for diff

  dir.create(dird, recursive = TRUE, showWarnings = FALSE) # Create diff dir if needed

  # Get the information about the tests
  # TODO: make this robust to changes in tests from dir1 to dir2?
  testinfo <- dget(file.path(dir2, "testinfo.dat"))

  if (convertpng) {
    # Generate PNGs from PDFs, if asked
    convert_pdf2png(testinfo, dir1, dir1)
    convert_pdf2png(testinfo, dir2, dir2)
  }


  outfile <- file.path(pathd, subdir, "index.html")
  message("Writing ", outfile)

  # Write HTML code to show a single test
  item_html <- function(t, dir1, dir2, dird) {
    # TODO: change to use div
    #<div class="float">name <img src="…"/ > </div>
    #.float {float: left; width: 200px}
    
    # The diff file is a png. If convertpng==TRUE, so are the 1/ and 2/ files
    pngfile <- sub("\\.pdf$", ".png", t$filename)
    if (convertpng) reffile <- pngfile       # The filename in dirs 1/ and 2/
    else            reffile <- t$filename

    if (file.exists(file.path(dird, pngfile)))
      diffcontent <- paste("<img src='", pngfile, "'>", sep="")
    else
      diffcontent <- "identical"

    paste("<table border=1>\n",
          "<tr><td align='center' colspan='3'>", t$hash, "</td></tr>\n",
          "<tr><td align='center' colspan='3'>", t$desc, "</td></tr>\n",
          "<tr>\n",
          "  <td><img src='", relativePath(file.path(dir1, reffile), dird), "'></td>\n",
          "  <td><img src='", relativePath(file.path(dir2, reffile), dird), "'></td>\n",
          "  <td>", diffcontent, "</td>\n",
          "</tr>\n",
          "</table>\n", sep="")
  }

  write(paste("<html><head><title>ggplot2 tests: ", subdir,
              "</title></head><body><h1>ggplot2 tests: ", subdir,
              "</h1>\n", sep = ""), outfile)

  write("<table border='1'>\n", outfile, append = TRUE)

  # Write information about all the test items in testinfo
  write(sapply(testinfo, item_html, dir1, dir2, dird), outfile, sep = "\n", append = TRUE)

  write("</table></body></html>", outfile, append = TRUE)

}

# Find path to d, relative to start. If `start` is NULL, use current dir
# if d is ./foo/bar and start is ./foo, then return "bar"
# if d is ./zz and start is ./foo/bar, then return "../../zz"
relativePath <- function(path, start = NULL) {
  if (is.null(start)) start <- getwd()

  p <- strsplit(normalizePath(path,  winslash = "/"), "/")[[1]]
  s <- strsplit(normalizePath(start, winslash = "/"), "/")[[1]]

  len <- min(length(s), length(p))
  lastmatch <- min(which(s[1:len] != p[1:len])) - 1 # last path part that is the same
  lastmatch <- min(lastmatch, len)                  # if no match found, lastmatch <- len
  p <- p[-(1:lastmatch)]                            # remove everything that matches

  # Build the relative path, adding ..'s for each path level in s
  paste(c(rep("..", length(s)-lastmatch), p), collapse="/")
}


# Run all the visual tests
visual_test <- function(pattern = "\\.r$") {
  files <- dir("visual_test", pattern, full.names = TRUE, include.dirs = FALSE)
  lapply(files, source)
}
