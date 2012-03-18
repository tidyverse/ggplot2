# Set the context of the visual tests
# This creates vis_context and vis_info in the Global environment

get_vcontext <- NULL
set_vcontext <- NULL
get_vtestinfo <- NULL
append_vtestinfo <- NULL

local({
  context <- NULL
  testinfo <- NULL
  count <- 1

  get_vcontext <<- function() context
  set_vcontext <<- function(value) {
    context <<- value
    testinfo <<- list()
  }
  get_vtestinfo <<- function() testinfo
  append_vtestinfo <<- function(value) {
    testinfo <<- c(testinfo, list(c(value, id = count)))
    count <<- count + 1
  }
})


# Start a visual test context
vcontext <- function(context) {
  if (!is.null(get_vcontext()))
    stop("Can't open new context while current context is still open. Use finish_vcontext().")
  set_vcontext(context)
  message(context, appendLF = FALSE)
}

# Finish a visual test context.
# This will generate the web page for the context (a version of the webpage with PDFs)
# TODO: add a function that will force-close the context, even if something bad happened in the middle.
finish_vcontext <- function() {
  message("")         # Print a newline

  make_vtest_webpage()

  # TODO: find a better way to do this
  if (isTRUE(globalenv()$convertpng))  # might be NULL, so test with isTRUE
    make_vtest_webpage(convertpng = TRUE) # also generate PNG version

  # Save the test information into a file
  dput(get_vtestinfo(), file.path("visual_test", get_vcontext(), "testinfo.dat"))
  # Reset the context
  set_vcontext(NULL)
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
# * convertpng: if TRUE, create a parallel visual_test/png/ subdir, create a
#     webpage in the dir, and convert the source PDF files to PNG.
# Reads from vcontext and vtestinfo
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

    convertfiles <- sapply(testinfo, "[[", "filename")
    convert_pdf2png(convertfiles, indir, outdir)       # Convert the images to PNG
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
convert_pdf2png <- function(filenames, indir = NULL, outdir = NULL) {
  infiles <- filenames[grepl("\\.pdf$", filenames)]  # Keep the .pdf files only
  outfiles <- sub("\\.pdf$", ".png", infiles)

  # Prepend paths if specified; otherwise assume that 'filenames' has a full path
  if (!is.null(indir))   infiles  <- file.path(indir, infiles)
  if (!is.null(outdir))  outfiles <- file.path(outdir, outfiles)

  message("Converting ", length(infiles), " PDF files")

  # Convert multiple PNGs by building a command string like this:
  # convert \( a.pdf -write a.png +delete \) \( b.pdf -write b.png +delete \) null:

  args <- NULL
  for (i in seq_along(infiles)) {
    args <- c(args, "\\(", infiles[i], "-density", "72x72", "-write", outfiles[i],
             "+delete", "\\)")
  }

  # Need the these "null:" to suppress convert warnings, for some reason
  args <- c(args, "null:", "null:")

  system2("convert", args)
}





# Make visual diff from two refs
# TODO: when convertpng==TRUE, don't convert PDFs twice if they're identical
vdiff <- function(ref1 = "HEAD", ref2 = "", convertpng = FALSE, prompt = TRUE) {
  # TODO: check color space in conversion
  # TODO: Check imagemagick in path
  # TODO: print message about png option, and slow png vs safari-only pdf
  # TODO: Add subdir option
  # TODO: deal with different file sets in ref1 and ref2
  # TODO: allow ^C termination somehow
  # TODO: Check out work tree in tempdir()

  if (ref1 == "")  stop('ref1 must not be blank "" (because git doesn\'t like it)')

  # A function for checking out visual_test from a commit ref, or "" for current state
  checkout <- function(ref = "", dir = NULL, paths = "") {

    if (is.null(dir))  dir <- file.path(tempdir(), "gitcheckout")

    # TODO: change this - it's very dangerous if someone uses "/"!
    unlink(dir, recursive = TRUE)      # Delete existing directory
    dir.create(dir, recursive = TRUE)  # Create the new directory

    if (ref == "") {
      # If blank ref, simply copy the files over from the working tree
      files <- list.files(paths, recursive = TRUE, full.names = TRUE)

      # Find which directories need to be created, and then create them
      newdirs <- unique(file.path(dir, dirname(files)))
      sapply(newdirs, dir.create, recursive = TRUE, showWarnings = FALSE)
      # Copy the files over
      file.copy(files, file.path(dir, files))

    } else {
      # Checkout the git ref into dir
      if (system2("git", c("--work-tree", dir, "checkout", ref, "--", paths)) != 0)
        stop("git checkout failed.")
      # Need to reset git index status after the checkout (so git doesn't get confused)
      system2("git", c("reset", "--mixed"), stdout = TRUE)
    }
  }

  # Check we're in top level of the repo
  if (getwd() != system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE))
    stop("This must be run from the top level of the git tree.")

  if (prompt) {
    resp <- readline(paste("This will unstage changes to the git index, so if you have staged any changes",
      "with 'git add' or similar commands, you will need to restage them. This will",
      "not change files in the working tree. (Use `prompt=FALSE` to disable this messsage.)",
      "Continue? (y/n) ", sep="\n"))
    if (tolower(resp) != "y")
      return(invisible())
  }

  # The directories for ref1, ref2, and the diffs
  path1 <- normalizePath(file.path(tempdir(), "vdiff1"))
  path2 <- normalizePath(file.path(tempdir(), "vdiff2"))
  pathd <- normalizePath(file.path(tempdir(), "diff"))

# NOTE: can't handle vdiff("", "HEAD"), but vdiff("HEAD", "") works. This is because
# diff --name-status only works in one direction when comparing to working tree

  # Checkout the files for ref1
  checkout(ref1, dir = path1, paths = "visual_test")

  # These are the files that were added or modified between ref1 and ref2
  # We already checked out ref1, so now we'll check out these specific files from ref2
  changed <- read.table(text = system2("git", c("diff", "--name-status",
                          ref1, ref2), stdout = TRUE), stringsAsFactors = FALSE)
  changed <- setNames(changed, c("status", "filename"))
  # We only care about files in visual_test/
  changed <- subset(changed, grepl("^visual_test/", filename))
  # The Modified and Added files
  ref2_changed <- subset(changed, (status =="M" | status=="A"), select = filename, drop = TRUE)

  # Check out from ref1 only the Modified and Deleted files
  checkout(ref2, dir = path2, paths = ref2_changed)


  # Generate diff image only for Modified files
if(F) {
  dfiles <- subset(changed, status == "M" & grepl("\\.pdf", filename), select = filename, drop = TRUE)
  message("Changed test PDF images:\n", paste(dfiles, collapse="\n"))
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

    system2("convert", c("-density", "72", f1, f1png))
    system2("convert", c("-density", "72", f2, f2png))
    system2("compare", c("-dissimilarity-threshold", "1", f1png, f2png,
                         sub(".pdf$", ".png", dfilesout[i])))
  }
}
  # Find the subdirs that have testinfo.dat, and generate diff webpages for them
  testdirs <- dirname(list.files(file.path(path1, "visual_test"),
                                 pattern = "testinfo.dat",
                                 recursive = TRUE))

  # TODO: remove if no problems -- this shouldn't be needed:
  #testdirs <- testdirs[!grepl("^diff", testdirs)]  # Ignore ^diff subdirs in the diff dir

  # Make diff pages for each of these directories
  for (t in testdirs) {
    # Just the changed files in this directory
    cfiles <- subset(changed, grepl(paste("visual_test/", t, "/", sep = ""), filename))
    # Strip off the leading path part of the filename
    cfiles$filename <- sub(paste("visual_test/", t, "/", sep = ""), "",
                           cfiles$filename, fixed = TRUE)
    make_diffpage(cfiles,
                  file.path(path1, "visual_test", t),
                  file.path(path2, "visual_test", t), 
                  file.path(pathd, "visual_test", t), convertpng = convertpng)
  }
  # sapply(testdirs, make_diffpage, changedm path1, path2, pathd, convertpng = convertpng)

  invisible()
}


# TODO: Make the webpages work!
#   - make use of 'changed'
#   - finihs changing "dirx" to "pathx"
# Make a web page with diffs between one path and another path
# This assumes that they contain all the same files. If they don't, it won't be happy.
make_diffpage <- function(changed, path1, path2, pathd, convertpng = FALSE) {

  dir.create(pathd, recursive = TRUE, showWarnings = FALSE) # Create diff dir if needed

  # Get the information about the tests
  testinfo1 <- dget(file.path(path1, "testinfo.dat"))
  if (file.exists(file.path(path2, "testinfo.dat")))
    testinfo2 <- dget(file.path(path2, "testinfo.dat"))
  else
    testinfo2 <- testinfo1   # If testinfo2 doesn't exist, then it's unchanged from testinfo1

  # Convert them from nested lists to data frames
  testinfo1 <- ldply(testinfo1, data.frame)
  testinfo2 <- ldply(testinfo2, data.frame)

  # We want to merge 'changed' together with testinfo1 and testinfo2
  # This is a little tricky
  testinfo1 <- merge(changed, testinfo1, by = "filename", all.y = TRUE)
  testinfo2 <- merge(changed, testinfo2, by = "filename", all.y = TRUE)
  testinfo <- merge(testinfo2, testinfo2, all = TRUE)

  testinfo$status[is.na(testinfo$status)] <- "U" # Set status to U for unchanged files


  # Figure out which files need to be converted to png.
  # All Modified files need to be converted (so they can be diffed)
  dfiles <- testinfo$filename[testinfo$status == "D"]  # Deleted files (in path1)
  afiles <- testinfo$filename[testinfo$status == "A"]  # Added files (in path2)
  mfiles <- testinfo$filename[testinfo$status == "M"]  # Modified files (in path1 and path2), also create a diff
  ufiles <- testinfo$filename[testinfo$status == "U"]  # Unchanged files (in path1)
  convertfiles <- c(file.path(path1, mfiles), file.path(path2, mfiles))

  # Add in the other files, if asked
  if (convertpng) {
    convertfiles <- c(convertfiles, file.path(path1, c(dfiles, ufiles)),
                                    file.path(path2, afiles))
  }

  # TODO: move this to the bottom
  convert_pdf2png(convertfiles)

# TODO: continue from here, only convert files that exist, link to single copy if no change
browser()

  outfile <- file.path(pathd, subdir, "index.html")
  message("Writing ", outfile)

  # Write HTML code to show a single test
  item_html <- function(t, path1, path2, pathd) {
    # TODO: change to use div
    #<div class="float">name <img src="…"/ > </div>
    #.float {float: left; width: 200px}
    
    # The diff file is a png. If convertpng==TRUE, so are the 1/ and 2/ files
    pngfile <- sub("\\.pdf$", ".png", t$filename)
    if (convertpng) reffile <- pngfile       # The filename in dirs 1/ and 2/
    else            reffile <- t$filename

    if (file.exists(file.path(pathd, pngfile)))
      diffcontent <- paste("<img src='", pngfile, "'>", sep="")
    else
      diffcontent <- "identical"

    paste("<table border=1>\n",
          "<tr><td align='center' colspan='3'>", t$hash, "</td></tr>\n",
          "<tr><td align='center' colspan='3'>", t$desc, "</td></tr>\n",
          "<tr>\n",
          "  <td><img src='", relativePath(file.path(path1, reffile), pathd), "'></td>\n",
          "  <td><img src='", relativePath(file.path(path2, reffile), pathd), "'></td>\n",
          "  <td>", diffcontent, "</td>\n",
          "</tr>\n",
          "</table>\n", sep="")
  }

  write(paste("<html><head><title>ggplot2 tests: ", subdir,
              "</title></head><body><h1>ggplot2 tests: ", subdir,
              "</h1>\n", sep = ""), outfile)

  write("<table border='1'>\n", outfile, append = TRUE)

  # Write information about all the test items in testinfo
  write(sapply(testinfo, item_html, path1, path2, pathd), outfile, sep = "\n", append = TRUE)

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
# * convertpng: if TRUE, generate the PNG versions of the web page as well.
visual_test <- function(pattern = "\\.r$", convertpng = FALSE) {
  # TODO: There must be a better way to do this. This is horrible
  # This is used so that we can enable PNG versions of the web pages, without altering
  #   the individual test scripts.
  .GlobalEnv$convertpng <- convertpng

  files <- dir("visual_test", pattern, full.names = TRUE, include.dirs = FALSE)
  lapply(files, source)
}
