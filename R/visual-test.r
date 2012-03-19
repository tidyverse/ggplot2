# Set the context of the visual tests

get_vcontext <- NULL
set_vcontext <- NULL
get_vtestinfo <- NULL
append_vtestinfo <- NULL

local({
  context <- NULL
  testinfo <- NULL
  count <- NULL

  get_vcontext <<- function() context
  set_vcontext <<- function(value) {
    context <<- value
    testinfo <<- data.frame()
    count <<- 1
  }
  get_vtestinfo <<- function() testinfo
  append_vtestinfo <<- function(value) {
    testinfo <<- rbind(testinfo, cbind(value, data.frame(id = count)))
    count <<- count + 1
  }
})


# Run visual tests
vtest <- function(filter = NULL) {
  if (!file.exists("visual_test")) 
    return()

  files <- dir("visual_test", filter, full.names = TRUE, include.dirs = FALSE)
  files <- files[grepl("\\.[rR]$", files)]
  lapply(files, source)

  f_quote    <- ifelse(is.null(filter), '', paste('"', filter, '"', sep = ""))
  fopt_quote <- ifelse(is.null(filter), '', paste('filter="', filter, '"', sep = ""))
  message("\nRun vtest_webpage(", f_quote, ") to generate web pages for viewing tests.\n",
    "Run vdiffstat(", fopt_quote, ") to see what files have changed.\n",
    "Run vdiff_webpage(", fopt_quote,
    ") to generate web pages comparing results to another commit in the git repository.\n",
    "If you have added new tests, remember to add the output files to the git repository.")

  # If this is moved out of ggplot2, should check for visual_test/.gitignore
}


# Start a visual test context
vcontext <- function(context) {
  # Check we're in top level of the repo
  if (getwd() != system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE))
    stop("This must be run from the top level of the git tree.")

  if (!is.null(get_vcontext()))
    stop("Can't open new context while current context is still open. Use finish_vcontext().")
  set_vcontext(context)
  message(context, appendLF = FALSE)
  unlink(file.path("visual_test", context), recursive = TRUE)
}

# Finish a visual test context.
# This will generate the web page for the context (a version of the webpage with PDFs)
end_vcontext <- function() {
  # Save the test information into a file
  dput(get_vtestinfo(), file.path("visual_test", get_vcontext(), "testinfo.dat"))

  set_vcontext(NULL)  # Reset the context
  message("")         # Print a newline
}


# Save an individual test to file, and record information using append_vtestinfo
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
  if (is.null(get_vcontext()))     stop("Must have active vcontext")
  if (is.null(desc) || desc == "") stop("desc must not be empty")
  if (device != "pdf")             stop('Only "pdf" device supported at this time')

  # Put files in visual_test/<subdir>, where subdir is the vcontext
  destdir <- file.path("visual_test", get_vcontext())
  dir.create(destdir, showWarnings = FALSE)    # Create dir if missing

  # Save it to a temporary file
  temppdf <- tempfile("vtest", fileext = ".pdf")
  ggsave(temppdf, width = width, height = height, dpi = dpi,
    device = match.fun(device), compress = FALSE)

  # Load the tempfile and modify the CreationDate and ModDate (lines 5 and 6)
  # so that the files are exactly the same, regardless of date + time.
  temppdf_fd <- file(temppdf, "r")
  pdftext <- readLines(temppdf_fd)
  close(temppdf_fd)

  if (!grepl("^/CreationDate ", pdftext[5]) || !grepl("^/ModDate ", pdftext[6]))
    stop("Unexpected structure of PDF file. CreationDate or ModDate not found in right place in ",
         temppdf)

  pdftext[5] <- "/CreationDate (D:00000000000000)"
  pdftext[6] <- "/ModDate (D:00000000000000)"

  # Write the modified PDF file to the final destination file
  hash <- digest(desc)
  filename <- paste(hash, device, sep=".")
  outpdf <- file(file.path(destdir, filename), "w")
  writeLines(pdftext, outpdf)
  close(outpdf)

  # Remove temp file
  unlink(temppdf)

  # Append the info for this test in the vis_info list
  append_vtestinfo(data.frame(filename = filename, desc = desc, hash = hash,
                    type = device, width = width, height = height, dpi = dpi,
                    stringsAsFactors = FALSE))

  message(".", appendLF = FALSE)
}


# =============================================================
# Functions for generating web pages to view tests
# =============================================================

# This is the function that the user calls
# * convertpng: if TRUE, convert the source PDFs files to PNG instead.
# TODO: Create overall index file?
vtest_webpage <- function(filter = "", convertpng = TRUE) {
  # Find subdirs with testinfo.dat
  dirs <- dirname(list.files("visual_test", pattern = "testinfo.dat",
                             recursive = TRUE))

  dirs <- dirs[grepl(filter, dirs)]
  dirs <- dirs[!grepl("^diff/", dirs)]
  dirs <- dirs[!grepl("^html/", dirs)]

  for(d in dirs) {
    make_vtest_webpage(file.path("visual_test", d), 
      outdir = file.path("visual_test", "html", d), convertpng = convertpng)
  }

  # Copy the css file
  file.copy(file.path("visual_test", "style.css"), file.path("visual_test", "html"),
            overwrite = TRUE)
  invisible()
}


# Make a single web page (user shouldn't use this function)
# TODO: display filename if it differs from hash
make_vtest_webpage <- function(dir = NULL, outdir = NULL, convertpng = TRUE) {
  if (is.null(dir))     stop("dir cannot be  NULL")
  if (is.null(outdir))  stop("outdir cannot be  NULL")

  # Read in the information about the tests
  testinfo <- dget(file.path(dir, "testinfo.dat"))

  # Sort by id
  testinfo <- testinfo[order(testinfo$id), ]

  unlink(outdir, recursive= TRUE)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  if (convertpng)
    convert_pdf2png(testinfo$filename, dir, outdir)
  else
    file.copy(file.path(dir, testinfo$filename), outdir)

  htmlfile <- file.path(normalizePath(outdir), "index.html")
  message("Writing ", htmlfile)

  # Get the name of the subdirectory of visual tests
  vname <- strsplit(dir, "/")[[1]]
  vname <- vname[length(vname)]

  write(paste('<html><head>\n',
              '<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />',
              '<title>Visual tests: ', vname,
              '</title></head><body><h1>Visual tests: ', vname,
              '</h1>\n', sep = ""), htmlfile)

  # Write HTML code to show a single test
  item_html <- function(t, convertpng = FALSE) {
    if (convertpng) f <- sub("\\.pdf$", "\\.png", t$filename)
    else            f <- t$filename

    paste('<div class="float">\n',
          '  <div class="description">', t$desc, '<p class="name">', t$hash, '</p>', '</div>\n',
          '  <div class="imageset">\n',
          '    <span class="imagewrap">\n',
          '      <div class="image"><img src="', f, '"></div>\n',
          '    </span>\n',
          '  </div>\n',
          '</div>\n', sep="")
  }

  # Get the list of info about all tests, then write information about each of the items
  for (i in seq_len(nrow(testinfo))) {
    write(item_html(testinfo[i, ], convertpng), htmlfile, append = TRUE)
  }

  write('</body></html>', htmlfile, append = TRUE)
}



# =============================================================
# Functions for generating visual diff pages
# =============================================================


# Find files modified between ref1 and ref2
# If ref2 is "" (the working tree), then we don't know exactly which of the new files
# the user plans to commit. So we just assume all new files in the working tree are
# added files (marked with A).
vdiffstat <- function(ref1 = "HEAD", ref2 = "", filter = "") {
  if (ref1 == "")  stop('ref1 must not be blank "" (because git doesn\'t like it)')

  changed <- read.table(text = system2("git", c("diff", "--name-status",
                          ref1, ref2), stdout = TRUE), stringsAsFactors = FALSE)
  changed <- setNames(changed, c("status", "filename"))
  changed <- subset(changed, grepl("^visual_test/", filename))

  # Special case where ref2 is the working tree. This is a bit hacky. Add all
  # the untracked files in the visual_test dir. Because they're not committed, we
  # can't tell exactly which files *should* be compared. So copy all the untracked
  # files over.
  if (ref2 == "") {
    wfiles <- system2("git", c("ls-files", "--other", "--exclude-standard", "visual_test/"),
              stdout = TRUE)
    changed <- rbind(changed, data.frame(status = "A",  filename = wfiles,
                                         stringsAsFactors = FALSE))
  }

  # use 'filter' on the second part of the path (right after visual_test/)
  cpaths <- strsplit(changed$filename,"/")
  changed[grepl(filter, sapply(cpaths, "[[", 2)), ]
}


# Make visual diff from two refs
# TODO: Create overall index file, with status
vdiff_webpage <- function(ref1 = "HEAD", ref2 = "", filter = "", convertpng = TRUE,
                  method = "ghostscript", prompt = TRUE) {
  # TODO: message about weird color space in conversion using convert
  # TODO: print message about png option, and slow png vs safari-only pdf
  # TODO: display filename if it differs from hash

  # TODO: de-hard code this?
  cssfile <- file.path("visual_test", "style.css")

  if (ref1 == "")  stop('ref1 must not be blank "" (because git doesn\'t like it)')

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
  path1 <- normalizePath(file.path("visual_test", "diff", "1"), mustWork = FALSE)
  path2 <- normalizePath(file.path("visual_test", "diff", "2"), mustWork = FALSE)
  pathd <- normalizePath(file.path("visual_test", "diff", "diff"), mustWork = FALSE)

  # Checkout the files for ref1
  checkout_worktree(ref1, outdir = path1, paths = "visual_test")

  # Find what changed between ref1 and ref2
  changed <- vdiffstat(ref1, ref2)

  # The Modified and Added files
  ref2_changed <- subset(changed, (status =="M" | status=="A"), select = filename, drop = TRUE)

  # Check out from ref1 only the Modified and Deleted files
  checkout_worktree(ref2, outdir = path2, paths = ref2_changed)


  # Copy the CSS file over to the diff/visual_test dir
  dir.create(file.path(pathd, "visual_test"), recursive = TRUE, showWarnings = FALSE)
  css_outfile <- file.path(pathd, "visual_test", basename(cssfile))
  file.copy(cssfile, css_outfile, overwrite = TRUE)

  # Find the subdirs that have testinfo.dat, and generate diff webpages for them
  testdirs <- dirname(list.files(file.path(path1, "visual_test"),
                                 pattern = "testinfo.dat",
                                 recursive = TRUE))

  testdirs <- testdirs[grepl(filter, testdirs)]

  # Make diff pages for each of these directories
  for (t in testdirs) {
    # Just the changed files in this directory
    cfiles <- subset(changed, grepl(paste("visual_test/", t, "/", sep = ""), filename))
    # Strip off the leading path part of the filename
    cfiles$filename <- sub(paste("visual_test/", t, "/", sep = ""), "",
                           cfiles$filename, fixed = TRUE)
    make_diffpage(cfiles, name = t,
                  file.path(path1, "visual_test", t),
                  file.path(path2, "visual_test", t),
                  file.path(pathd, "visual_test", t),
                  cssfile = css_outfile,
                  convertpng = convertpng, method = method,
                  refnames = c(ref1, ifelse(ref2 == "", "working tree", ref2)))
  }

  invisible()
}


# Make a web page with diffs between one path and another path
# This assumes that they contain all the same files. If they don't, it won't be happy.
make_diffpage <- function(changed, name = "", path1, path2, pathd, cssfile,
    convertpng = FALSE, method = "ghostscript", refnames = c("","")) {

  dir.create(pathd, recursive = TRUE, showWarnings = FALSE) # Create diff dir if needed

  # Get the information about the tests
  testinfo1 <- dget(file.path(path1, "testinfo.dat"))
  if (file.exists(file.path(path2, "testinfo.dat")))
    testinfo2 <- dget(file.path(path2, "testinfo.dat"))
  else
    testinfo2 <- testinfo1   # If testinfo2 doesn't exist, then it's unchanged from testinfo1

  # We want to merge 'changed' together with testinfo1 and testinfo2
  # This is a little tricky
  testinfo1 <- merge(changed, testinfo1, by = "filename", all.y = TRUE)
  testinfo2 <- merge(changed, testinfo2, by = "filename", all.y = TRUE)

  # id numbers can change if a test is inserted. So we have to do some acrobatics.
  mergeby <- intersect(names(testinfo1), names(testinfo2))
  mergeby <- mergeby[mergeby != "id"]
  testinfo <- merge(testinfo1, testinfo2, by = mergeby, all = TRUE)

  # In the special case where comparing a commit-ref against working dir (""),
  # added files won't have an "A" in changed$status (which is from git diff --name-status).
  # We can detect these cases, because they will be missing id.x, and manually
  # set the status to A.
  testinfo$status[is.na(testinfo$id.x) & !is.na(testinfo$id.y)] <- "A"

  testinfo <- arrange(testinfo, id.x, id.y)  # Order by ref1 and then ref2
  testinfo$id <- seq_len(nrow(testinfo))     # Assign new id (used for ordering items)

  testinfo$status[is.na(testinfo$status)] <- "U" # Set status to U for unchanged files

  # Figure out which files need to be converted to png.
  # All Modified files need to be converted (so they can be diffed)
  dfiles <- testinfo$filename[testinfo$status == "D"]  # Deleted files (in path1)
  afiles <- testinfo$filename[testinfo$status == "A"]  # Added files (in path2)
  mfiles <- testinfo$filename[testinfo$status == "M"]  # Modified files (in path1 and path2), also create a diff
  ufiles <- testinfo$filename[testinfo$status == "U"]  # Unchanged files (in path1)

  convertfiles <- c(file.path(path1, mfiles), file.path(path2, mfiles))

  # Add in the other files to convert, if convertpng==TRUE
  if (convertpng) {
    convertfiles <- c(convertfiles, file.path(path1, c(dfiles, ufiles)),
                                    file.path(path2, afiles))
  }


  outfile <- file.path(pathd, "index.html")
  message("Writing ", outfile)

  # Write HTML code to show a single test
  item_html <- function(t, path1, path2, pathd, convertpng) {

    # The diff file is a png. If convertpng==TRUE, so are the 1/ and 2/ files
    pngfile <- sub("\\.pdf$", ".png", t$filename)
    if (convertpng) reffile <- pngfile       # The filename in dirs 1/ and 2/
    else            reffile <- t$filename

    if (t$status == "D") {           # Deleted file
      status <- "changed"
      cell1 <- paste("<img src='", file.path(relativePath(path1, pathd), reffile), "'>", sep="")
      cell2 <- "Not present"
      celld <- "NA"
    } else if (t$status == "A") {    # Added file
      status <- "changed"
      cell1 <- "Not present"
      cell2 <- paste("<img src='", file.path(relativePath(path2, pathd), reffile), "'>", sep="")
      celld <- "NA"    
    } else if (t$status == "M") {    # Modified file
      status <- "changed"
      cell1 <- paste("<img src='", file.path(relativePath(path1, pathd), reffile), "'>", sep="")
      cell2 <- paste("<img src='", file.path(relativePath(path2, pathd), reffile), "'>", sep="")
      celld <- paste("<img src='", pngfile, "'>", sep="")
    } else if (t$status == "U") {    # Unchanged file
      status <- "unchanged"
      cell1 <- paste("<img src='", file.path(relativePath(path1, pathd), reffile), "'>", sep="")
      cell2 <- cell1
      celld <- "Identical"
    }

    paste('<div class="float"><div class="', status, '">\n',
          '  <div class="description">', t$desc, '<p class="name">', t$hash, '</p>', '</div>\n',
          '  <div class="imageset">\n',
          '    <span class="imagewrap">\n',
          '      <div><span class="refspec">', refnames[1],'</span></div>\n',
          '      <div class="image">', cell1, '</div>\n',
          '    </span>\n',
          '    <span class="imagewrap">\n',
          '      <div><span class="refspec">', refnames[2],'</span></div>\n',
          '      <div class="image">', cell2, '</div>\n',
          '    </span>\n',
          '    <span class="imagewrap">\n',
          '      <div>Difference</div>\n',
          '      <div class="image">', celld, '</div>\n',
          '    </span>\n',
          '  </div>\n',
          '</div></div>\n', sep="")
  }

  write(paste('<html><head>\n',
        '<link rel="stylesheet" type="text/css" href="../style.css" media="screen" />',
        '<title>Visual tests diffs: ', name,
        '</title></head><body>\n',
        '<h1>Visual tests diffs: ', name, '</h1>\n',
        '<h2>Comparing <span class="refspec">', refnames[1],
        '</span> to <span class="refspec">', refnames[2],
        '</span></h2>\n',
        '<h2>', sum(testinfo$status != "U"), ' of ', nrow(testinfo),
        ' test files changed.</h2>\n', sep = ""), outfile)

  # Write information about all the test items in testinfo
  for (i in seq_len(nrow(testinfo))) {
    write(item_html(testinfo[i, ], path1, path2, pathd, convertpng), outfile, append = TRUE)
  }

  write("</table></body></html>", outfile, append = TRUE)

  convert_pdf2png(convertfiles, method = method)

  mfilespng <- sub("\\.pdf$", ".png", mfiles)  # For the compared files, use png
  compare_png(file.path(path1, mfilespng),
              file.path(path2, mfilespng),
              file.path(pathd, mfilespng))

}



# =============================================================
# Utility functions
# =============================================================


# Generate the PNG images for a directory
convert_pdf2png <- function(filenames, indir = NULL, outdir = NULL, method = "ghostscript") {
  if (length(filenames) == 0) return()
  infiles <- filenames[grepl("\\.pdf$", filenames)]  # Keep the .pdf files only
  outfiles <- sub("\\.pdf$", ".png", infiles)

  # Prepend paths if specified; otherwise assume that 'filenames' has a full path
  if (!is.null(indir))   infiles  <- file.path(indir, infiles)
  if (!is.null(outdir))  outfiles <- file.path(outdir, outfiles)

  message("Converting ", length(infiles), " PDF files to PNG, using method ", method)

  # Convert multiple PNGs by building a command string like this:
  # convert \( a.pdf -write a.png +delete \) \( b.pdf -write b.png +delete \) null:

  if (method == "ghostscript") {
    for (i in seq_along(infiles)) {
      system2("gs", c("-dNOPAUSE", "-dBATCH", "-sDEVICE=png16m", "-r72",
        "-dTextAlphaBits=4", "-dGraphicsAlphaBits=4",
        paste("-sOutputFile=", outfiles[i], sep=""), infiles[i]), stdout = TRUE)
    }

  } else if (method == "imagemagick") {
    args <- NULL
    for (i in seq_along(infiles)) {
      args <- c(args, "\\(", infiles[i], "-density", "72x72", "-write", outfiles[i],
               "+delete", "\\)")
    }

    # Need the these "null:" to suppress convert warnings, for some reason
    args <- c(args, "null:", "null:")

    system2("convert", args)

  } else {
    stop("Unknown method.")
  }
}


# Compare png files
compare_png <- function(files1, files2, filesout) {
  if (length(files1) == 0) return()
  message("Comparing ", length(files1), " pairs of images")

  # Not sure how to build a single command line string to compare (as was done
  #   with convert in convert_pdf2png), so do them individually.
  for (i in seq_along(files1)) {
    system2("compare", c("-dissimilarity-threshold", "1", files1[i], files2[i], filesout[i]))
  }
}


# A function for checking out a path (like "visual_test) from a commit ref,
#  or use "" for current state
checkout_worktree <- function(ref = "", outdir = NULL, paths = "") {
  if (is.null(outdir))  outdir <- file.path(tempdir(), "checkout-workdir")

  # TODO: change this - it's dangerous if someone uses "/"!
  unlink(outdir, recursive = TRUE)      # Delete existing directory
  dir.create(outdir, recursive = TRUE)  # Create the new directory

  if (ref == "") {
    # If blank ref, simply copy the files over from the working tree
    # First get the (non-dir) files only, then recurse into directories
    dirs <- file.info(paths)$isdir
    files <- paths[!dirs]
    files <- c(files, list.files(paths[dirs], recursive = TRUE, full.names = TRUE))

    # Find which directories need to be created, and then create them
    newdirs <- unique(file.path(outdir, dirname(files)))
    sapply(newdirs, dir.create, recursive = TRUE, showWarnings = FALSE)
    # Copy the files over
    file.copy(files, file.path(outdir, files))

  } else {
    # Checkout the git ref into outdir
    if (system2("git", c("--work-tree", outdir, "checkout", ref, "--", paths)) != 0)
      stop("git checkout failed.")
    # Need to reset git index status after the checkout (so git doesn't get confused)
    system2("git", c("reset", "--mixed"), stdout = TRUE)
  }
}


# Find path to d, relative to start. If `start` is NULL, use current dir
# if d is ./foo/bar and start is ./foo, then return "bar"
# if d is ./zz and start is ./foo/bar, then return "../../zz"
relativePath <- function(path, start = NULL) {
  if (is.null(start)) start <- getwd()

  # If either of these fail (with a warning), it'll give an incorrect relative
  # path, so throw an error.
  tryCatch({
    p <- strsplit(normalizePath(path,  winslash = "/"), "/")[[1]]
    s <- strsplit(normalizePath(start, winslash = "/"), "/")[[1]]
  }, warning = function(w) stop(w) )

  len <- min(length(s), length(p))
  # Find if any of these pieces are different. If so, that's the first mismatch;
  #   if not, then the next piece is the first mismatch.
  mismatches <- s[1:len] != p[1:len]
  if (any(mismatches))  lastmatch <- min(which(mismatches)) - 1
  else                  lastmatch <- len

  p <- p[-(1:lastmatch)]                            # remove everything that matches

  # Build the relative path, adding ..'s for each path level in s
  paste(c(rep("..", length(s)-lastmatch), p), collapse="/")
}
