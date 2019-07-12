
test_that("the `complete` paramter in stat_contour() throws a deprecation warning", {
  p <- ggplot(faithfuld, aes(waiting, eruptions)) +
    stat_contour(aes(z = density, col = stat(level)), complete = TRUE)

  expect_warning(ggplot_build(p), "`complete` is deprecated")
})


isoline_to_tbl <- function(cl_iso) {
  new_data_frame(
    list(
      level = as.numeric(names(cl_iso)),
      contours = cl_iso
    ),
    n = length(cl_iso)
  )
}

tbl_to_isoline <- function(tbl) {
  cl_iso <- tbl$contours
  names(cl_iso) <- as.character(tbl$level)
  class(cl_iso) <- c("isolines", "iso")
  cl_iso
}

xyz <- data_frame(x = faithfuld$waiting, y = faithfuld$eruptions, z = faithfuld$density)
breaks <- c(0, 0.01, 0.02, 0.03, 0.04)

cl_iso <- xyz_to_isoline(xyz, breaks)

x <- 10*1:ncol(volcano)
y <- 10*1:nrow(volcano)

breaks <- c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190)

cl_iso <- isoband::isolines(x, y, volcano, levels = breaks)

tbl_to_isoline(isoline_to_tbl(cl_iso))





