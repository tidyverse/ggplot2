context("Aes - setting values")

test_that("Aesthetic parameters must match length of data", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot(df, aes(x, y))

  set_colours <- function(colours) {
    pdf(file=NULL)
    print(p + geom_point(colour = colours))
    dev.off()
  }

  set_colours("red")
  expect_error(set_colours(rep("red", 2)), "Incompatible lengths")
  dev.off()  # Need to manually close device because of error
  expect_error(set_colours(rep("red", 3)), "Incompatible lengths")
  dev.off()
  expect_error(set_colours(rep("red", 4)), "Incompatible lengths")
  dev.off()
  set_colours(rep("red", 5))


})
