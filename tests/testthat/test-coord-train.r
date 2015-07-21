context("coord_train")

test_that("NA's don't appear in breaks", {

  # Returns true if any major/minor breaks have an NA
  any_NA_major_minor <- function(trained) {
    ns <- names(trained)[grepl("(\\.major)|(\\.minor)$", names(trained))]

    for (n in ns) {
      if (!is.null(trained[n]) && any(is.na(trained[n])))
        return(TRUE)
    }

    return(FALSE)
  }

  scales <- list(x = scale_x_continuous(limits=c(1, 12)),
                 y = scale_y_continuous(limits=c(1, 12)))

  # First have to test that scale_breaks_positions will return a vector with NA
  # This is a test to make sure the later tests will be useful!
  # It's possible that changes to the the way that breaks are calculated will
  # make it so that scale_break_positions will no longer give NA for range 1, 12
  expect_true(any(is.na(scale_break_positions(scales$x))))
  expect_true(any(is.na(scale_break_positions(scales$y))))

  # Check the various types of coords to make sure they don't have NA breaks
  expect_false(any_NA_major_minor(coord_train(coord_polar(), scales)))
  expect_false(any_NA_major_minor(coord_train(coord_cartesian(), scales)))
  expect_false(any_NA_major_minor(coord_train(coord_trans(), scales)))
  expect_false(any_NA_major_minor(coord_train(coord_fixed(), scales)))
  expect_false(any_NA_major_minor(coord_train(coord_map(), scales)))
})
