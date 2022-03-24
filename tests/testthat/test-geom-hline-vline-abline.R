
# Visual tests ------------------------------------------------------------

test_that("check h/v/abline transformed on basic projections", {
  dat <- data_frame(x = LETTERS[1:5], y = 1:5)
  plot <- ggplot(dat, aes(x, y)) +
    geom_col(width = 1) +
    geom_point() +
    geom_vline(xintercept = 3, colour = "red") +
    geom_hline(yintercept = 3, colour = "blue") +
    geom_abline(intercept = 0, slope = 1, colour = "purple") +
    labs(x = NULL, y = NULL) +
    coord_cartesian(expand = FALSE)

  expect_doppelganger(
    "cartesian lines intersect mid-bars",
    plot
  )
  expect_doppelganger(
    "flipped lines intersect mid-bars",
    plot + coord_flip()
  )
  expect_doppelganger(
    "polar lines intersect mid-bars",
    plot + coord_polar()
  )
})

test_that("curved lines in map projections", {
  skip_if(packageVersion("base") < "3.5.0")
  nz <- subset(map_data("nz"), region == "North.Island ")
  nzmap <- ggplot(nz, aes(long, lat, group = group)) +
    geom_path() +
    geom_hline(yintercept = -38.6) + # roughly Taupo
    geom_vline(xintercept = 176) +
    coord_map()

  expect_doppelganger("straight lines in mercator",
    nzmap
  )
  expect_doppelganger("lines curved in azequalarea",
    nzmap + coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0))
  )
})

# Warning tests ------------------------------------------------------------

test_that("warn_overwritten_args() produces gramatically correct error messages", {
  expect_warning(
    warn_overwritten_args("fun_test", "is_overwritten", "provided"),
    "fun_test: Ignoring `is_overwritten` because `provided` was provided."
  )
  expect_warning(
    warn_overwritten_args("fun_test", "is_overwritten", c("provided1", "provided2")),
    "fun_test: Ignoring `is_overwritten` because `provided1` and/or `provided2` were provided."
  )
  expect_warning(
    warn_overwritten_args("fun_test", "is_overwritten", c("provided1", "provided2", "provided3")),
    "fun_test: Ignoring `is_overwritten` because `provided1`, `provided2`, and/or `provided3` were provided."
  )
})

test_that("Warning if a supplied mapping is going to be overwritten", {

  expect_warning(
    geom_vline(xintercept = 3, aes(colour = colour)),
    "Ignoring `mapping`"
  )

  expect_warning(
    geom_hline(yintercept = 3, aes(colour = colour)),
    "Ignoring `mapping`"
  )

  expect_warning(
    geom_abline(intercept = 3, aes(colour = colour)),
    "Ignoring `mapping`"
  )

  expect_warning(
    geom_abline(intercept = 3, slope = 0.5, aes(colour = colour)),
    "Ignoring `mapping`"
  )

  expect_warning(
    geom_abline(slope = 0.5, aes(colour = colour)),
    "Ignoring `mapping`"
  )
})


test_that("Warning if supplied data is going to be overwritten", {

  sample_data <- data_frame(x = 1)

  expect_warning(
    geom_vline(xintercept = 3, data = sample_data),
    "Ignoring `data`"
  )

  expect_warning(
    geom_hline(yintercept = 3, data = sample_data),
    "Ignoring `data`"
  )

  expect_warning(
    geom_abline(intercept = 3, data = sample_data),
    "Ignoring `data`"
  )

  expect_warning(
    geom_abline(intercept = 3, slope = 0.5, data = sample_data),
    "Ignoring `data`"
  )

  expect_warning(
    geom_abline(slope = 0.5, data = sample_data),
    "Ignoring `data`"
  )
})
