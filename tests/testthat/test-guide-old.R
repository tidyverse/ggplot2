skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("old S3 guides can be implemented", {
  my_env <- env()
  my_env$guide_circle <- function() {
    structure(
      list(available_aes = c("x", "y"), position = "bottom"),
      class = c("guide", "circle")
    )
  }

  registerS3method(
    "guide_train",
    "circle",
    function(guide, ...) guide,
    envir = my_env
  )
  registerS3method(
    "guide_transform",
    "circle",
    function(guide, ...) guide,
    envir = my_env
  )
  registerS3method(
    "guide_merge",
    "circle",
    function(guide, ...) guide,
    envir = my_env
  )
  registerS3method(
    "guide_geom",
    "circle",
    function(guide, ...) guide,
    envir = my_env
  )
  registerS3method(
    "guide_gengrob",
    "circle",
    function(guide, ...) {
      absoluteGrob(
        gList(circleGrob()),
        height = unit(1, "cm"),
        width = unit(1, "cm")
      )
    },
    envir = my_env
  )

  withr::local_environment(my_env)
  withr::local_options(lifecycle_verbosity = "quiet")

  my_guides <- guides(x = guide_circle())
  expect_length(my_guides$guides, 1)
  expect_s3_class(my_guides$guides[[1]], "guide")

  expect_doppelganger(
    "old S3 guide drawing a circle",
    ggplot(mtcars, aes(disp, mpg)) +
      geom_point() +
      my_guides
  )
})
