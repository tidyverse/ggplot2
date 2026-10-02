skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("old S3 guides can be implemented", {

  guide_circle <- function() {
    structure(
      list(available_aes = c("x", "y"), position = "bottom"),
      class = c("guide", "circle_guide")
    )
  }

  registerS3method(
    "guide_train",
    "circle_guide",
    function(guide, ...) guide
  )
  registerS3method(
    "guide_transform",
    "circle_guide",
    function(guide, ...) guide
  )
  registerS3method(
    "guide_merge",
    "circle_guide",
    function(guide, ...) guide
  )
  registerS3method(
    "guide_geom",
    "circle_guide",
    function(guide, ...) guide
  )
  registerS3method(
    "guide_gengrob",
    "circle_guide",
    function(guide, ...) {
      absoluteGrob(
        gList(circleGrob()),
        height = unit(1, "cm"),
        width = unit(1, "cm")
      )
    }
  )

  withr::local_options(lifecycle_verbosity = "quiet")

  my_guides <- guides(x = guide_circle())
  expect_length(my_guides$guides, 1)
  expect_s3_class(my_guides$guides[[1]], "guide")

  gt <- ggplotGrob(
    ggplot(mtcars, aes(disp, mpg)) +
      geom_point() +
      my_guides
  )

  axis <- gtable_filter(gt, "axis-b")$grobs[[1]]
  expect_s3_class(
    axis$children[[1]],
    "circle"
  )
})
