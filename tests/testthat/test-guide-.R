skip_on_cran()

test_that("plotting does not induce state changes in guides", {
  guides <- guides(
    x = guide_axis(title = "X-axis"),
    colour = guide_colourbar(title = "Colourbar"),
    shape = guide_legend(title = "Legend"),
    size = guide_bins(title = "Bins")
  )

  p <- ggplot(
    mpg,
    aes(displ, hwy, colour = cty, shape = factor(cyl), size = cyl)
  ) +
    geom_point() +
    guides

  # Preserve S7 class references while copying the mutable guide state.
  # Serializing these references also copies installation-specific environments.
  class_refs <- list()
  snapshot <- serialize(as.list(p@guides), NULL, refhook = function(x) {
    if (!inherits(x, "S7_class_ref")) {
      return(NULL)
    }
    class_refs[[length(class_refs) + 1L]] <<- x
    as.character(length(class_refs))
  })

  grob <- ggplotGrob(p)

  expect_identical(
    as.list(p@guides),
    unserialize(snapshot, refhook = function(x) class_refs[[as.integer(x)]])
  )
})

test_that("adding guides doesn't change plot state", {
  p1 <- ggplot(mtcars, aes(disp, mpg))

  expect_length(p1@guides$guides, 0)

  p2 <- p1 + guides(y = guide_axis(angle = 45))

  expect_length(p1@guides$guides, 0)
  expect_length(p2@guides$guides, 1)

  p3 <- p2 + guides(y = guide_axis(angle = 90))

  expect_length(p3@guides$guides, 1)
  expect_equal(p3@guides$guides[[1]]$params$angle, 90)
  expect_equal(p2@guides$guides[[1]]$params$angle, 45)
})

test_that("dots are checked when making guides", {
  expect_snapshot_warning(
    new_guide(foo = "bar", super = GuideAxis)
  )
  expect_snapshot_warning(
    guide_legend(foo = "bar")
  )
})
