test_that("setting guide labels works", {

    expect_identical(xlab("my label")$x, "my label")
    expect_identical(labs(x = "my label")$x, "my label")

    expect_identical(ylab("my label")$y, "my label")
    expect_identical(labs(y = "my label")$y, "my label")

    # Plot titles
    expect_identical(labs(title = "my title")$title, "my title")
    expect_identical(labs(title = "my title",
                          subtitle = "my subtitle")$subtitle, "my subtitle")

    # whole plot annotations
    expect_identical(labs(caption = "my notice")$caption, "my notice")
    expect_identical(labs(title = "my title",
                          caption = "my notice")$caption, "my notice")
    expect_identical(labs(tag = "A)")$tag, "A)")
    expect_identical(labs(title = "my title",
                          tag = "A)")$tag, "A)")

    # Colour
    expect_identical(labs(colour = "my label")$colour, "my label")
    # American spelling
    expect_identical(labs(color = "my label")$colour, "my label")

    # No extra elements exists
    expect_equal(labs(title = "my title"),  list(title = "my title"),  ignore_attr = TRUE)   # formal argument
    expect_equal(labs(colour = "my label"), list(colour = "my label"), ignore_attr = TRUE)   # dot
    expect_equal(labs(foo = "bar"),         list(foo = "bar"),         ignore_attr = TRUE)   # non-existent param

    # labs() has list-splicing semantics
    params <- list(title = "my title", tag = "A)")
    expect_identical(labs(!!!params)$tag, "A)")

    # NULL is preserved
    expect_equal(labs(title = NULL), list(title = NULL), ignore_attr = TRUE)

    # ggtitle works in the same way as labs()
    expect_identical(ggtitle("my title")$title, "my title")
    expect_identical(
      ggtitle("my title", subtitle = "my subtitle")$subtitle,
      "my subtitle"
    )
    expect_equal(
      unclass(ggtitle("my title", subtitle = NULL)),
      list(title = "my title", subtitle = NULL),
      ignore_attr = TRUE
    )
})

test_that("Labels from default stat mapping are overwritten by default labels", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_density2d()

  labels <- get_labs(p)
  expect_equal(labels$colour[1], "colour")
  expect_true(attr(labels$colour, "fallback"))

  p <- p + geom_smooth(aes(color = drv), method = "lm", formula = y ~ x)
  expect_equal(get_labs(p)$colour, "drv")
})

test_that("Labels can be extracted from attributes", {
  df <- mtcars
  attr(df$mpg, "label") <- "Miles per gallon"

  p <- ggplot(df, aes(mpg, disp)) + geom_point()

  labels <- get_labs(p)
  expect_equal(labels$x, "Miles per gallon")
  expect_equal(labels$y, "disp")
})

test_that("Labels from static aesthetics are ignored (#6003)", {

  df <- data.frame(x = 1, y = 1, f = 1)

  p <- ggplot(df, aes(x, y, colour = f)) + geom_point()
  expect_equal(get_labs(p)$colour, "f")

  p <- ggplot(df, aes(x, y, colour = f)) + geom_point(colour = "blue")
  expect_null(get_labs(p)$colour)
})

test_that("alt text is returned", {
  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_point()
  expect_equal(get_alt_text(p), "")
  p <- p + labs(alt = "An alt text")
  expect_equal(get_alt_text(p), "An alt text")
})

test_that("alt text can take a function", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    labs(alt = ~ generate_alt_text(.x))
  expect_snapshot(get_alt_text(p))
})

test_that("get_alt_text checks dots", {
  expect_snapshot_warning(get_alt_text(ggplot(), foo = "bar"))
})

test_that("warnings are thrown for unknown labels", {
  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point() + labs(foo = 'bar')
  expect_snapshot_warning(ggplot_build(p))
})

test_that("plot.tag.position rejects invalid input", {
  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point() + labs(tag = "Fig. A)")

  expect_snapshot_error(
    ggplotGrob(p + theme(plot.tag.position = TRUE))
  )
  expect_snapshot_error(
    ggplotGrob(p + theme(plot.tag.position = "foobar"))
  )
  expect_snapshot(
    ggplotGrob(p + theme(plot.tag.position = c(0, 0.5, 1))),
    error = TRUE
  )
  expect_snapshot(
    ggplotGrob(p + theme(plot.tag.position = c(0, 0), plot.tag.location = "margin")),
    error = TRUE
  )

})

test_that("position axis label hierarchy works as intended", {
  df <- data_frame(foo = c(1e1, 1e5), bar = c(0, 100))

  p <- ggplot(df, aes(foo, bar)) +
    geom_point(size = 5)

  p <- ggplot_build(p)
  resolve_label <- function(x) p$layout$resolve_label(x, p$plot$labels)

  # In absence of explicit title, get title from mapping
  expect_identical(
    resolve_label(p$layout$panel_scales_x[[1]]),
    list(secondary = NULL, primary = "foo")
  )
  expect_identical(
    resolve_label(p$layout$panel_scales_y[[1]]),
    list(primary = "bar", secondary = NULL)
  )

  # Scale name overrules mapping label
  expect_identical(
    resolve_label(scale_x_continuous("Baz")),
    list(secondary = NULL, primary = "Baz")
  )
  expect_identical(
    resolve_label(scale_y_continuous("Qux")),
    list(primary = "Qux", secondary = NULL)
  )

  # Guide titles overrule scale names
  p$layout$setup_panel_guides(
    guides_list(list(x = guide_axis("quuX"), y = guide_axis("corgE"))),
    p$plot$layers
  )
  expect_identical(
    resolve_label(scale_x_continuous("Baz")),
    list(secondary = NULL, primary = "quuX")
  )
  expect_identical(
    resolve_label(scale_y_continuous("Qux")),
    list(primary = "corgE", secondary = NULL)
  )

  # Secondary axis names work
  xsec <- scale_x_continuous("Baz", sec.axis = dup_axis(name = "grault"))
  expect_identical(
    resolve_label(xsec),
    list(secondary = "grault", primary = "quuX")
  )
  ysec <- scale_y_continuous("Qux", sec.axis = dup_axis(name = "garply"))
  expect_identical(
    resolve_label(ysec),
    list(primary = "corgE", secondary = "garply")
  )

  # Secondary guide titles override secondary axis names
  p$layout$setup_panel_guides(
    guides_list(list(x = guide_axis("quuX"), y = guide_axis("corgE"),
                     x.sec = guide_axis("waldo"), y.sec = guide_axis("fred"))),
    p$plot$layers
  )
  expect_identical(
    resolve_label(xsec),
    list(secondary = "waldo", primary = "quuX")
  )
  ysec <- scale_y_continuous("Qux", sec.axis = dup_axis(name = "garply"))
  expect_identical(
    resolve_label(ysec),
    list(primary = "corgE", secondary = "fred")
  )
})

test_that("moving guide positions lets titles follow", {
  df <- data_frame(foo = c(1e1, 1e5), bar = c(0, 100))

  p <- ggplot(df, aes(foo, bar)) +
    geom_point(size = 5)

  p <- ggplot_build(p)

  # Default guide positions
  p$layout$setup_panel_guides(
    guides_list(
      list(x = guide_axis("baz", position = "bottom"),
           y = guide_axis("qux", position = "left"))
    ),
    p$plot$layers
  )
  labs <- get_labs(p)
  expect <- list(x = "baz", x.sec = NULL, y = "qux", y.sec = NULL)
  expect_identical(labs[names(expect)], expect)

  # Guides at secondary positions
  p$layout$setup_panel_guides(
    guides_list(
      list(x = guide_axis("baz", position = "top"),
           y = guide_axis("qux", position = "right"))
    ),
    p$plot$layers
  )
  labs <- get_labs(p)
  expect_identical(labs[names(expect)], expect)

  # Primary guides at secondary positions with
  # secondary guides at primary positions
  p$layout$setup_panel_guides(
    guides_list(
      list(x = guide_axis("baz", position = "top"),
           y = guide_axis("qux", position = "right"),
           x.sec = guide_axis("quux"),
           y.sec = guide_axis("corge"))
    ),
    p$plot$layers
  )
  labs <- get_labs(p)
  expect[c("x.sec", "y.sec")] <- list("quux", "corge")
  expect_identical(labs[names(expect)], expect)
})

test_that("label dictionaries work", {

  p <- ggplot(mtcars, aes(disp, mpg, shape = factor(cyl), size = drat)) +
    geom_point() +
    labs(dictionary = c(
      disp = "Displacement",
      mpg  = "Miles per gallon",
      `factor(cyl)` = "Number of cylinders",
      drat = "Rear axle ratio"
    ))
  p <- ggplot_build(p)

  x <- p$layout$resolve_label(p$layout$panel_scales_x[[1]], p$plot$labels)
  expect_equal(x$primary, "Displacement")

  y <- p$layout$resolve_label(p$layout$panel_scales_y[[1]], p$plot$labels)
  expect_equal(y$primary, "Miles per gallon")

  shape <- p$plot$guides$get_params("shape")$title
  expect_equal(shape, "Number of cylinders")

  size <- p$plot$guides$get_params("size")$title
  expect_equal(size, "Rear axle ratio")
})

# Visual tests ------------------------------------------------------------

test_that("tags are drawn correctly", {
  dat <- data_frame(x = 1:10, y = 10:1)
  p <- ggplot(dat, aes(x = x, y = y)) + geom_point() + labs(tag = "Fig. A)")

  expect_doppelganger("defaults", p)
  expect_doppelganger(
    "Tag in margin",
    p + theme(plot.tag.position = "bottom", plot.tag.location = "margin")
  )
  expect_doppelganger(
    "Tag in panel, as character",
    p + theme(plot.tag.position = "topright", plot.tag.location = "panel")
  )
  expect_doppelganger(
    "Tag in panel, as numeric",
    p + theme(plot.tag.position = c(0.25, 0.25), plot.tag.location = "panel")
  )
  expect_doppelganger(
    "Tag in plot, as character",
    p + theme(plot.tag.position = "bottomleft", plot.tag.location = "plot")
  )
  expect_doppelganger(
    "Tag in plot, as numeric",
    p + theme(plot.tag.position = c(0.05, 0.05), plot.tag.location = "plot")
  )
})
