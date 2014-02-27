context("Creating aesthetic mappings")

test_that("function aes", {
  expect_equal(aes(x = mpg, y = wt),
               structure(list(x = bquote(mpg), y = bquote(wt)), class = "uneval"))

  expect_equal(aes(x = mpg ^ 2, y = wt / cyl),
               structure(list(x = bquote(mpg ^ 2), y = bquote(wt / cyl)), class = "uneval"))

})

test_that("function aes_string", {
  expect_equal(aes_string(x = "mpg", y = "wt"),
               structure(list(x = bquote(mpg), y = bquote(wt)), class = "uneval"))

  expect_equal(aes_string(x = "mpg ^ 2", y = "wt / cyl"),
               structure(list(x = bquote(mpg ^ 2), y = bquote(wt / cyl)), class = "uneval"))
})

test_that("function aes_all", {
  expect_equal(aes_all(names(mtcars)),
               structure(
                 list(
                   mpg = bquote(mpg),
                   cyl = bquote(cyl),
                   disp = bquote(disp),
                   hp = bquote(hp),
                   drat = bquote(drat),
                   wt = bquote(wt),
                   qsec = bquote(qsec),
                   vs = bquote(vs),
                   am = bquote(am),
                   gear = bquote(gear),
                   carb = bquote(carb)),
                 class = "uneval"))

  expect_equal(aes_all(c("x", "y", "col", "pch")),
               structure(list(x = bquote(x), y = bquote(y), colour = bquote(col), shape = bquote(pch)), class = "uneval"))
})

test_that("function aes_auto", {
  df <- data.frame(x = 1, y = 1, colour = 1, label = 1, pch = 1)
  expect_equal(aes_auto(df),
               structure(list(colour = bquote(colour), label = bquote(label), shape = bquote(pch), x = bquote(x), y = bquote(y)), class = "uneval"))

  expect_equal(aes_auto(names(df)),
               structure(list(colour = bquote(colour), label = bquote(label), shape = bquote(pch), x = bquote(x), y = bquote(y)), class = "uneval"))

  df <- data.frame(xp = 1:3, y = 1:3, colour = 1:3, txt = letters[1:3], foo = 1:3)
  expect_equal(aes_auto(df, x = xp, label = txt),
               structure(list(colour = bquote(colour), y = bquote(y), x = bquote(xp), label = bquote(txt)), class = "uneval"))
  expect_equal(aes_auto(names(df), x = xp, label = txt),
               structure(list(colour = bquote(colour), y = bquote(y), x = bquote(xp), label = bquote(txt)), class = "uneval"))
  expect_equal(aes_auto(x = xp, label = txt, data = df),
               structure(list(colour = bquote(colour), y = bquote(y), x = bquote(xp), label = bquote(txt)), class = "uneval"))

  df <- data.frame(foo = 1:3)
  expect_equal(aes_auto(df, x = xp, y = yp),
               structure(list(x = bquote(xp), y = bquote(yp)), class = "uneval"))
  expect_equal(aes_auto(df), structure(setNames(list(), character(0)), class = "uneval"))
})

