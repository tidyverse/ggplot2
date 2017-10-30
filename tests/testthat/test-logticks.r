context("logticks")

# Make a log-log plot (without log ticks)
 a <- ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point(na.rm = TRUE) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_bw()

test_that("logticks to ticks - default", {

 p1_logticks <- a + annotation_logticks()

 p1_ticks <- a + annotation_ticks(sides = 'bl',scale = rep('log10',2))

 vdiffr::expect_doppelganger("p1_logticks", p1_ticks)

})

test_that("logticks to ticks - Log ticks for y, on left and right", {

 p2_logticks <- a + annotation_logticks(sides = "lr")

 p2_ticks <- a + annotation_ticks(sides = 'lr',scale = rep('log10',2))

 vdiffr::expect_doppelganger("p2_logticks", p2_ticks)

})

test_that("logticks to ticks - All four sides", {

 p3_logticks <- a + annotation_logticks(sides = "trbl")

 p3_ticks <- a + annotation_ticks(sides = 'trbl',scale = rep('log10',4))

 vdiffr::expect_doppelganger("p3_logticks", p3_ticks)

 })

test_that("logticks to ticks - Hide the minor grid lines because they don't align with the ticks", {

 p4_logticks <- a + annotation_logticks(sides = "trbl") + theme(panel.grid.minor = element_blank())

 p4_ticks <- a + annotation_ticks(sides = 'trbl',scale = rep('log10',4)) + theme(panel.grid.minor = element_blank())

 vdiffr::expect_doppelganger("p4_logticks", p4_ticks)

})

test_that("logticks to ticks - log-transform the data before plotting it", {

 # Another way to get the same results as 'a' above: log-transform the data before
 # plotting it. Also hide the minor grid lines.

 b <- ggplot(msleep, aes(log10(bodywt), log10(brainwt))) +
  geom_point(na.rm = TRUE) +
  scale_x_continuous(name = "body", labels = scales::math_format(10^.x)) +
  scale_y_continuous(name = "brain", labels = scales::math_format(10^.x)) +
  theme_bw() + theme(panel.grid.minor = element_blank())

 p5_logticks <- b + annotation_logticks()

 p5_ticks <- b + annotation_ticks(sides = 'bl',scale = rep('log10',2))

 vdiffr::expect_doppelganger("p5_logticks", p5_ticks)

})

test_that("logticks to ticks - Using a coordinate transform requires scaled = FALSE", {

 t <- ggplot(msleep, aes(bodywt, brainwt)) +
   geom_point() +
   coord_trans(x = "log10", y = "log10") +
   theme_bw()

 p6_logticks <- t + annotation_logticks(scaled = FALSE)

 p6_ticks <- t + annotation_ticks(sides = 'bl',scale=rep('log10',2), scaled = FALSE)

 vdiffr::expect_doppelganger("p6_logticks", p6_ticks)

})

test_that("logticks to ticks - Change the length of the ticks", {

 p7_logticks <- a + annotation_logticks(
   short = unit(.5,"mm"),
   mid = unit(3,"mm"),
   long = unit(4,"mm")
 )

 p7_ticks <- a + annotation_ticks(
   sides = 'bl',
   scale = rep('log10',2),
   short = unit(.5,"mm"),
   mid = unit(3,"mm"),
   long = unit(4,"mm")
 )

 vdiffr::expect_doppelganger("p7_logticks", p7_ticks)

})
