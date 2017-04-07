context("Munch")

test_that("interp works", {
    single_interp_test <- function(start, end, n) {
        i <- interp(start, end, n)
        info <- paste0("start: ", start, "; end: ", end, "; n: ", n)
        expect_equal(length(i), n, info = info)
        expect_true(start %in% i, info = info)
        expect_false(end %in% i, info = info)
        expect_true(all(i >= start), info = info)
        expect_true(all(i <= end), info = info)
    }
    single_interp_test(0, 1, 1)
    single_interp_test(0, 1, 2)
    single_interp_test(0, 1, 7)
    single_interp_test(-23, 56, 1)
    single_interp_test(-23, 56, 4)
    single_interp_test(31.276, 34.443, 1)
    single_interp_test(31.276, 34.443, 100)
})

test_that("munch_data works", {
    single_munch_test <- function(data, dist=NULL, segment_length = 0.01) {
        md <- munch_data(data, dist, segment_length)
        # all rows of dat are in md
        expect_equal(nrow(merge(md, dat)), nrow(dat))
        expect_true(nrow(md) >= nrow(dat))
    }
    dat <- data.frame(x =     c(0,  60, 30, 20, 40, 45),
                      y =     c(1,  1,  2,  2,  2,  2),
                      group = c(1L, 1L, 1L, 2L, 2L, 2L))
    dist <- dist_euclidean(dat$x, dat$y)
    dist[dat$group[-1] != dat$group[-nrow(dat)]] <- NA
    single_munch_test(dat, dist)
    single_munch_test(dat, dist, segment_length = 10)
    single_munch_test(dat, dist, segment_length = 100)
    dist <- coord_polar(theta = "x")$distance(dat$x, dat$y,
                           list(r.range = range(c(0,dat$y)),
                                theta.range = range(dat$x)))
    dist[dat$group[-1] != dat$group[-nrow(dat)]] <- NA
    single_munch_test(dat, dist)
    single_munch_test(dat, dist, segment_length = 10)
    single_munch_test(dat, dist, segment_length = 100)
    dist <- coord_polar(theta = "y")$distance(dat$x, dat$y,
                           list(r.range = range(c(0,dat$x)),
                                theta.range = range(dat$y)))
    dist[dat$group[-1] != dat$group[-nrow(dat)]] <- NA
    single_munch_test(dat, dist)
    single_munch_test(dat, dist, segment_length = 10)
    single_munch_test(dat, dist, segment_length = 100)
})
