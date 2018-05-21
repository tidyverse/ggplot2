context("Fortify")

test_that("spatial polygons have correct ordering", {
  skip_if_not_installed("sp")

  make_square <- function(x = 0, y = 0, height = 1, width = 1){
    delx <- width/2
    dely <- height/2
    sp::Polygon(matrix(c(x + delx, x - delx,x - delx,x + delx,x + delx ,
        y - dely,y - dely,y + dely,y + dely,y - dely), ncol = 2))
  }

  make_hole <- function(x = 0, y = 0, height = .5, width = .5){
    p <- make_square(x = x, y = y, height = height, width = width)
    p@hole <- TRUE
    p
  }

  fake_data <- data.frame(ids = 1:5, region = c(1,1,2,3,4))
  rownames(fake_data) <- 1:5
  polys <- list(sp::Polygons(list(make_square(), make_hole()), 1),
                sp::Polygons(list(make_square(1,0), make_square(2, 0)), 2),
                sp::Polygons(list(make_square(1,1)), 3),
                sp::Polygons(list(make_square(0,1)), 4),
                sp::Polygons(list(make_square(0,3)), 5))

  polys_sp <- sp::SpatialPolygons(polys)
  fake_sp <- sp::SpatialPolygonsDataFrame(polys_sp, fake_data)

  # now reorder regions
  polys2 <- rev(polys)
  polys2_sp <- sp::SpatialPolygons(polys2)
  fake_sp2 <- sp::SpatialPolygonsDataFrame(polys2_sp, fake_data)

  expect_equivalent(fortify(fake_sp), plyr::arrange(fortify(fake_sp2), id, order))
})

test_that("fortify.default proves a helpful error with class uneval", {
  expect_error(
    ggplot(aes(x = x)),
    regexp = paste(
      "`data` must be a data frame, or other object coercible by (.+)",
      "Did you accidentally pass `aes\\(\\)` to the `data` argument?",
      sep = "\\n"
    )
  )
})
