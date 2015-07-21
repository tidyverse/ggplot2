context("Fortify")
library(sp)

test_that("Spatial polygons have correct ordering", {
  make_square <- function(x = 0, y = 0, height = 1, width = 1){
    delx <- width/2
    dely <- height/2
    Polygon(matrix(c(x + delx, x - delx,x - delx,x + delx,x + delx ,
        y - dely,y - dely,y + dely,y + dely,y - dely), ncol = 2))
  }

  make_hole <- function(x = 0, y = 0, height = .5, width = .5){
    p <- make_square(x = x, y = y, height = height, width = width)
    p@hole <- TRUE
    p
  }

  fake_data <- data.frame(ids = 1:5, region = c(1,1,2,3,4))
  rownames(fake_data) <- 1:5
  polys <- list(Polygons(list(make_square(), make_hole()), 1),
                Polygons(list(make_square(1,0), make_square(2, 0)), 2),
                Polygons(list(make_square(1,1)), 3),
                Polygons(list(make_square(0,1)), 4),
                Polygons(list(make_square(0,3)), 5))


  polys_sp <- SpatialPolygons(polys)
  fake_sp <- SpatialPolygonsDataFrame(polys_sp, fake_data)

  # now reorder regions
  polys2 <- rev(polys)
  polys2_sp <- SpatialPolygons(polys2)
  fake_sp2 <- SpatialPolygonsDataFrame(polys2_sp, fake_data)

  expect_equivalent(fortify(fake_sp), arrange(fortify(fake_sp2), id, order))

})
