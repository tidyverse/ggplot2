# Fortify method for classes from the sp package.

**\[deprecated\]**

To figure out the correct variable name for region, inspect
`as.data.frame(model)`.

## Usage

``` r
# S3 method for class 'SpatialPolygonsDataFrame'
fortify(model, data, region = NULL, ...)

# S3 method for class 'SpatialPolygons'
fortify(model, data, ...)

# S3 method for class 'Polygons'
fortify(model, data, ...)

# S3 method for class 'Polygon'
fortify(model, data, ...)

# S3 method for class 'SpatialLinesDataFrame'
fortify(model, data, ...)

# S3 method for class 'Lines'
fortify(model, data, ...)

# S3 method for class 'Line'
fortify(model, data, ...)
```

## Arguments

- model:

  `SpatialPolygonsDataFrame` to convert into a dataframe.

- data:

  not used by this method

- region:

  name of variable used to split up regions

- ...:

  not used by this method
