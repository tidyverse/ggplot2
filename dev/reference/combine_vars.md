# Take input data and define a mapping between faceting variables and ROW, COL and PANEL keys

Take input data and define a mapping between faceting variables and ROW,
COL and PANEL keys

## Usage

``` r
combine_vars(data, env = emptyenv(), vars = NULL, drop = TRUE)
```

## Arguments

- data:

  A list of data.frames, the first being the plot data and the
  subsequent individual layer data

- env:

  The environment the vars should be evaluated in

- vars:

  A list of quoted symbols matching columns in data

- drop:

  should missing combinations/levels be dropped

## Value

A data.frame with columns for PANEL, ROW, COL, and faceting vars
