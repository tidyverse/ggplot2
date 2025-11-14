# Find panels in a gtable

These functions help detect the placement of panels in a gtable, if they
are named with "panel" in the beginning. `find_panel()` returns the
extend of the panel area, while `panel_cols()` and `panel_rows()`
returns the columns and rows that contains panels respectively.

## Usage

``` r
find_panel(table)

panel_cols(table)

panel_rows(table)
```

## Arguments

- table:

  A gtable

## Value

A data.frame with some or all of the columns t(op), r(ight), b(ottom),
and l(eft)
