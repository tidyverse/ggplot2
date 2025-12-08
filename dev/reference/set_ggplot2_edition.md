# Set ggplot2 edition

ggplot2 uses the 'edition' concept to manage the lifecycles of functions
and arguments. Setting a recent edition opens up the latest features but
also closes down deprecated and superseded functionality.

## Usage

``` r
set_ggplot2_edition(edition = NULL)

local_ggplot2_edition(edition = NULL, env = parent.frame())

with_ggplot2_edition(edition = NULL, code)

get_ggplot2_edition()
```

## Arguments

- edition:

  An edition. Possible values currently include `"2026"` only. Can be
  `NULL` (default) to unset an edition.

- env:

  An `environment` to use for scoping.

- code:

  Code to execute in the temporary environment.

## Value

For `set_ggplot2_edition()`, the previous `edition` value and for
`local_ggplot2_edition()`: `NULL`. Thesef unction are called for the
side effect of setting the edition though. For `with_ggplot2_edition`,
the result of the evaluation. For `get_ggplot2_edition()`, the currently
active edition.

## Examples

``` r
# Setting an edition
set_ggplot2_edition(2026)

# Getting the edition
get_ggplot2_edition()
#> [1] "2026"


# Unsetting an edition
set_ggplot2_edition()

# Using withr-like scoping
with_ggplot2_edition(2026, get_ggplot2_edition())
#> [1] "2026"
```
