# Guide constructor

A constructor function for guides, which performs some standard
compatibility checks between the guide and provided arguments.

## Usage

``` r
new_guide(..., available_aes = "any", super)
```

## Arguments

- ...:

  Named arguments that match the parameters of `super$params` or the
  theme elements in `super$elements`.

- available_aes:

  A vector of character strings listing the aesthetics for which the
  guide can be drawn.

- super:

  The super class to use for the constructed guide. Should be a Guide
  class object.

## Value

A `Guide` ggproto object.
