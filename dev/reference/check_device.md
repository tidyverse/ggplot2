# Check graphics device capabilities

This function makes an attempt to estimate whether the graphics device
is able to render newer graphics features.

## Usage

``` r
check_device(
  feature,
  action = "warn",
  op = NULL,
  maybe = FALSE,
  call = caller_env()
)
```

## Arguments

- feature:

  A string naming a graphics device feature. One of: `"clippingPaths"`,
  `"alpha_masks"`, `"lumi_masks"`, `"compositing"`, `"blending"`,
  `"transformations"`, `"gradients"`, `"patterns"`, `"paths"` or
  `"glyphs"`. See the 'Features' section below for an explanation of
  these terms.

- action:

  A string for what action to take. One of:

  - `"test"` returns `TRUE` or `FALSE` indicating support of the
    feature.

  - `"warn"` also returns a logical, but throws an informative warning
    when `FALSE`.

  - `"abort"` throws an error when the device is estimated to not
    support the feature.

- op:

  A string for a specific operation to test for when `feature` is either
  `"blending"` or `"compositing"`. If `NULL` (default), support for all
  known blending or compositing operations is queried.

- maybe:

  A logical of length 1 determining what the return value should be in
  case the device capabilities cannot be assessed. When the current
  device is the 'null device', `maybe` is returned.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.html). The
  function will be mentioned in warnings and error messages as the
  source of the warning or error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

`TRUE` when the feature is thought to be supported and `FALSE`
otherwise.

## Details

The procedure for testing is as follows:

- First, the R version is checked against the version wherein a feature
  was introduced.

- Next, the
  [dev.capabilities()](https://rdrr.io/r/grDevices/dev.capabilities.html)
  function is queried for support of the feature.

- If that check is ambiguous, the svglite and ragg devices are checked
  for known support.

- Lastly, if there is no answer yet, it is checked whether the device is
  one of the 'known' devices that supports a feature.

## Features

- `"clippingPaths"`:

  While most devices support rectangular clipping regions, this feature
  is about the support for clipping to arbitrary paths. It can be used
  to only display a part of a drawing.

- `"alpha_masks"`:

  Like clipping regions and paths, alpha masks can also be used to only
  display a part of a drawing. In particular a semi-transparent mask can
  be used to display a drawing in the opaque parts of the mask and hide
  a drawing in transparent part of a mask.

- `"lumi_masks`:

  Similar to alpha masks, but using the mask's luminance (greyscale
  value) to determine what is drawn. Light values are opaque and dark
  values are transparent.

- `"compositing"`:

  Compositing allows one to control how to drawings are drawn in
  relation to one another. By default, one drawing is drawn 'over' the
  previous one, but other operators are possible, like 'clear', 'in' and
  'out'.

- `"blending"`:

  When placing one drawing atop of another, the blend mode determines
  how the colours of the drawings relate to one another.

- `"transformations"`:

  Performing an affine transformation on a group can be used to
  translate, rotate, scale, shear and flip the drawing.

- `"gradients"`:

  Gradients can be used to show a transition between two or more colours
  as a fill in a drawing. The checks expects both linear and radial
  gradients to be supported.

- `"patterns"`:

  Patterns can be used to display a repeated, tiled drawing as a fill in
  another drawing.

- `"paths"`:

  Contrary to 'paths' as polyline or polygon drawings, `"paths"` refers
  to the ability to fill and stroke collections of drawings.

- `"glyphs"`:

  Refers to the advanced typesetting feature for controlling the
  appearance of individual glyphs.

## Limitations

- On Windows machines, bitmap devices such as
  [`png()`](https://rdrr.io/r/grDevices/png.html) or
  [`jpeg()`](https://rdrr.io/r/grDevices/png.html) default to
  `type = "windows"`. At the time of writing, these don't support any
  new features, in contrast to `type = "cairo"`, which does. Prior to R
  version 4.2.0, the capabilities cannot be resolved and the value of
  the `maybe` argument is returned.

- With the exception of the ragg and svglite devices, if the device
  doesn't report their capabilities via
  [dev.capabilities()](https://rdrr.io/r/grDevices/dev.capabilities.html),
  or the R version is below 4.2.0, the `maybe` value is returned.

- Even though patterns and gradients where introduced in R 4.1.0, they
  are considered unsupported because providing vectorised patterns and
  gradients was only introduced later in R 4.2.0.

- When using the RStudio graphics device, the back end is assumed to be
  the next device on the list. This assumption is typically met by
  default, unless the device list is purposefully rearranged.

## Examples

``` r
# Typically you'd run `check_device()` inside a function that might produce
# advanced graphics.
# The check is designed for use in control flow statements in the test mode
if (check_device("patterns", action = "test")) {
  print("Yay")
} else {
  print("Nay")
}
#> [1] "Yay"

# Automatically throw a warning when unavailable
if (check_device("compositing", action = "warn")) {
  print("Yay")
} else {
  print("Nay")
}
#> [1] "Yay"

# Possibly throw an error
try(check_device("glyphs", action = "abort"))
#> [1] TRUE
```
