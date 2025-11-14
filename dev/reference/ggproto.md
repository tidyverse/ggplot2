# Create a new ggproto object

Construct a new object with `ggproto()`, test with
[`is_ggproto()`](https://ggplot2.tidyverse.org/dev/reference/is_tests.md),
and access parent methods/fields with `ggproto_parent()`.

## Usage

``` r
ggproto(`_class` = NULL, `_inherit` = NULL, ...)

ggproto_parent(parent, self)
```

## Arguments

- \_class:

  Class name to assign to the object. This is stored as the class
  attribute of the object. This is optional: if `NULL` (the default), no
  class name will be added to the object.

- \_inherit:

  ggproto object to inherit from. If `NULL`, don't inherit from any
  object.

- ...:

  A list of named members in the ggproto object. These can be functions
  that become methods of the class or regular objects.

- parent, self:

  Access parent class `parent` of object `self`.

## Details

ggproto implements a protype based OO system which blurs the lines
between classes and instances. It is inspired by the proto package, but
it has some important differences. Notably, it cleanly supports
cross-package inheritance, and has faster performance.

In most cases, creating a new OO system to be used by a single package
is not a good idea. However, it was the least-bad solution for ggplot2
because it required the fewest changes to an already complex code base.

## Calling methods

ggproto methods can take an optional `self` argument: if it is present,
it is a regular method; if it's absent, it's a "static" method (i.e. it
doesn't use any fields).

Imagine you have a ggproto object `Adder`, which has a method
`addx = function(self, n) n + self$x`. Then, to call this function, you
would use `Adder$addx(10)` – the `self` is passed in automatically by
the wrapper function. `self` be located anywhere in the function
signature, although customarily it comes first.

## Calling methods in a parent

To explicitly call a methods in a parent, use
`ggproto_parent(Parent, self)`.

## Working with ggproto classes

The ggproto objects constructed are build on top of environments, which
has some ramifications. Environments do not follow the 'copy on modify'
semantics one might be accustomed to in regular objects. Instead they
have ['modify in
place'](https://adv-r.hadley.nz/names-values.html#env-modify) semantics.

## See also

The [ggproto introduction
section](https://ggplot2-book.org/internals#sec-ggproto) of the online
ggplot2 book.

## Examples

``` r
Adder <- ggproto("Adder",
  x = 0,
  add = function(self, n) {
    self$x <- self$x + n
    self$x
  }
 )
is_ggproto(Adder)
#> [1] TRUE

Adder$add(10)
#> [1] 10
Adder$add(10)
#> [1] 20

Doubler <- ggproto("Doubler", Adder,
  add = function(self, n) {
    ggproto_parent(Adder, self)$add(n * 2)
  }
)
Doubler$x
#> [1] 20
Doubler$add(10)
#> [1] 40
```
