# Format or print a ggproto object

If a ggproto object has a `$print` method, this will call that method.
Otherwise, it will print out the members of the object, and optionally,
the members of the inherited objects.

## Usage

``` r
# S3 method for class 'ggproto'
print(x, ..., flat = TRUE)

# S3 method for class 'ggproto'
format(x, ..., flat = TRUE)
```

## Arguments

- x:

  A ggproto object to print.

- ...:

  If the ggproto object has a `print` method, further arguments will be
  passed to it. Otherwise, these arguments are unused.

- flat:

  If `TRUE` (the default), show a flattened list of all local and
  inherited members. If `FALSE`, show the inheritance hierarchy.

## Examples

``` r
Dog <- ggproto(
  print = function(self, n) {
    cat("Woof!\n")
  }
 )
Dog
#> Woof!
cat(format(Dog), "\n")
#> <ggproto object: Class gg>
#>     print: function 
```
