# make_constructor builds a geom constructor

    Code
      print(geom_foo)
    Output
      function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
          ..., my_param = "foo", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
      {
          match.arg(my_param, c("foo", "bar"))
          layer(mapping = mapping, data = data, geom = "foo", stat = stat, 
              position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
              params = list2(na.rm = na.rm, my_param = my_param, ...))
      }
      <environment: {censored}>

# make_constructor builds a stat constructor

    Code
      print(stat_foo)
    Output
      function (mapping = NULL, data = NULL, geom = "point", position = "identity", 
          ..., my_param = "foo", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
      {
          match.arg(my_param, c("foo", "bar"))
          layer(mapping = mapping, data = data, geom = geom, stat = "foo", 
              position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
              params = list2(na.rm = na.rm, my_param = my_param, ...))
      }
      <environment: {censored}>

# make_constructor refuses overdefined cases

    Code
      make_constructor(GeomPoint, geom = "line")
    Condition
      Error in `make_constructor()`:
      ! `geom` is a reserved argument.

---

    Code
      make_constructor(StatDensity, geom = "point", stat = "smooth")
    Condition
      Error in `make_constructor()`:
      ! `stat` is a reversed argument.

# make_constructor complains about default values

    In `geom_foo()`: please consider providing default values for: my_param.

---

    In `stat_foo()`: please consider providing default values for: my_param.

# make_constructor rejects bad input for `checks`

    Code
      make_constructor(GeomPoint, checks = 10)
    Condition
      Error in `make_constructor()`:
      ! `checks` must be a list of calls, such as one constructed with `rlang::exprs()`.

---

    Code
      make_constructor(StatDensity, geom = "line", checks = "A")
    Condition
      Error in `make_constructor()`:
      ! `checks` must be a list of calls, such as one constructed with `rlang::exprs()`.

