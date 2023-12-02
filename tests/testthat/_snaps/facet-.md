# facet_grid() fails if passed both a formula and a vars()

    Code
      facet_grid(~foo, vars())
    Condition
      Error in `grid_as_facets_list()`:
      ! `rows` must be `NULL` or a `vars()` list if `cols` is a `vars()` list.

# can't pass formulas to `cols`

    Code
      facet_grid(NULL, ~foo)
    Condition
      Error in `grid_as_facets_list()`:
      ! `cols` must be a `vars()` specification or `NULL`, not a <formula> object.

# facet gives clear error if 

    Code
      print(ggplot(df, aes(x)) + facet_grid(x ~ x))
    Condition
      Error in `facet_grid()`:
      ! Faceting variables can only appear in `rows` or `cols`, not both.
      i Duplicated variables: "x"

---

    Code
      print(ggplot(df, aes(x)) %>% facet_grid(. ~ x))
    Condition
      Error in `grid_as_facets_list()`:
      ! `rows` must be `NULL` or a `vars()` list if `cols` is a `vars()` list.
      i Did you use `%>%` or `|>` instead of `+`?

---

    Code
      print(ggplot(df, aes(x)) + facet_grid(list(1, 2, 3)))
    Condition
      Error in `grid_as_facets_list()`:
      ! A grid facet specification can't have more than two dimensions.

---

    Code
      print(ggplot(df, aes(x)) + facet_grid(vars(x), "free"))
    Condition
      Error in `grid_as_facets_list()`:
      ! `cols` must be a `vars()` specification or `NULL`, not the string "free".

# at least one layer must contain all facet variables in combine_vars()

    Code
      combine_vars(list(df), vars = vars(letter = number))
    Condition
      Error in `combine_vars()`:
      ! At least one layer must contain all faceting variables: `letter`
      x Plot is missing `letter`
      Layer is missing `letter`

# combine_vars() generates the correct combinations

    Code
      combine_vars(list(data.frame(a = 1:2, b = 2:3), data.frame(a = 1:2, c = 2:3)),
      vars = vars(b = b, c = c))
    Condition
      Error in `combine_vars()`:
      ! At least one layer must contain all faceting variables: `b` and `c`
      x Plot is missing `c`
      x Layer 1 is missing `b`

---

    Code
      combine_vars(list(data.frame(a = 1:2), data.frame(b = numeric())), vars = vars(
        b = b))
    Condition
      Error in `combine_vars()`:
      ! Faceting variables must have at least one value.

# validate_facets() provide meaningful errors

    Code
      validate_facets(aes(var))
    Condition
      Error in `validate_facets()`:
      ! Please use `vars()` to supply facet variables.

---

    Code
      validate_facets(ggplot())
    Condition
      Error in `validate_facets()`:
      ! Please use `vars()` to supply facet variables.
      i Did you use `%>%` or `|>` instead of `+`?

# check_layout() throws meaningful errors

    Code
      check_layout(mtcars)
    Condition
      Error in `check_layout()`:
      ! Facet layout has a bad format. It must contain columns `PANEL`, `SCALE_X`, and `SCALE_Y`.

