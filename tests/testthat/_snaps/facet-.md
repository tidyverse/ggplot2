# facet_grid() fails if passed both a formula and a vars()

    `rows` must be "NULL" or a `vars()` list if `cols` is a `vars()` list

# can't pass formulas to `cols`

    `cols` must be a `vars()` specification or `NULL`, not a <formula> object.

# facet gives clear error if 

    Faceting variables can only appear in `rows` or `cols`, not both.
    i Duplicated variables: "x"

---

    `rows` must be "NULL" or a `vars()` list if `cols` is a `vars()` list
    i Did you use `%>%` or `|>` instead of `+`?

---

    A grid facet specification can't have more than two dimensions

---

    `cols` must be a `vars()` specification or `NULL`, not the string "free".

# at least one layer must contain all facet variables in combine_vars()

    At least one layer must contain all faceting variables: `letter`
    x Plot is missing `letter`
    Layer is missing `letter`

# combine_vars() generates the correct combinations

    At least one layer must contain all faceting variables: `b` and `c`
    x Plot is missing `c`
    x Layer 1 is missing `b`

---

    Faceting variables must have at least one value

# validate_facets() provide meaningful errors

    Please use `vars()` to supply facet variables

---

    Please use `vars()` to supply facet variables
    i Did you use `%>%` or `|>` instead of `+`?

# check_layout() throws meaningful errors

    Facet layout has a bad format. It must contain columns `PANEL`, `SCALE_X`, and `SCALE_Y`

