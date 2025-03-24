# layer() checks its input

    `geom` must be either a string or a <Geom> object, not `NULL`.

---

    `stat` must be either a string or a <Stat> object, not `NULL`.

---

    `position` must be either a string or a <Position> object, not `NULL`.

---

    `mapping` must be created by `aes()`.

---

    `mapping` must be created by `aes()`.
    i Did you use `%>%` or `|>` instead of `+`?

---

    Can't find geom called "test".

---

    `environment()` must be either a string or a <geom> object, not an environment.

---

    Failed to retrieve a <Geom> object from `geom_foo()`.
    Caused by error in `geom_foo()`:
    ! This function is unconstructable.

# unknown params create warning

    Ignoring unknown parameters: `blah`

# unknown aesthetics create warning

    Ignoring unknown aesthetics: blah

# empty aesthetics create warning

    Ignoring empty aesthetics: `fill` and `shape`.

# invalid aesthetics throws errors

    Problem while computing aesthetics.
    i Error occurred in the 1st layer.
    Caused by error:
    ! Aesthetics are not valid data columns.
    x The following aesthetics are invalid:
    * `fill = data`
    i Did you mistype the name of a data column or forget to add `after_stat()`?

---

    Problem while mapping stat to aesthetics.
    i Error occurred in the 1st layer.
    Caused by error:
    ! Aesthetics must be valid computed stats.
    x The following aesthetics are invalid:
    * `fill = after_stat(data)`
    i Did you map your stat in the wrong layer?

# missing aesthetics trigger informative error

    Code
      ggplot_build(ggplot(df) + geom_line())
    Condition
      Error in `geom_line()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error in `compute_geom_1()`:
      ! `geom_line()` requires the following missing aesthetics: x and y.

---

    Code
      ggplot_build(ggplot(df) + geom_col())
    Condition
      Error in `geom_col()`:
      ! Problem while setting up geom.
      i Error occurred in the 1st layer.
      Caused by error in `compute_geom_1()`:
      ! `geom_col()` requires the following missing aesthetics: x and y.

# function aesthetics are wrapped with after_stat()

    Problem while computing aesthetics.
    i Error occurred in the 1st layer.
    Caused by error:
    ! Aesthetics are not valid data columns.
    x The following aesthetics are invalid:
    * `colour = density`
    * `fill = density`
    i Did you mistype the name of a data column or forget to add `after_stat()`?

# computed stats are in appropriate layer

    Problem while mapping stat to aesthetics.
    i Error occurred in the 1st layer.
    Caused by error:
    ! Aesthetics must be valid computed stats.
    x The following aesthetics are invalid:
    * `colour = after_stat(density)`
    * `fill = after_stat(density)`
    i Did you map your stat in the wrong layer?

# layer reports the error with correct index etc

    Problem while setting up geom.
    i Error occurred in the 1st layer.
    Caused by error in `compute_geom_1()`:
    ! `geom_linerange()` requires the following missing aesthetics: ymax or xmin and xmax.

---

    Problem while converting geom to grob.
    i Error occurred in the 2nd layer.
    Caused by error in `draw_group()`:
    ! Can only draw one boxplot per group.
    i Did you forget `aes(group = ...)`?

# layer warns for constant aesthetics

    All aesthetics have length 1, but the data has 32 rows.
    i Please consider using `annotate()` or provide this layer with data containing a single row.

# layer names can be resolved

    Code
      p + l + l
    Condition
      Error in `new_layer_names()`:
      ! Names must be unique.
      x These names are duplicated:
        * "foobar" at locations 3 and 4.

# layer_data returns a data.frame

    `layer_data()` must return a <data.frame>.

