# layer() checks its input

    Can't create layer without a geom.

---

    Can't create layer without a stat.

---

    Can't create layer without a position.

---

    `mapping` must be created by `aes()`

---

    `mapping` must be created by `aes()`
    i Did you use `%>%` or `|>` instead of `+`?

---

    Can't find geom called "test"

---

    geom must be either a string or a <geom> object, not a base object of type <environment>

# invalid aesthetics throws errors

    Problem while computing aesthetics.
    i Error occurred in the 1st layer.
    Caused by error in `compute_aesthetics()`:
    ! Aesthetics are not valid data columns.
    x The following aesthetics are invalid:
    x `fill = data`
    i Did you mistype the name of a data column or forget to add `after_stat()`?

---

    Problem while mapping stat to aesthetics.
    i Error occurred in the 1st layer.
    Caused by error in `map_statistic()`:
    ! Aesthetics must be valid computed stats.
    x The following aesthetics are invalid:
    x `fill = after_stat(data)`
    i Did you map your stat in the wrong layer?

# function aesthetics are wrapped with after_stat()

    Problem while computing aesthetics.
    i Error occurred in the 1st layer.
    Caused by error in `compute_aesthetics()`:
    ! Aesthetics are not valid data columns.
    x The following aesthetics are invalid:
    x `colour = NULL`
    x `fill = NULL`
    i Did you mistype the name of a data column or forget to add `after_stat()`?

# computed stats are in appropriate layer

    Problem while mapping stat to aesthetics.
    i Error occurred in the 1st layer.
    Caused by error in `map_statistic()`:
    ! Aesthetics must be valid computed stats.
    x The following aesthetics are invalid:
    x `colour = NULL`
    x `fill = NULL`
    i Did you map your stat in the wrong layer?

# layer reports the error with correct index etc

    Problem while setting up geom.
    i Error occurred in the 1st layer.
    Caused by error in `compute_geom_1()`:
    ! `geom_linerange()` requires the following missing aesthetics: ymax or xmin and xmax

---

    Problem while converting geom to grob.
    i Error occurred in the 2nd layer.
    Caused by error in `draw_group()`:
    ! Can only draw one boxplot per group
    i Did you forget `aes(group = ...)`?

# layer_data returns a data.frame

    `layer_data()` must return a <data.frame>

