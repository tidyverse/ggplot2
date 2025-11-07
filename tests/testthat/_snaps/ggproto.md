# construction checks input

    All members of a <ggproto> object must be named.

---

    All members of a <ggproto> object must be named.

---

    `_inherit` must be a <ggproto> object, not a <data.frame> object.

# ggproto objects print well

    Code
      print(Foo)
    Output
      <ggproto object: Class Foo, gg>
          empty: NULL
          env: environment
          method: function
          num: 12
          theme: theme, ggplot2::theme, gg, S7_object

---

    Code
      print(Foo$method)
    Output
      <ggproto method>
        <Wrapper function>
          function(...) !!call2(name, !!!args)
      
        <Inner function (f)>
          function(x) print(x)

