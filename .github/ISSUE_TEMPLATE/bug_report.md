---
name: Bug report
about: Submit a bug report to help us improve ggplot2
---

### Tips for a helpful bug report:

* If you have a question, please don't use this form. Instead, ask on <https://stackoverflow.com/> or <https://community.rstudio.com/>.

* Please include a **minimal reproducible example**, a reprex, to demonstrate the bug.
If you've never heard of a reprex, please read ["Make a reprex"](https://www.tidyverse.org/help/#reprex).
Do not include session info unless it is explicitly asked for.

* If you can, use one of the built-in datasets or a small toy dataset that exposes the bug.
If for some reason, the bug only occurs on your original data, try to limit the number of rows that are necessary to expose the bug. 
Share such data by copying `dput()` output rather than an external file.

* Unless the bug is about the theme, labels, scales or other plot decoration: please omit these from the code.
    
* Please check whether somebody has reported the same problem in the [issues](https://github.com/tidyverse/ggplot2/issues).

Delete these instructions once you have read them.

---

I found a problem with ...

I expected ...

Here is the code to reproduce the bug:

```r
# copy your code to the clipboard and run:
reprex::reprex()
```
