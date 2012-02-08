\name{update_element}
\alias{update_element}
\title{Update theme param}
\usage{
  update_element(name, ...)
}
\arguments{
  \item{name}{name of a theme element}

  \item{...}{Pairs of name and value of theme parameters.}
}
\value{
  Updated theme element
}
\description{
  Update contents of a theme
}
\examples{
x <- theme_text(size = 15)
update_element(x, colour = "red")
# Partial matching works
update_element(x, col = "red")
# So does positional
update_element(x, "Times New Roman")
# And it throws an error if you use an argument that doesn't exist
\donttest{update_element(x, noargument = 12)}
# Or multiple arguments with the same name
\donttest{update_element(x, size = 12, size = 15)}

# Will look up element if given name
update_element("axis.text.x", colour = 20)
# Throws error if incorrectly named
\donttest{update_element("axis.text", colour = 20)}
}

