# Contributing to ggplot2 development

The goal of this guide is to help you get up and contributing to ggplot2 as 
quickly as possible. The guide is divided into two main pieces:

1. Filing a bug report or feature request in an issue.
1. Suggesting a change via a pull request.

Please note that ggplot2 is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project, 
you agree to abide by its terms.

## Issues

When filing an issue, the most important thing is to include a minimal 
reproducible example so that we can quickly verify the problem, and then figure 
out how to fix it. There are three things you need to include to make your 
example reproducible: required packages, data, code.

1.  **Packages** should be loaded at the top of the script, so it's easy to
    see which ones the example needs.
  
1.  The easiest way to include **data** is to use `dput()` to generate the R code 
    to recreate it. For example, to recreate the `mtcars` dataset in R,
    I'd perform the following steps:
  
       1. Run `dput(mtcars)` in R
       2. Copy the output
       3. In my reproducible script, type `mtcars <- ` then paste.
       
    But even better is if you can create a `data.frame()` with just a handful
    of rows and columns that still illustrates the problem.
  
1.  Spend a little bit of time ensuring that your **code** is easy for others to
    read:
  
    * make sure you've used spaces and your variable names are concise, but
      informative
  
    * use comments to indicate where your problem lies
  
    * do your best to remove everything that is not related to the problem.  
     The shorter your code is, the easier it is to understand.

You can check you have actually made a reproducible example by starting up a 
fresh R session and pasting your script in.

(Unless you've been specifically asked for it, please don't include the output 
of `sessionInfo()`.)

## Pull requests

To contribute a change to ggplot2, you follow these steps:

1. Create a branch in git and make your changes.
1. Push branch to github and issue pull request (PR).
1. Discuss the pull request.
1. Iterate until either we accept the PR or decide that it's not
   a good fit for ggplot2.

Each of these steps are described in more detail below. This might feel 
overwhelming the first time you get set up, but it gets easier with practice. 
If you get stuck at any point, please reach out for help on the [ggplot2-dev](https://groups.google.com/forum/#!forum/ggplot2-dev) mailing list.

If you're not familiar with git or github, please start by reading <http://r-pkgs.had.co.nz/git.html>

<!--
* [ ] Motivate the change in one paragraph, and include it in NEWS.
      In parentheses, reference your github user name and this issue:
      `(@hadley, #1234)`
* [ ] Check pull request only includes relevant changes.
* [ ] Use the [official style](http://adv-r.had.co.nz/Style.html).
* [ ] Update documentation and re-run roxygen2
* [ ] Add test, if bug in non-graphical function
* [ ] Add visual test, if bug in graphical function
* [ ] Add minimal example, if new graphical feature

See http://docs.ggplot2.org/dev/vignettes/development.html for more details.
--->

Pull requests will be evaluated against a seven point checklist:

1.  __Motivation__. Your pull request should clearly and concisely motivate the
    need for change. Unfortunately neither Winston nor I have much time to
    work on ggplot2 these days, so you need to describe the problem and show
    how your pull request solves it as concisely as possible.

    Also include this motivation in `NEWS` so that when a new release of
    ggplot2 comes out it's easy for users to see what's changed. Add your
    item at the top of the file and use markdown for formatting. The
    news item should end with `(@yourGithubUsername, #the_issue_number)`.

1.  __Only related changes__. Before you submit your pull request, please
    check to make sure that you haven't accidentally included any unrelated
    changes. These make it harder to see exactly what's changed, and to
    evaluate any unexpected side effects.

    Each PR corresponds to a git branch, so if you expect to submit
    multiple changes make sure to create multiple branches. If you have
    multiple changes that depend on each other, start with the first one
    and don't submit any others until the first one has been processed.

1.  __Use ggplot2 coding style__. Please follow the
    [official tidyverse style](http://style.tidyverse.org). Maintaining
    a consistent style across the whole code base makes it much easier to
    jump into the code. If you're modifying existing ggplot2 code that
    doesn't follow the style guide, a separate pull request to fix the
    style would be greatly appreciated.

1.  If you're adding new parameters or a new function, you'll also need
    to document them with [roxygen](https://github.com/klutometis/roxygen).
    Make sure to re-run `devtools::document()` on the code before submitting.

    Currently, ggplot2 uses the development version of roxygen2, which you
    can get with `install_github("klutometis/roxygen")`. This will be
    available on CRAN in the near future.

1.  If fixing a bug or adding a new feature to a non-graphical function,
    please add a [testthat](https://github.com/r-lib/testthat) unit test.

1.  If fixing a bug in the visual output, please add a visual test.
    (Instructions to follow soon)

1.  If you're adding a new graphical feature, please add a short example
    to the appropriate function.

This seems like a lot of work but don't worry if your pull request isn't perfect.
It's a learning process and members of the ggplot2 team will be on hand to help you
out. A pull request ("PR") is a process, and unless you've submitted a few in the
past it's unlikely that your pull request will be accepted as is. All PRs require
review and approval from at least one member of the ggplot2 development team 
before merge.

Finally, remember that ggplot2 is a mature package used by thousands of people. 
This means that it's extremely difficult (i.e. impossible) to change any existing
functionality without breaking someone's code (or another package on CRAN). 
Please don't submit pull requests that change existing behaviour. Instead, 
think about how you can add a new feature in a minimally invasive way.
