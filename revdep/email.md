Hi,

This is an automated email to let you know that I have submitted {{{ my_package }}} to cran today  {{{ date }}}.  To check for potential problems, I ran `R CMD check` on your package {{{your_package}}} ({{{your_version}}}). 

I found: {{{your_summary}}}.

{{#you_have_problems}}
{{{your_results}}}

If I got an ERROR because I couldn't install your package (or one of it's dependencies), my apologies. Unfortunately I don't have the time to diagnose installation failures so you'll need to run the check yourself.

Otherwise, please determine if this is a problem with my code or your code.  If you think it's a problem with {{ my_package}}, please file an issue on github that attempts to recreate the problem with as little code as possible. If it's a problem with your code, please prepare an updated package for CRAN as soon as possible. The ERRORs and WARNINGs might not be caused by the update to {{{my_package}}}, but it really makes life easier (for both me and CRAN) if you also fix any other problems that may have accumulated over time. Please also try to minimise the NOTEs. It's not essential you do this, but the fewer the false positives the more likely we are to detect a real problem with your package. 

To get the development version of {{{ my_package }}} so you can run the checks yourself, you can run:

    # install.packages("devtools")
    devtools::install_github("{{my_github}}")
    
To see what's changed visit <https://blog.rstudio.org/2016/09/30/ggplot2-2-2-0-coming-soon/>

{{/you_have_problems}}
{{^you_have_problems}}
It looks like everything is ok, so you don't need to take any action, but you might want to read the NEWS, <https://github.com/{{{my_github}}}/blob/master/NEWS.md>, to see what's changed.
{{/you_have_problems}}

If you have any questions about this email, please feel free to respond directly.

Regards,

{{{ me }}}
