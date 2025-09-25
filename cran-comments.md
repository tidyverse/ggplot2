This is a major release of ggplot2. Among other things it includes an adaption
of S7 for all the S3 classes and methods that were currently in use.
Unfortunately such a change is not without issue with the number of reverse
dependencies that ggplot2 has as many packages wrongfully checks the internals
of ggplot2 objects in their tests.

Because of this you should expect a larger than usual number of breaking
packages. We have been very diligent to reach out to all maintainers over the
last 2-3 months and provided resolutions but breakages are still to be expected.
