This is a patch release fixing a range of regressions reported after the 4.0.0
release. While no breaking changes in the API we did detect a few packages that
breaks with this release due to either making assumptions about ggplot2 internal
behaviour, or requiring documentation updates that can only happen after release
of 4.0.1

The packages are:
- adproplus: https://github.com/henry-heppe/adproclus/issues/3
- ggsurveillance: https://github.com/biostats-dev/ggsurveillance/issues/1
- ggformula: https://github.com/ProjectMOSAIC/ggformula/issues/187
- ggside: https://github.com/jtlandis/ggside/issues/71

They have all been notified in advance of this submission
