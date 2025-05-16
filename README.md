---
editor_options: 
  markdown: 
    wrap: 72
---

# ajlFactorImputation

[ THIS PACKAGE IS IN DEVELOPMENT ]

`ajlFactorImputation` is an R package to run exploratory factor analysis
with multiple imputation for missing data values.

`ajlFactorImputation` does not set out to do anything complicated or
novel, rather it aims to streamline the process for the analyst -
providing an easier-to-use interface to existing packages.

The key package employed is called
[`mifa`](https://github.com/teebusch/mifa) which itself calls the
[`mice`](https://amices.org/mice/) (multivariate imputation by chained
equations) package for imputation. The
[`psych`](https://personality-project.org/r/psych/) package is used for
factor analysis.

`ajlFactorImputation` will try to:

1)  Reduce decision-making about algorithm settings by selecting
    "sensible" default settings
2)  Check options and input data format
3)  Run imputation and factor analysis
4)  Output resulting imputed data objects for subsequent analysis

Necessarily "sensible defaults" are neither objective nor universal.
They have been selected because they work well for the sort of data the
users of this package typically work with. They may not be sensible
defaults for your application.

## Installation

You can install ajlFactorImputation like so:

``` r
remotes::install_github("AndrewLawrence/ajlFactorImputation")
```

## Example

TBC.
