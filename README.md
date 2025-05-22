# ajlFactorImputation

[ THIS PACKAGE IS IN DEVELOPMENT ]

`ajlFactorImputation` is an R package to run exploratory factor analysis with multiple imputation for missing data values.

`ajlFactorImputation` does not set out to do anything complicated or novel, rather it aims to streamline the process for the analyst - providing an easier-to-use interface to existing packages.

The key package employed is called [`mifa`](https://github.com/teebusch/mifa) which itself calls the [`mice`](https://amices.org/mice/) (multivariate imputation by chained equations) package for imputation. The [`psych`](https://personality-project.org/r/psych/) package is used for factor analysis.

`ajlFactorImputation` will try to:

1)  Reduce decision-making about algorithm settings by selecting "sensible" default settings
2)  Check options and input data format
3)  Run imputation and factor analysis
4)  Output resulting imputed data objects for subsequent analysis

Necessarily "sensible defaults" are neither objective nor universal.
They have been selected because they should work well for the sort of data the users of this package typically work with.
They may not be sensible defaults for your application.

## Installation

The package is not available on CRAN. To install from github use the 
remotes package like so:

``` r
# install.packages("remotes")
remotes::install_github("AndrewLawrence/ajlFactorImputation")
```

## Example

```r
library(ajlFactorImputation)

# View help for the main function:
?factor_imputation

# For this example, we work with personality data from the psych package
#   (an example of a 5-factor structure with missing values)
data("bfi", package = "psych")

# Convert gender to a factor:
bfi$gender <- factor(bfi$gender, levels = 1:2, labels = c("Male", "Female"))

# Run a factor imputation requesting a 5-factor solution
#   (5 factors known structure for this data), using the package defaults
#   for factor analysis (rather than pca).
#
# [ ! Note: this step is time-consuming ! ]
bfi_5f_result <- ajlFactorImputation::factor_imputation(
  data = bfi,
  av_vars = c("gender", "education", "age"),
  options = fami_options(fa_type = "fa", nfactors = 5L)
)

# Printing the result gives some basic information:
bfi_5f_result

# The package includes accessor functions to extract:
#   1. the data covariance matrix (pooled over imputations)
get_pooledcov(bfi_5f_result)
#   2. the factor loadings
get_loadings(bfi_5f_result)
#   3. the multiply imputed data set (mids)
#         (this includes factor scores for each imputation)
get_mids(bfi_5f_result)

# inspect the factor structure:
print(get_loadings(bfi_5f_result), cutoff = 0.35)

# extract the mids object to use in further analysis:
bfi_5f_mids <- get_mids(bfi_5f_result)

# fit a simple regression model to the data using imputed factor scores.
#   Specifically we will test the hypothesis that factor PA1
#   (which corresponds to Extraversion) does not differ between genders.
library(mice)
mi_models <- with(bfi_5f_mids, lm(PA1 ~ gender))

mi_pooled <- mice::pool(mi_models)

# The pooled analysis result:
summary(mi_pooled)
#           term    estimate  std.error statistic       df     p.value
# 1  (Intercept)  0.07378399 0.03295151  2.239169 2788.247 0.025223489
# 2 genderFemale -0.11008271 0.04019658 -2.738609 2790.070 0.006209165

# Great, we can reject the null hypothesis of no difference.

# We can compare to a complete-cases model:
cc_model <- lm(PA1 ~ gender, data = complete(bfi_5f_mids, 0L))
# There are only 2436 observations (rather than 2800)

broom::tidy(cc_model)
# # A tibble: 2 x 5
#   term         estimate std.error statistic p.value
#   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
# 1 (Intercept)    0.0971    0.0354      2.74 0.00619
# 2 genderFemale  -0.118     0.0433     -2.73 0.00645

# In this example the results are very similar.
#   Coefficient standard errors are reduced by multiple imputation,
#   but the effect estimate for gender is also marginally smaller, so the
#   key t-statistics are comparable.

```
