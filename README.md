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

# Export for analysis

The multiply imputed data can be exported for use in SPSS via the mice function 
`mice::mids2spss()` which uses `haven::write_sav()`.

There is a similar function for mplus (mids2mplus).

```r

# see:
?mice::mids2spss

mice::mids2spss(
  imp = get_mids(my_factor_imputation_object),
  filename = "my_spss_imputed_dataset"
)

```

## Analysis in SPSS overview

In SPSS multiply imputed data can be analysed by including a named variable
`Imputation_` which contains an integer indicating the imputed set the 
observations come from. mids2spss will do this for you.

To correctly analyse multiply imputed data in SPSS you first need to:

 1) "Split" the file by imputation
 2) Exclude any `Imputation_ = 0` observations from the analysis - these are 
     the "original" data (with missing values).

To split the file:

`Data > Split File > Compare Groups > Groups Based on > [ Imputation_ ] > OK`

To exclude `Imputation = 0`:

`Data > Select Cases > If Condition is satisfied > [ Imputation_ > 0 ] > Continue`

Then run analysis as normal. SPSS output will be presented for each imputation,
with the pooled results appended at the end (for those outputs that SPSS has 
implemented pooling for)

*Warning: if the above steps are not done correctly you can get wrong - harmfully 
liberal - results. To ensure everything is working as expected make sure to
verify SPSS is reporting the correct degrees of freedom for your original 
sample size, and also check there is output for the correct number of 
imputations. For example a paired t-test for 30 observations with 5 imputations
should show a df of ($n-2 = 30 - 2 = 28$) with 5 imputation results and a pooled
result. If it showed a df of 148 or showed 6 imputation outputs and the pooled
output then something went wrong.*
