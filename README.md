# ajlFactorImputation

[ THIS PACKAGE IS IN EARLY STAGES OF DEVELOPMENT ]

`ajlFactorImputation` is an R package to run exploratory factor analysis with multiple imputation for missing data values.

`ajlFactorImputation` does not implement anything complicated or novel. It aims to streamline the process of conducting a particular analysis - providing an easier-to-use interface to existing R packages.

The key package for Factor Imputation is called [`mifa`](https://github.com/teebusch/mifa) which itself calls the [`mice`](https://amices.org/mice/) (multivariate imputation by chained equations) package for imputation. The [`psych`](https://personality-project.org/r/psych/) package is then used for factor analysis of the results and generation of multiply imputed factor scores.

## Factor Imputation

The basic steps run by the core package function `factor_imputation()` are:

1)  Run [`mice`](https://amices.org/mice/) multiple imputation on the variables in the factor analysis, using additional auxiliary variables to help improve the imputation.
2)  Extract a single pooled covariance matrix of the factor analysis variables, pooled over the multiple imputations.
3)  Run factor analysis on the pooled covariance matrix, extracting either:
    a)  A pre-specified number of factors, -or-
    b)  A number of factors determined automatically by a particular method (currently: parallel analysis).
4)  Construct factor scores for each imputed dataset using the pooled factor structure.

The key result is a mice ["mids"](https://amices.org/mice/reference/mids.html) object containing both the original multiply imputed data from the mice function, *and* the calculated multiply imputed factor scores. This mids object can be subjected to further analysis.

## Package rationale

The purpose of the `ajlFactorImputation` package is to bring together the above steps into a standardised package of code. In the process it aims to:

1)  Make the process easy for the user
2)  Reduce decision-making about algorithm settings by selecting "sensible" default settings
3)  Reduce the chance for user errors by checking options and input data format

It might not do any of the above successfully. Especially, I think "sensible defaults" cannot be objective nor universal - they may not be sensible defaults for your application.

## Installation

The package is not available on CRAN. To install from github use the remotes package:

``` r
# install.packages("remotes")
remotes::install_github("AndrewLawrence/ajlFactorImputation", dependencies = TRUE)
```

## Example

``` r
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
set.seed(42L)
bfi_5f_result <- ajlFactorImputation::factor_imputation(
  data = bfi,
  av_vars = c("gender", "education", "age"),
  options = fami_options(fa_type = "fa", nfactors = 5L)
)

# Printing the result gives some basic information:
bfi_5f_result
# An object of class ajlfacimp.
# ℹ type: fa
# ℹ # factors/comps: 5
# ℹ # factor vars: 25
# ℹ # auxiliary vars: 3
# ℹ # other vars: 0
# ℹ # observations: 2800
# ℹ # imputed values: 425.166666666667

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
# 1  (Intercept)  0.03536537 0.01447483  2.443231 2790.535 0.014617989
# 2 genderFemale -0.05267827 0.01766694 -2.981744 2786.181 0.002890949

# Great, we can reject the null hypothesis of no difference.

# We can compare to a complete-cases model:
cc_model <- lm(PA1 ~ gender, data = complete(bfi_5f_mids, 0L))
# There are only 2436 observations (rather than 2800)

broom::tidy(cc_model)
# # A tibble: 2 × 5
#   term         estimate std.error statistic p.value
#   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
# 1 (Intercept)    0.0488    0.0156      3.14 0.00172
# 2 genderFemale  -0.0582    0.0190     -3.06 0.00221

# In this example the results are very similar.
#   Coefficient standard errors are reduced by multiple imputation, due
#   to the increased sample size relative to complete cases, but the effect
#   estimate for gender is also marginally smaller, so the t-statistics
#   are comparable.
```

## Input Checks

Prior to factor analysis you will need to import your dataset into R, decide which variables are to be factored and which (if any) serve as auxiliary variables in the imputation. These variables need to be named and formatted appropriately, with any variance stabilising transformations applied. Errors can occur in in these steps, and this package will try to check for common issues.

The `factor_imputation()` function will, by default, run a number of checks on your input. This behaviour can be controlled with the argument: `run_checks = FALSE`. These "sense" checks aim to be overzealous and help spot common problems before the potentially time-consuming imputation starts.

If you are sure about your data and what you're doing then these checks can be turned off and won't run.

Briefly, the input checks look for:

-   Presence of commonly used missing value codes in the data (e.g. -999) - all missing data should be set to NA.

-   Sample size small for common factor analysis (PCA may be more reasonable)

-   Inclusion of non-numeric factors/character variables in the factor analysis

-   Auxiliary variables coded as factors with too many levels, or with rare factor levels.

-   Additional issues identified by mice software (does a minimal processing "dry-run" of mice)

## Subsequent Analysis of Multiply Imputed Data

As shown in the example above, multiply imputed data needs to be multiply analysed. A statistical model is fit to each imputed dataset and the multiple results from those models are pooled together using Rubin's Rules to give the multiply imputed result.

For background and more examples of workflows in R, see [Chapter 5 of Flexible Imputation of Missing Data](https://stefvanbuuren.name/fimd/ch-analysis.html) van Buuren (2018).

SPSS also provides some support for pooled analysis of multiply imputed data.

### SPSS

Results from `ajlFactorImputation` can be exported for use in SPSS via the mice function `mice::mids2spss()` which uses `haven::write_sav()`.

There is also a similar function for mplus (mids2mplus).

``` r

# see:
?mice::mids2spss

mice::mids2spss(
  imp = get_mids(my_factor_imputation_object),
  filename = "my_spss_imputed_dataset"
)
```

In SPSS multiply imputed data can be analysed by including a named variable `Imputation_` which contains an integer indicating the imputed set the observations come from. mids2spss will do this for you.

To correctly analyse multiply imputed data in SPSS you first need to:

1)  "Split" the file by imputation
2)  Exclude any `Imputation_ = 0` observations from the analysis - these are the "original" data (with missing values).

To split the file:

`Data > Split File > Compare Groups > Groups Based on > [ Imputation_ ] > OK`

To exclude `Imputation = 0`:

`Data > Select Cases > If Condition is satisfied > [ Imputation_ > 0 ] > Continue`

Then run analysis as normal. SPSS output will be presented for each imputation, with the pooled results appended at the end (for those outputs that SPSS has implemented pooling for)

*Warning: if the above steps are not done correctly you can get wrong - harmfully liberal - results. To ensure everything is working as expected make sure to verify SPSS is reporting the correct degrees of freedom for your original sample size, and also check there is output for the correct number of imputations. For example a paired t-test for 30 observations with 5 imputations should show a df of (*$n-2 = 30 - 2 = 28$) with 5 imputation results and a pooled result. If it showed a df of 148 or showed 6 imputation outputs and the pooled output then something went wrong.
