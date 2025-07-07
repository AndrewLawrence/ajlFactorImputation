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
# ℹ # factorscomps: 5
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
