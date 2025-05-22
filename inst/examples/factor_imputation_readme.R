library(ajlFactorImputation)

# View help for main function
?factor_imputation

# load raw five factor personality data
#   (example of a factor structure with missing values)
data("bfi", package = "psych")

# convert gender to a factor:
bfi$gender <- factor(bfi$gender, levels = 1:2, labels = c("Male", "Female"))

# run factor imputation requesting a 5-factor solution
#   (5 factors known structure for this data), using package default
#   PAF factor analysis options
#
# [ ! Note: this step is time-consuming ! ]
bfi_5f_result <- ajlFactorImputation::factor_imputation(
  data = bfi,
  av_vars = c("gender", "education", "age"),
  options = fami_options(fa_type = "fa", nfactors = 5L)
)

# inspect the factor structure:
print(get_loadings(bfi_5f_result), cutoff = 0.35)

# extract a mids object to use in further analysis:
bfi_5f_mids <- get_mids(bfi_5f_result)

# fit a simple regression model to the data using imputed factor scores.
#   Specifically, test the hypothesis that factor PA1
#   (which corresponds to Extraversion) does not differ between genders.
library(mice)
mi_models <- with(bfi_5f_mids, lm(PA1 ~ gender))

mi_pooled <- mice::pool(mi_models)

# The pooled analysis result:
summary(mi_pooled)
#           term    estimate  std.error statistic       df     p.value
# 1  (Intercept)  0.07331121 0.03297476  2.223252 2783.833 0.026278810
# 2 genderFemale -0.10969545 0.04024731 -2.725535 2777.805 0.006459998


# Great, we can reject the null hypothesis of no difference.

# Note that above we specified the number of factors according to
#   theory (the "big5" personality structure). If instead we didn't
#   know the ideal number of factors for the data we could have
#   the package estimate this.
#   Estimation by parallel analysis is the default behaviour of
#   the factor_imputation function.

# We can see what would happen by rerunning factor_imputation, but
#   without nfactors set, or with fami_options(nfactors = "parallel").

# However, this would re-impute the data.
#   In this case it will be quicker just to check if this *would* have selected
#   the appropriate number of factors by applying an equivalent parallel
#   analysis to the pooled multiply-imputed covariance matrix
#   (which has already been calculated).
get_pooledcov(bfi_5f_result)

set.seed(42)
psych::fa.parallel(get_pooledcov(bfi_5f_result),
                   n.obs = nrow(bfi),
                   fm = "pa", fa = "fa",
                   n.iter = 100,
                   sim = TRUE,
                   error.bars = TRUE,
                   quant = 0.99,
                   se.bars = TRUE)

# Close. It identified 6.

# Velicer's Minimum Average Partial method identifies 5 though
which.min(psych::nfactors(
  get_pooledcov(bfi_5f_result),
  n = 10,
  n.obs = nrow(bfi),
  fm = "pa",
  rotate = "oblimin"
)$map)
# [1] 5
