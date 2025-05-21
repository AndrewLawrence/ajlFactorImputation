\dontrun{

  # This is a long form example showing the difference between factor-multiple-
  #   imputation and complete-cases/mean imputation in a setting where we
  #   know the right answer because we simulated missing data with a known
  #   pattern.

  # load the bfi 5-factor personality example from the psych package
  data("bfi", package = "psych")

  # limit to n=2236 complete cases:
  bfi_complete <- bfi[complete.cases(bfi), ]
  bfi_complete$gender <- factor(bfi_complete$gender,
                                levels = 1:2,
                                labels = c("Male", "Female"))
  # fix reverse coding:
  for (i in c("A1", "C4", "C5", "E1", "E2", "O2", "O5")) {
    bfi_complete[[i]] <- 7 - bfi_complete[[i]]
  }


  # run factor analysis on the full dataset:
  fa_complete <- psych::fa(bfi_complete[, 1:25],
                           nfactors = 5,
                           fm = "pa",
                           rotate = "oblimin")

  # inspect the factor loadings:
  print(fa_complete$loadings, cutoff = 0.4)

  # remove N1 scores for male subjects with >average values
  #   for PA2 (neuroticism):
  removers <- which((fa_complete$scores[, "PA2"] >
                       mean(fa_complete$scores[, "PA2"])) &
                      bfi_complete$gender == "Male")

  bfi_missing <- bfi_complete
  bfi_missing$N1[removers] <- NA

  # re-run the factor analysis with missing data:
  fa_missing1 <- psych::fa(bfi_missing[, 1:25],
                           nfactors = 5,
                           fm = "pa",
                           rotate = "oblimin")

  # and using mean imputation:
  fa_missing2 <- psych::fa(bfi_missing[, 1:25],
                           nfactors = 5,
                           fm = "pa",
                           missing = TRUE,
                           impute = "mean",
                           rotate = "oblimin")

  # We hope that factor imputation will be more effective.
  # first set-up some options. We will use the default settings
  # Indeed, the settings above were chosen to match the ajlFactorImputation
  #   defaults settings for consistency.
  fi_options <- fami_options(fa_type = "fa",
                             nfactors = 5L)

  # This function does the work.
  #   We want to use demographic factors as auxiliary variables, but all
  #   other variables in the data.frame will be in the factor analysis, so
  #   we don't need to specify fa_vars.
  fi_res <- factor_imputation(
    bfi_missing,
    av_vars = c("gender", "education", "age"),
    options = fi_options
  )

  # Inspect loading matrices:
  print(fa_complete$loadings, cutoff = 0.4)
  print(fi_res$fa$loadings, cutoff = 0.4)

  # Consider a specific question:
  #   is there a gender difference in PA2: neuroticism?:

  # first assemble datasets with the PA2 factor scores and gender
  #   from different methods:
  gender_neuroticism_data <- list(
    complete = cbind(
      psych::factor.scores(bfi_complete[, 1:25],
                           f = fa_complete)$scores,
      bfi_complete
    ),
    missing1 = cbind(
      PA2 = psych::factor.scores(bfi_missing[, 1:25],
                                 f = fa_missing1)$scores[, 1],
      bfi_missing
    ),
    missing2 = cbind(
      PA2 = psych::factor.scores(
        bfi_missing[, 1:25],
        f = fa_missing2,
        missing = TRUE,
        impute = "mean"
      )$scores[, 1],
      bfi_missing
    )
  )

  # fit the same model to each:
  gender_neuroticism_models <- lapply(gender_neuroticism_data,
         \(x) lm(PA2 ~ gender, data = x))

  # extract coefficients:
  results_table <- sapply(gender_neuroticism_models, coef)

  # Multiply imputed data needs to be handled slightly differently:
  #   run the model for each imputed dataset:
  gender_neuroticism_imputed <- with(get_mids(fi_res),
                                     lm( PA2 ~ gender ))
  #   then pool the results:
  gender_neuroticism_pooled <- mice::pool(gender_neuroticism_imputed)

  # add the results in:
  results_table <- cbind(
    results_table,
    factor_imputation = summary(gender_neuroticism_pooled)$estimate
  )

  # Inspect the coeffients:
  results_table
  #                complete   missing1   missing2 factor_imputation
  # (Intercept)  -0.1241459 -0.7744490 -0.2638178        -0.2373717
  # genderFemale  0.1849368  0.8962288  0.3930024         0.3392518

  # or look at root mean square deviation:
  sqrt(colSums((results_table[,2:4] - results_table[,1])^2) / 2)
  #          missing1          missing2 factor_imputation
  #         0.6814802         0.1771998         0.1353388

  # Unsurprisingly given the missingness mechanism applied,
  #   all methods estimated the PA2 neuroticism factor to be
  #   higher for females than males than the ground truth.
  #
  # However, the smallest deviations from the "ground truth" results
  #   was obtained with factor imputation.
  #   (mean imputation didn't do too badly though!)
}
