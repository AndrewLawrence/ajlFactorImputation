# setup
dat <- data.frame(y = rnorm(20),
                  x0 = rnorm(20),
                  x1 = rep(1, 20))
dat[2, 1] <- NA
dat[3, 2] <- NA
dat$x2 <- dat$y
dat$x3 <- rep(NA, 20)

test_that("factor imputation check", {
  capture.output(
    suppressMessages(
      suppressWarnings(
        expect_error(
          factor_imputation(dat[, c("y", "x0", "x3")])
        )
      )
    ),
    file = nullfile()
  )
})

test_that("mice check:", {
  # A constant:
  capture.output(
    expect_true(
      suppressMessages(
        suppressWarnings(
          sensecheck_dryrunmice(FALSE,
                                fami_options(),
                                dat[, c("y", "x0", "x1")])
        )
      )
    ),
    file = nullfile()
  )

  # perfect colinearity:
  capture.output(
    expect_true(
      suppressMessages(
        suppressWarnings(
          sensecheck_dryrunmice(FALSE,
                                fami_options(),
                                dat[, c("y", "x0", "x2")])
        )
      )
    ),
    file = nullfile()
  )

})
