test_that("checking works:", {
  # basic run:
  expect_no_error(check_varlists("test", NULL, NULL, "test"))
  # zero length:
  expect_error(
    check_varlists(vector(mode = "character", length = 0L), NULL, NULL, "test")
  )
  # collision:
  expect_error(check_varlists("test", "test", NULL, "test"))
  # not in column names:
  expect_error(check_varlists("missing", NULL, NULL, "test"))
  expect_error(check_varlists(NULL, "missing", NULL, "test"))
  expect_error(check_varlists(NULL, NULL, "missing", "test"))
})

test_that("parsing works:", {
  # basic run:
  expect_equal(
    parse_varlists("test", NULL, NULL, "test"),
    list(
      fa = "test",
      av = character(0),
      other = character(0),
      excluded = character(0)
    )
  )
  # unlisted columns in data are excluded if all sets specified:
  expect_true("d" %in% parse_varlists("a", "b", "c", letters[1:4])$excluded)
  # ...and otherwise included in the first unspecified set:
  expect_true("d" %in% parse_varlists("a", "b", NULL, letters[1:4])$other)
  expect_true("d" %in% parse_varlists("a", NULL, NULL, letters[1:4])$av)
  expect_true("d" %in% parse_varlists(NULL, NULL, "c", letters[1:4])$fa)
})
