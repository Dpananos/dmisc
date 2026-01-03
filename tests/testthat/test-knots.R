test_that("knots returns correct number for small samples (n < 100)", {
  set.seed(123)
  x_small <- rnorm(50)
  result <- knots(x_small)

  expect_equal(length(result), 3)
  expect_type(result, "double")
  expect_true(all(!is.na(result)))
})

test_that("knots returns correct quantiles for small samples", {
  set.seed(123)
  x_small <- rnorm(50)
  result <- knots(x_small)
  expected <- unname(quantile(x_small, c(0.10, 0.50, 0.90)))

  expect_equal(result, expected)
})

test_that("knots returns correct number for medium samples (100 <= n <= 200)", {
  set.seed(123)
  x_medium <- rnorm(150)
  result <- knots(x_medium)

  expect_equal(length(result), 4)
})

test_that("knots returns correct quantiles for medium samples", {
  set.seed(123)
  x_medium <- rnorm(150)
  result <- knots(x_medium)
  expected <- unname(quantile(x_medium, c(0.05, 0.35, 0.65, 0.95)))

  expect_equal(result, expected)
})

test_that("knots returns correct number for large samples (n > 200)", {
  set.seed(123)
  x_large <- rnorm(300)
  result <- knots(x_large)

  expect_equal(length(result), 5)
})

test_that("knots returns correct quantiles for large samples", {
  set.seed(123)
  x_large <- rnorm(300)
  result <- knots(x_large)
  expected <- unname(quantile(x_large, c(0.05, 0.275, 0.50, 0.725, 0.95)))

  expect_equal(result, expected)
})

test_that("knots returns unnamed numeric vector", {
  set.seed(123)
  x <- rnorm(100)
  result <- knots(x)

  expect_null(names(result))
  expect_type(result, "double")
})

test_that("knots returns sorted values", {
  set.seed(123)
  x <- rnorm(100)
  result <- knots(x)

  expect_equal(result, sort(result))
})

test_that("knots handles NA values correctly", {
  set.seed(123)
  x_with_na <- c(rnorm(50), NA, NA, NA)
  result <- knots(x_with_na)

  expect_equal(length(result), 3)
  expect_true(all(!is.na(result)))

  # Should give same result as without NAs
  x_without_na <- rnorm(50)
  set.seed(123)
  x_without_na <- rnorm(50)
  expected <- knots(x_without_na)

  set.seed(123)
  x_with_na <- c(rnorm(50), NA, NA, NA)
  result_with_na <- knots(x_with_na)

  expect_equal(result_with_na, expected)
})

test_that("knots errors on all NA values", {
  x_all_na <- c(NA_real_, NA_real_, NA_real_)

  expect_error(knots(x_all_na), "'x' contains only NA values.")
})

test_that("knots respects n_knots override", {
  set.seed(123)
  x <- rnorm(50)

  # Default would give 3 knots, override to 4
  result <- knots(x, n_knots = 4)

  expect_equal(length(result), 4)
})

test_that("n_knots override uses evenly-spaced quantiles", {
  set.seed(123)
  x <- rnorm(50)
  result <- knots(x, n_knots = 4)

  # For 4 knots, evenly spaced: seq(0, 1, length.out=6)[-c(1,6)]
  # = c(0.2, 0.4, 0.6, 0.8)
  expected <- unname(quantile(x, c(0.2, 0.4, 0.6, 0.8)))

  expect_equal(result, expected)
})

test_that("n_knots override works with different sample sizes", {
  set.seed(123)
  x_large <- rnorm(300)

  # Default would give 5 knots, override to 3
  result <- knots(x_large, n_knots = 3)

  expect_equal(length(result), 3)
})

test_that("knots errors on non-numeric input", {
  x_char <- c("a", "b", "c")

  expect_error(knots(x_char), "'x' must be a numeric vector.")
})

test_that("knots errors on invalid strategy", {
  x <- rnorm(50)

  expect_error(knots(x, strategy = "invalid"), "'arg' should be")
})

test_that("knots errors on invalid n_knots (non-integer)", {
  x <- rnorm(50)

  expect_error(knots(x, n_knots = 3.5), "'n_knots' must be a single integer value.")
})

test_that("knots errors on invalid n_knots (vector)", {
  x <- rnorm(50)

  expect_error(knots(x, n_knots = c(3, 4)), "'n_knots' must be a single integer value.")
})

test_that("knots errors on negative n_knots", {
  x <- rnorm(50)

  expect_error(knots(x, n_knots = -1), "'n_knots' must be at least 1")
})

test_that("knots errors on zero n_knots", {
  x <- rnorm(50)

  expect_error(knots(x, n_knots = 0), "'n_knots' must be at least 1")
})

test_that("knots warns on n_knots > 10", {
  x <- rnorm(200)

  expect_warning(knots(x, n_knots = 12), "more than 10 knots")
})

test_that("knots errors when x has fewer than 3 values", {
  x_small <- c(1, 2)

  expect_error(knots(x_small), "'x' must have at least 3 non-NA values.")
})

test_that("knots errors when x has only one unique value", {
  x_constant <- c(5, 5, 5, 5, 5)

  expect_error(knots(x_constant), "'x' must contain more than one unique value.")
})

test_that("knots errors when n_knots exceeds unique values", {
  x <- c(1, 2, 3, 3, 3)  # Only 3 unique values

  expect_error(knots(x, n_knots = 5), "'n_knots' cannot exceed the number of unique values in 'x'.")
})

test_that("knots works at boundary n=100", {
  set.seed(123)
  x_boundary <- rnorm(100)
  result <- knots(x_boundary)

  # At n=100, should use 4 knots (100 <= n <= 200)
  expect_equal(length(result), 4)
})

test_that("knots works at boundary n=200", {
  set.seed(123)
  x_boundary <- rnorm(200)
  result <- knots(x_boundary)

  # At n=200, should use 4 knots (100 <= n <= 200)
  expect_equal(length(result), 4)
})

test_that("knots works at boundary n=201", {
  set.seed(123)
  x_boundary <- rnorm(201)
  result <- knots(x_boundary)

  # At n=201, should use 5 knots (n > 200)
  expect_equal(length(result), 5)
})
