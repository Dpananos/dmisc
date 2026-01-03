# Test setup - create reusable test fixtures
setup_test_data <- function() {
  set.seed(123)
  # Create simple test data
  n <- 100
  data <- data.frame(
    time = rexp(n, rate = 0.01),
    status = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
    age = rnorm(n, mean = 60, sd = 10),
    sex = sample(0:1, n, replace = TRUE)
  )

  model <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = data)

  list(data = data, model = model)
}

# BASIC FUNCTIONALITY TESTS
test_that("adjusted_survival_curves works with single-row grid", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("time", "estimate", "conf_low", "conf_high") %in% colnames(result)))
  expect_true("age" %in% colnames(result))
  expect_true("sex" %in% colnames(result))
})

test_that("adjusted_survival_curves works with multi-row grid", {
  test_setup <- setup_test_data()
  grid <- expand.grid(age = c(50, 60, 70), sex = c(0, 1))

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > nrow(grid))  # Multiple times per grid row
})

test_that("adjusted_survival_curves works with specified times", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)
  times <- c(10, 20, 30)

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model, times = times)

  expect_equal(nrow(result), length(times))
  expect_true(all(result$time %in% times))
})

test_that("adjusted_survival_curves preserves grid columns", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = c(50, 70), sex = c(0, 1), id = c("A", "B"))

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model)

  expect_true("id" %in% colnames(result))
  expect_true(all(c("A", "B") %in% result$id))
})

# OUTPUT STRUCTURE TESTS
test_that("output has correct column names", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model)

  expected_cols <- c("age", "sex", "time", "n_risk", "n_event",
                     "estimate", "std_error", "conf_low", "conf_high")
  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("confidence intervals are properly ordered", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model)

  expect_true(all(result$conf_low <= result$estimate))
  expect_true(all(result$estimate <= result$conf_high))
})

test_that("survival estimates are between 0 and 1", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model)

  expect_true(all(result$estimate >= 0 & result$estimate <= 1))
})

# INPUT VALIDATION TESTS
test_that("errors on non-coxph model", {
  grid <- data.frame(age = 60, sex = 1)
  bad_model <- lm(mpg ~ wt, data = mtcars)

  expect_error(
    adjusted_survival_curves(grid = grid, model = bad_model),
    "must be a coxph object"
  )
})

test_that("errors on non-dataframe grid", {
  test_setup <- setup_test_data()
  bad_grid <- c(age = 60, sex = 1)

  expect_error(
    adjusted_survival_curves(grid = bad_grid, model = test_setup$model),
    "must be a data frame"
  )
})

test_that("errors on empty grid", {
  test_setup <- setup_test_data()
  empty_grid <- data.frame(age = numeric(0), sex = numeric(0))

  expect_error(
    adjusted_survival_curves(grid = empty_grid, model = test_setup$model),
    "cannot be empty"
  )
})

test_that("errors on negative times", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  expect_error(
    adjusted_survival_curves(grid = grid, model = test_setup$model, times = c(-1, 10)),
    "must be positive"
  )
})

test_that("errors on zero times", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  expect_error(
    adjusted_survival_curves(grid = grid, model = test_setup$model, times = c(0, 10)),
    "must be positive"
  )
})

test_that("errors on empty times vector", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  expect_error(
    adjusted_survival_curves(grid = grid, model = test_setup$model, times = numeric(0)),
    "cannot be an empty vector"
  )
})

test_that("accepts NULL times (default behavior)", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  expect_no_error(
    adjusted_survival_curves(grid = grid, model = test_setup$model, times = NULL)
  )
})

test_that("errors on missing required covariates", {
  test_setup <- setup_test_data()
  grid_missing <- data.frame(age = 60)  # Missing 'sex'

  expect_error(
    adjusted_survival_curves(grid = grid_missing, model = test_setup$model),
    "missing required model covariates.*sex"
  )
})

test_that("works with extra columns in grid", {
  test_setup <- setup_test_data()
  grid_extra <- data.frame(age = 60, sex = 1, extra = "info")

  result <- adjusted_survival_curves(grid = grid_extra, model = test_setup$model)

  expect_true("extra" %in% colnames(result))
  expect_equal(unique(result$extra), "info")
})

# FORMULA COMPLEXITY TESTS - Test various formula constructs
test_that("works with natural spline basis functions", {
  test_setup <- setup_test_data()
  age_knots <- quantile(test_setup$data$age, c(0.10, 0.50, 0.90))

  model_spline <- survival::coxph(
    survival::Surv(time, status) ~ splines::ns(age, knots = age_knots) + sex,
    data = test_setup$data
  )

  grid <- data.frame(age = c(50, 60, 70), sex = 1)
  result <- adjusted_survival_curves(grid = grid, model = model_spline)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("age", "sex") %in% colnames(result)))
})

test_that("works with B-spline basis functions", {
  test_setup <- setup_test_data()
  age_knots <- quantile(test_setup$data$age, c(0.25, 0.50, 0.75))

  model_bs <- survival::coxph(
    survival::Surv(time, status) ~ splines::bs(age, knots = age_knots) + sex,
    data = test_setup$data
  )

  grid <- data.frame(age = 60, sex = 1)
  result <- adjusted_survival_curves(grid = grid, model = model_bs)

  expect_s3_class(result, "data.frame")
})

test_that("works with polynomial transformations", {
  test_setup <- setup_test_data()

  model_poly <- survival::coxph(
    survival::Surv(time, status) ~ poly(age, 2) + sex,
    data = test_setup$data
  )

  grid <- data.frame(age = 60, sex = 1)
  result <- adjusted_survival_curves(grid = grid, model = model_poly)

  expect_s3_class(result, "data.frame")
})

test_that("works with I() transformations", {
  test_setup <- setup_test_data()

  model_trans <- survival::coxph(
    survival::Surv(time, status) ~ I(age^2) + I(age * sex) + sex,
    data = test_setup$data
  )

  grid <- data.frame(age = 60, sex = 1)
  result <- adjusted_survival_curves(grid = grid, model = model_trans)

  expect_s3_class(result, "data.frame")
})

test_that("works with stratified models", {
  test_setup <- setup_test_data()

  model_strat <- survival::coxph(
    survival::Surv(time, status) ~ age + strata(sex),
    data = test_setup$data
  )

  grid <- data.frame(age = c(50, 70), sex = c(0, 1))
  result <- adjusted_survival_curves(grid = grid, model = model_strat)

  expect_s3_class(result, "data.frame")
})

test_that("works with interaction terms", {
  test_setup <- setup_test_data()

  model_interact <- survival::coxph(
    survival::Surv(time, status) ~ age * sex,
    data = test_setup$data
  )

  grid <- expand.grid(age = c(50, 70), sex = c(0, 1))
  result <- adjusted_survival_curves(grid = grid, model = model_interact)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(grid), 4)
})

test_that("still errors when actual covariate missing with splines", {
  test_setup <- setup_test_data()
  age_knots <- quantile(test_setup$data$age, c(0.10, 0.50, 0.90))

  model_spline <- survival::coxph(
    survival::Surv(time, status) ~ splines::ns(age, knots = age_knots) + sex,
    data = test_setup$data
  )

  grid_missing <- data.frame(age = 60)  # Missing sex

  expect_error(
    adjusted_survival_curves(grid = grid_missing, model = model_spline),
    "missing required model covariates.*sex"
  )
})

# EDGE CASE TESTS
test_that("handles grid with one row correctly", {
  test_setup <- setup_test_data()
  grid <- data.frame(age = 60, sex = 1)

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model)

  expect_equal(unique(result$age), 60)
  expect_equal(unique(result$sex), 1)
})

test_that("handles many grid rows efficiently", {
  test_setup <- setup_test_data()
  grid <- expand.grid(
    age = seq(40, 80, by = 5),
    sex = c(0, 1)
  )

  result <- adjusted_survival_curves(grid = grid, model = test_setup$model)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})
