# Extracted from test-adjusted-survival-curves.R:171

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "dmisc", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
test_setup <- setup_test_data()
grid_missing <- data.frame(age = 60)
expect_error(
    adjusted_survival_curves(grid = grid_missing, model = test_setup$model),
    "missing required model covariates.*sex"
  )
