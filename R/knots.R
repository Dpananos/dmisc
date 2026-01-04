#' Select knot positions for natural cubic splines
#'
#' This function selects knot positions for use with natural cubic splines
#' (e.g., splines::ns()) using recommended quantile-based strategies. By default,
#' it implements Frank Harrell's approach which selects both the number and
#' positions of knots based on sample size to balance flexibility and stability.
#'
#' @param x A numeric vector of data values. NA values are automatically removed.
#' @param strategy Character string specifying the knot placement strategy.
#'   Currently only 'harrell' is supported.
#' @param n_knots Optional integer to manually override the number of knots.
#'   If NULL (default), the number is determined by Harrell's sample-size-based
#'   rules. Must be at least 1 and should typically not exceed 10.
#' @return A numeric vector of knot positions (quantile values from x), sorted
#'   in ascending order.
#' @details
#' The 'harrell' strategy implements Frank Harrell's recommendations for knot
#' selection, which place knots at specific quantiles of the data:
#'
#' \itemize{
#'   \item n < 100: 3 knots at quantiles 0.10, 0.50, 0.90
#'   \item 100 <= n <= 200: 4 knots at quantiles 0.05, 0.35, 0.65, 0.95
#'   \item n > 200: 5 knots at quantiles 0.05, 0.275, 0.50, 0.725, 0.95
#' }
#'
#' where n is the number of non-NA values in x. This approach avoids overfitting
#' by using relatively few knots while placing them at informative locations
#' in the distribution.
#'
#' When n_knots is specified, it overrides the automatic selection and uses
#' evenly-spaced quantiles from 0 to 1 (excluding endpoints).
#'
#' @references
#' Harrell, F. E. (2015). Regression Modeling Strategies: With Applications to
#' Linear Models, Logistic and Ordinal Regression, and Survival Analysis
#' (2nd ed.). Springer.
#'
#' @examples
#' # Small sample: returns 3 knots
#' set.seed(123)
#' x_small <- rnorm(50)
#' knots(x_small)
#'
#' # Medium sample: returns 4 knots
#' x_medium <- rnorm(150)
#' knots(x_medium)
#'
#' # Large sample: returns 5 knots
#' x_large <- rnorm(300)
#' knots(x_large)
#'
#' # Manual override
#' knots(x_small, n_knots = 4)
#'
#' # Use with splines::ns()
#' library(splines)
#' age <- runif(200, 18, 80)
#' age_knots <- knots(age)
#' age_spline <- ns(age, knots = age_knots)
#'
#' @export
knots <- function(x, strategy = 'harrell', n_knots = NULL) {
  # 1. Validate inputs
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector.")
  }

  strategy <- match.arg(strategy, c("harrell"))

  if (!is.null(n_knots)) {
    if (!is.numeric(n_knots) || length(n_knots) != 1 || n_knots != round(n_knots)) {
      stop("'n_knots' must be a single integer value.")
    }
    stopifnot("'n_knots' must be at least 1" = n_knots >= 1)
    if (n_knots > 10) {
      warning("Using more than 10 knots is not typically recommended and may lead to overfitting.")
    }
  }

  # 2. Remove NAs and check data sufficiency
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    stop("'x' contains only NA values.")
  }

  if (length(x) < 3) {
    stop("'x' must have at least 3 non-NA values.")
  }

  if (length(unique(x)) == 1) {
    stop("'x' must contain more than one unique value.")
  }

  if (!is.null(n_knots) && n_knots > length(unique(x))) {
    stop("'n_knots' cannot exceed the number of unique values in 'x'.")
  }

  # 3. Determine number of knots (Harrell strategy)
  if (strategy == "harrell") {
    n <- length(x)

    # Use manual override if provided, otherwise use Harrell's rules
    if (!is.null(n_knots)) {
      num_knots <- n_knots
    } else {
      if (n < 100) {
        num_knots <- 3
      } else if (n <= 200) {
        num_knots <- 4
      } else {
        num_knots <- 5
      }
    }

    # 4. Select quantile positions
    if (!is.null(n_knots)) {
      # For manual override, use evenly-spaced quantiles
      quantile_positions <- seq(0, 1, length.out = num_knots + 2)[-c(1, num_knots + 2)]
    } else {
      # Use Harrell's recommended quantile positions
      quantile_positions <- switch(
        as.character(num_knots),
        "3" = c(0.10, 0.50, 0.90),
        "4" = c(0.05, 0.35, 0.65, 0.95),
        "5" = c(0.05, 0.275, 0.50, 0.725, 0.95),
        stop("Unexpected number of knots: ", num_knots)
      )
    }

    # 5. Compute and return knot values
    knot_values <- stats::quantile(x, probs = quantile_positions, na.rm = TRUE)
    knot_values <- base::unname(knot_values)

    return(knot_values)
  }
}
