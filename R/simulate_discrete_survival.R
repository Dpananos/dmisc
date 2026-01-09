# Internal helper functions for discrete survival conversions
# These implement the mathematical relationships between hazard, survival, and PMF

#' Convert discrete hazard to PMF values
#'
#' @param h Numeric vector of hazard values h(1), h(2), ..., h(T).
#' @return Numeric vector of PMF values f(1), f(2), ..., f(T) where
#'   f(t) = P(T = t).
#' @noRd
hazard_to_pmf <- function(h) {
  # S(t) = prod_{k=1}^{t} (1 - h(k)), with S(0) = 1
  # f(t) = h(t) * S(t-1)
  S_prev <- c(1, cumprod(1 - h))  # S(0), S(1), ..., S(T)
  S_prev <- S_prev[-length(S_prev)]  # S(0), S(1), ..., S(T-1)
  h * S_prev
}

#' Convert discrete survival to PMF values
#'
#' @param S Numeric vector of survival values S(1), S(2), ..., S(T).
#'   Note: S(0) = 1 is assumed.
#' @return Numeric vector of PMF values f(1), f(2), ..., f(T) where
#'   f(t) = P(T = t).
#' @noRd
survival_to_pmf <- function(S) {
  # f(t) = S(t-1) - S(t)
  S_full <- c(1, S)  # S(0), S(1), ..., S(T)
  S_full[-length(S_full)] - S_full[-1]
}

#' Simulate Discrete Survival Data
#'
#' Generates discrete survival data given one of: the hazard, the
#' probability mass function (PMF), or the survival function as a numeric
#' vector. Time is assumed to be discrete integers (1, 2, 3, ...).
#'
#' @param n Integer. The number of observations to simulate.
#' @param hazard Optional numeric vector of length T. The discrete hazard
#'   h(t) for t = 1, ..., T, giving the probability of the event at time t
#'   given survival to time t. Values must be in [0, 1].
#' @param pmf Optional numeric vector of length T. The probability mass
#'   function f(t) for t = 1, ..., T, giving the probability of the event
#'   occurring exactly at time t. Must sum to <= 1.
#' @param survival Optional numeric vector of length T. The survival function
#'   S(t) for t = 1, ..., T, giving the probability of surviving past time t.
#'   Must be non-increasing with values in [0, 1]. Note: S(0) = 1 is assumed.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{time}{Integer. The observed time (either event or censoring time).}
#'     \item{event}{Integer. 1 if the event was observed, 0 if censored.}
#'   }
#'
#' @details
#' Exactly one of \code{hazard}, \code{pmf}, or \code{survival} must be
#' provided. The length of the provided vector determines the maximum
#' follow-up time (max_time = T). The function converts whichever is provided
#' to a PMF internally and then samples event times using \code{sample()}.
#'
#' For discrete survival:
#' \itemize{
#'   \item Hazard: h(t) = P(T = t | T >= t)
#'   \item PMF: f(t) = P(T = t)
#'   \item Survival: S(t) = P(T > t)
#' }
#'
#' The relationships are:
#' \itemize{
#'   \item h(t) = f(t) / S(t-1)
#'   \item f(t) = h(t) * S(t-1)
#'   \item S(t) = S(t-1) * (1 - h(t))
#' }
#'
#' Administrative censoring occurs at max_time (the length of the input
#' vector) for subjects who have not experienced the event.
#'
#' @examples
#' # Simulate with constant hazard (geometric distribution)
#' constant_hazard <- rep(0.1, 20)
#' df <- simulate_discrete_survival(n = 100, hazard = constant_hazard)
#' head(df)
#'
#' # Simulate with increasing hazard (Weibull-like)
#' increasing_hazard <- 0.02 * (1:20)
#' df <- simulate_discrete_survival(n = 100, hazard = increasing_hazard)
#'
#' # Simulate with survival function
#' exp_survival <- exp(-0.1 * (1:20))
#' df <- simulate_discrete_survival(n = 100, survival = exp_survival)
#'
#' @export
simulate_discrete_survival <- function(n,
                                       hazard = NULL,
                                       pmf = NULL,
                                       survival = NULL) {

  # Validate that exactly one specification is provided
  specs_provided <- c(!is.null(hazard), !is.null(pmf), !is.null(survival))
  n_specs <- sum(specs_provided)

  if (n_specs == 0) {
    stop("One of 'hazard', 'pmf', or 'survival' must be provided.")
  }

  if (n_specs > 1) {
    stop("Only one of 'hazard', 'pmf', or 'survival' should be provided. ",
         "You provided ", n_specs, " specifications.")
  }

  # Validate n
  if (!is.numeric(n) || length(n) != 1 || n != round(n) || n < 1) {
    stop("'n' must be a positive integer.")
  }
  n <- as.integer(n)

  # Convert to PMF values using helper functions
  if (!is.null(hazard)) {
    if (!is.numeric(hazard) || length(hazard) < 1) {
      stop("'hazard' must be a numeric vector of length >= 1.")
    }
    if (any(hazard < 0 | hazard > 1)) {
      stop("'hazard' values must be between 0 and 1.")
    }
    f <- hazard_to_pmf(hazard)
  } else if (!is.null(survival)) {
    if (!is.numeric(survival) || length(survival) < 1) {
      stop("'survival' must be a numeric vector of length >= 1.")
    }
    if (any(survival < 0 | survival > 1)) {
      stop("'survival' values must be between 0 and 1.")
    }
    f <- survival_to_pmf(survival)
  } else if (!is.null(pmf)) {
    if (!is.numeric(pmf) || length(pmf) < 1) {
      stop("'pmf' must be a numeric vector of length >= 1.")
    }
    if (any(pmf < 0)) {
      stop("'pmf' values must be non-negative.")
    }
    f <- pmf
  }

  max_time <- length(f)

  # Validate PMF sums to <= 1
  if (sum(f) > 1 + 1e-10) {
    stop("PMF values must sum to <= 1. Got sum = ", sum(f))
  }

  # Add probability of surviving past max_time (administrative censoring)
  # This is 1 - sum(f), representing P(T > max_time)
  prob_censored <- 1 - sum(f)
  probs <- c(f, prob_censored)
  outcomes <- seq_len(max_time + 1)  # 1, 2, ..., max_time, max_time+1 (censored)

  # Sample event times using PMF
  sampled_outcomes <- sample(outcomes, size = n, replace = TRUE, prob = probs)

  # Outcomes at max_time + 1 represent administrative censoring
  event_times <- pmin(sampled_outcomes, max_time)
  event_indicator <- as.integer(sampled_outcomes <= max_time)

  tibble::tibble(
    time = event_times,
    event = event_indicator
  )
}

