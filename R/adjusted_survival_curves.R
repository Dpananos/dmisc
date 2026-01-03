#' Generate covariate-adjusted survival predictions for Cox models
#'
#' This function creates survival predictions for each scenario in a datagrid
#' using a Cox proportional hazards model. The predictions are "adjusted" in
#' that they account for the specific covariate values in each scenario,
#' producing conditional (covariate-specific) survival curves rather than
#' marginal (population-averaged) estimates.
#'
#' @param grid A data frame or tibble containing the scenarios to predict for.
#'   Each row represents one covariate profile. Must contain all variables
#'   used in the Cox model formula. Cannot be empty.
#' @param model A Cox proportional hazards model (coxph object) from the
#'   survival package.
#' @param times Optional numeric vector of times at which to evaluate
#'   predictions. All values must be positive (> 0). If NULL (default),
#'   uses event times from the fitted model. Cannot be an empty vector.
#'
#' @return A data frame with survival predictions. Each row represents one
#'   combination of a grid scenario and evaluation time. Columns include:
#'   \describe{
#'     \item{Covariate columns}{All columns from grid are preserved}
#'     \item{time}{Evaluation time point}
#'     \item{n_risk}{Number at risk at this time}
#'     \item{n_event}{Number of events at this time}
#'     \item{estimate}{Predicted survival probability (0-1)}
#'     \item{std_error}{Standard error of survival estimate}
#'     \item{conf_low}{Lower confidence interval bound}
#'     \item{conf_high}{Upper confidence interval bound}
#'   }
#'   Rows are ordered by grid row, then by time within each scenario.
#'
#' @details
#' The function processes each row of the grid independently, generating
#' a survival curve conditional on that row's covariate values. This is
#' accomplished by:
#' \enumerate{
#'   \item Splitting the grid into individual scenarios
#'   \item For each scenario, calling survival::survfit() with that row's covariates
#'   \item Extracting survival probabilities at specified or default times
#'   \item Combining results with original covariate values preserved
#' }
#'
#' The resulting predictions show how survival varies across different
#' covariate profiles, making it useful for understanding model predictions
#' and communicating results to stakeholders.
#'
#' @examples
#' \dontrun{
#' # Fit a Cox model
#' library(survival)
#' data(lung)
#' lung_clean <- na.omit(lung[, c("time", "status", "age", "sex")])
#' cox_model <- coxph(Surv(time, status) ~ age + sex, data = lung_clean)
#'
#' # Create prediction scenarios
#' library(dplyr)
#' pred_grid <- expand.grid(
#'   age = c(50, 60, 70),
#'   sex = c(1, 2)
#' )
#'
#' # Generate adjusted survival predictions
#' predictions <- adjusted_survival_curves(
#'   grid = pred_grid,
#'   model = cox_model
#' )
#'
#' # Predict at specific times
#' predictions_at_times <- adjusted_survival_curves(
#'   grid = pred_grid,
#'   model = cox_model,
#'   times = c(100, 200, 300, 400, 500)
#' )
#'
#' # Visualize
#' library(ggplot2)
#' ggplot(predictions, aes(x = time, y = estimate,
#'                         color = factor(age),
#'                         linetype = factor(sex))) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin = conf_low, ymax = conf_high, fill = factor(age)),
#'               alpha = 0.2) +
#'   labs(title = "Adjusted Survival Curves by Age and Sex",
#'        y = "Survival Probability",
#'        x = "Time") +
#'   theme_minimal()
#' }
#'
#' @importFrom tibble tibble
#' @export
adjusted_survival_curves <- function(grid, model, times = NULL) {
  # Validation
  if (!inherits(model, "coxph")) {
    stop("'model' must be a coxph object, got ", class(model)[1], " instead.")
  }

  if (!is.data.frame(grid)) {
    stop("'grid' must be a data frame or tibble.")
  }

  if (nrow(grid) == 0) {
    stop("'grid' cannot be empty. Provide at least one scenario to predict for.")
  }

  # Validate that grid contains all required covariates from the model
  model_formula <- model$formula
  required_vars <- all.vars(model_formula[[3]])  # RHS variables only
  missing_vars <- setdiff(required_vars, colnames(grid))
  if (length(missing_vars) > 0) {
    stop("'grid' is missing required model covariates: ",
         paste(missing_vars, collapse = ", "))
  }

  if (!is.null(times)) {
    stopifnot("All evaluation times must be positive." = all(times > 0))
  }

  if (!is.null(times) && length(times) == 0) {
    stop("'times' cannot be an empty vector.")
  }

  # 1. Convert the grid into a list of single-row dataframes
  grid_list <- split(grid, seq_len(nrow(grid)))

  # 2. Map the survfit function over each row of the grid
  results_list <- purrr::map_dfr(grid_list, function(row_data) {
    sf <- survival::survfit(model, newdata = row_data)

    # If times aren't provided, use the event times from the fit
    eval_times <- times
    if (is.null(eval_times)) {
      eval_times <- sf$time
    }

    # Extract survival summary at the specified times
    summ <- summary(sf, times = eval_times)

    # 3. Create a tibble with the requested columns and sensible names
    # Including row_data ensures the covariates are preserved in the output
    tidied_cols <- tibble::tibble(
      time = summ$time,
      n_risk = summ$n.risk,
      n_event = summ$n.event,
      estimate = summ$surv,
      std_error = summ$std.err,
      conf_low = summ$lower,
      conf_high = summ$upper
    )

    dplyr::cross_join(row_data, tidied_cols)
  })

  results_list
}
