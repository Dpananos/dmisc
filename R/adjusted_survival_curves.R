#' Generate adjusted survival predictions for Cox models
#'
#' This function creates survival predictions for each scenario in a datagrid
#' using a Cox proportional hazards model.
#'
#' @param grid A data frame or tibble containing the scenarios to predict for
#' @param model A Cox proportional hazards model (coxph object)
#' @param times Optional numeric vector of times at which to evaluate
#'   predictions
#' @return A data frame with survival predictions including confidence intervals
#'
#' @export
adjusted_survival_curves <- function(grid, model, times = NULL) {
  # Validation
  if (!inherits(model, "coxph")) {
    stop("'model' must be a coxph object.")
  }

  if (!is.data.frame(grid)) {
    stop("'grid' must be a data frame or tibble.")
  }

  stopifnot("All evaluation times must be positive." = all(times > 0))

  # 1. Convert the grid into a list of single-row dataframes
  # This allows us to map the model over each profile individually
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
