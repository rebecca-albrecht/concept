#' Estimate the decision boundary for one grouping
#'
#' Fits a logistic regression of the binary response on the stimulus value
#' for a single participant in a single condition. The decision boundary is defined
#' as the stimulus value at which the predicted probability of a response coded
#' as 1 equals 0.5:
#'
#' \deqn{\hat b = - \hat\beta_0 / \hat\beta_1.}
#'
#' The input data must contain the internal columns `.ecc_condition`,
#' `.ecc_response`, and `.ecc_x`.
#'
#' @param data A data frame containing data for one participant.
#' @param condition Condition label used for filtering.
#'
#' @return A tibble with the estimated coefficients, decision boundary,
#' status code, and warning flag.
#'
#' @keywords internal
.get_db <- function(data, condition) {
  # Keep only trials from the requested condition.
  data_condition <- dplyr::filter(data, .data$.ecc_condition == condition)

  error_result <- function(message) {
    tibble::tibble(
      condition = condition,
      beta0 = NA_real_,
      beta1 = NA_real_,
      decision_boundary = NA_real_,
      status = 3,
      flag = message
    )
  }

  if (!is.numeric(data_condition$.ecc_response) ||
      !all(data_condition$.ecc_response %in% c(0, 1))) {
    return(error_result("response must be numeric and coded 0/1"))
  }

  if (length(unique(data_condition$.ecc_response)) < 2L) {
    return(error_result("baseline response contains fewer than two response classes"))
  }

  if (!is.numeric(data_condition$.ecc_x) ||
      length(unique(data_condition$.ecc_x)) < 2L) {
    return(error_result("intensity must be numeric with at least two distinct values"))
  }

  warn_msg <- NULL

  # Fit logistic regression and store possible warnings.
  mod <- tryCatch(
    withCallingHandlers(
      stats::glm(.ecc_response ~ .ecc_x,
        data = data_condition,
        family = stats::binomial()
      ),
      warning = function(w) {
        warn_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(mod, "error")) {
    return(error_result(paste("decision-boundary model failed:", conditionMessage(mod))))
  }

  coefs <- stats::coef(mod)
  beta0 <- unname(coefs[1])
  beta1 <- unname(coefs[2])
  db <- -beta0 / beta1

  if (!is.finite(beta0) || !is.finite(beta1) ||
      abs(beta1) <= sqrt(.Machine$double.eps) || !is.finite(db)) {
    return(error_result("decision-boundary model produced non-finite or zero-slope coefficients"))
  }

  flag <- NA_character_
  status <- 1

  if (!is.null(warn_msg)) {
    flag <- warn_msg
    status <- 2
  }

  tibble::tibble(
    condition = condition,
    beta0 = beta0,
    beta1 = beta1,
    decision_boundary = db,
    status = status,
    flag = flag
  )
}
