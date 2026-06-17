#' Estimate the decision boundary for one grouping
#'
#' Fits a logistic regression of the binary response on the stimulus value
#' for a single participant in a single condition. The decision boundary is defined
#' as the stimulus value at which the predicted probability of a response coded
#' as 1 equals 0.5:
#'
#' \deqn{\hat b = - \hat\beta_0 / \hat\beta_1.}
#'
#' The input data must contain the columns `participant`, `condition`,
#' `responsenum`, and `x`.
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
  data_condition <- dplyr::filter(data, .ecc_condition == !!condition)

  warn_msg <- NULL

  # Fit logistic regression and store possible warnings.
  mod <- withCallingHandlers(
    stats::glm(.ecc_response ~ .ecc_x,
      data = data_condition,
      family = stats::binomial()
    ),
    warning = function(w) {
      warn_msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  coefs <- stats::coef(mod)
  beta0 <- unname(coefs[1])
  beta1 <- unname(coefs[2])
  db <- -beta0 / beta1

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
