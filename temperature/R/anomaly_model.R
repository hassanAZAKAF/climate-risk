#' Fit the temperature anomaly model
#'
#' Builds lagged anomaly variables and their interactions with the
#' Fourier basis, then fits a linear model without an intercept.
#'
#' @param fourier_basis A data frame containing the Fourier basis
#' and an `anomalies` column.
#' @param eps Small positive threshold used in subsequent variance
#' calculations.
#'
#' @return A fitted linear model for temperature anomalies.
#' @importFrom rlang .data
#' @export
anomaly_model <- function(fourier_basis, eps = 0.001) {
  
  # Construct lagged anomaly variables
  fourier_base_lagged <- fourier_basis |>
    dplyr::select(.data$anomalies) |>
    dplyr::mutate(
      anomalies_lag1 = dplyr::lag(.data$anomalies, 1),
      anomalies_lag8 = dplyr::lag(.data$anomalies, 8)
    )
  
  # Adding lagged Fourier coefficients
  fourier_base_lagged[paste("Beta(1)", as.character(0))] <-
    fourier_base_lagged$anomalies_lag1
  
  for (i in 1:48) {
    
    fourier_base_lagged[paste("Beta(1)", as.character(i))] <-
      fourier_basis[paste("Beta",as.character(i))] *
      fourier_base_lagged$anomalies_lag1
  }
  
  fourier_base_lagged[paste("Beta(8)", as.character(0))] <-
    fourier_base_lagged$anomalies_lag8
  
  for (i in 1:48) {
    
    fourier_base_lagged[paste("Beta(8)", as.character(i))] <-
      fourier_basis[paste("Beta",as.character(i))] *
      fourier_base_lagged$anomalies_lag8
  }
  
  # Fit anomaly model without intercept
  anomalies_model <- stats::lm(
    data = dplyr::select(
      fourier_base_lagged,
      -.data$anomalies_lag1,
      -.data$anomalies_lag8
    ),
    anomalies ~ . - 1,
    na.action = stats::na.exclude
  )
  
  anomalies_model
}


#' Fit the conditional variance model
#'
#' Models the squared residuals of the anomaly model using the
#' Fourier basis and truncates predicted variance at a positive
#' threshold.
#'
#' @param fourier_basis A data frame containing the Fourier basis
#' and temperature model variables.
#' @param anomalies_model The model returned by `anomaly_model()`.
#' @param eps Small positive threshold used to prevent non-positive
#' predicted variance.
#'
#' @return A list containing the fitted sigma model and the updated
#' Fourier basis.
#'
#' @importFrom rlang .data
#' @export
sigm_model <- function(fourier_basis, anomalies_model, eps = 0.001) {
  
  # Squared residuals from anomaly model
  sigma <- stats::resid(anomalies_model)
  
  fourier_basis$sigma <- sigma^2
  
  # Fit linear model to sigma squared
  sigma_model <- stats::lm(
    data = dplyr::select(
      fourier_basis,
      -.data$Temp,
      -.data$Time,
      -.data$variance,
      -.data$anomalies
    ),
    sigma ~ .,
    na.action = stats::na.exclude
  )
  
  predicted_sigma <- stats::fitted(sigma_model)
  
  # Truncate predicted sigma using a small positive threshold
  predicted_sigma <- pmax(predicted_sigma, eps)
  
  epsilon <- sigma / sqrt(predicted_sigma)
  
  list(
    sigma_model = sigma_model,
    fourier_basis = fourier_basis
  )
}


