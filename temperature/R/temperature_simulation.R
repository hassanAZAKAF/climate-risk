#' Calculate the seasonal AR(1) coefficient
#'
#' Calculates the time-varying AR(1) coefficient using the Fourier basis
#' and the fitted anomaly model.
#'
#' @param t Integer or numeric vector of time indices.
#' @param fourier_basis A data frame containing the Fourier basis.
#' @param anomalies_model A fitted anomaly model returned by
#'   [anomaly_model()].
#'
#' @return A numeric vector containing the seasonal AR(1) coefficients.
#'
#' @export
phi_1 <- function(t, fourier_basis, anomalies_model) {
  
  phi1 <- as.numeric(stats::coef(anomalies_model)[1])
  
  for (i in 2:49) {
    phi1 <- phi1 +
      as.numeric(stats::coef(anomalies_model)[i]) *
      fourier_basis[t, paste("Beta",as.character(i-1))]
  }
  
  as.numeric(phi1)
}


#' Calculate the seasonal AR(8) coefficient
#'
#' Calculates the time-varying AR(8) coefficient using the Fourier basis
#' and the fitted anomaly model.
#'
#' @param t Integer or numeric vector of time indices.
#' @param fourier_basis A data frame containing the Fourier basis.
#' @param anomalies_model A fitted anomaly model returned by
#'   [anomaly_model()].
#'
#' @return A numeric vector containing the seasonal AR(8) coefficients.
#'
#' @export
phi_8 <- function(t, fourier_basis, anomalies_model) {
  
  phi8 <- as.numeric(stats::coef(anomalies_model)[50])
  
  for (j in 1:48) {
    phi8 <- phi8 +
      as.numeric(stats::coef(anomalies_model)[j + 50]) *
      fourier_basis[t, paste("Beta",as.character(j))]
  }
  
  as.numeric(phi8)
}

#' Simulate temperature scenarios
#'
#' Generates temperature scenarios using the fitted mean temperature,
#' conditional variance, and volatility models.
#'
#' @param nb_years Number of years to simulate.
#' @param model_mt Fitted mean temperature model.
#' @param model_variance Fitted variance model.
#' @param sigma_model Fitted volatility model.
#' @param fourier_basis A data frame containing the Fourier basis.
#' @param anomalies_model Fitted anomaly model returned by
#'   [anomaly_model()].
#' @param eps Small positive threshold used to avoid non-positive
#'   variances.
#'
#' @return A numeric vector containing the simulated temperature series.
#'
#' @export
simulate_temperature <- function(
    nb_years,
    model_mt,
    model_variance,
    sigma_model,
    fourier_basis,
    anomalies_model,
    eps = 0.001
) {
  number <- nb_years * 8 * 365
  
  # Simulated mean temperature
  sim_mt <- rep(
    zoo::na.approx(stats::fitted(model_mt)[1:(365 * 8)]),
    times = nb_years
  )
  
  # Truncate negative variances
  sim_var <- pmax(
    zoo::na.approx(stats::fitted(model_variance)[1:(365 * 8)]),
    eps
  )
  
  # Generate white noise
  sim_epsilon <- stats::rnorm(number, mean = 0, sd = 1)
  
  # Seasonal AR coefficients
  phi1 <- rep(
    phi_1(1:(365 * 8), fourier_basis, anomalies_model),
    times = nb_years
  )
  
  phi8 <- rep(
    phi_8(1:(365 * 8), fourier_basis, anomalies_model),
    times = nb_years
  )
  
  # Volatility
  sim_sigma <- zoo::na.approx(
    stats::fitted(sigma_model)[((365 * 8) + 1):(365 * 8 * 2)]
  )
  
  sim_sigma <- rep(
    sqrt(pmax(sim_sigma, eps)),
    times = nb_years
  )
  
  # Simulate anomalies
  sim_anomalies <- numeric(number)
  
  sim_anomalies[1] <-
    sim_sigma[1] * sim_epsilon[1]
  
  for (i in 2:8) {
    sim_anomalies[i] <-
      phi1[i] * sim_anomalies[i - 1] +
      sim_sigma[i] * sim_epsilon[i]
  }
  
  
  
  for (j in 9:number) {
    sim_anomalies[j] <-
      phi1[j] * sim_anomalies[j - 1] +
      phi8[j] * sim_anomalies[j - 8] +
      sim_sigma[j] * sim_epsilon[j]
  }
  
  # Temperature = mean + anomaly
  sim_temperature <- sim_mt + sim_anomalies
  
  sim_temperature
}

#' Add leap days to simulated temperature data
#'
#' Inserts February 29 for leap years into an 8-observations-per-day
#' temperature series.
#'
#' @param arr Numeric temperature series at 3-hourly frequency.
#' @param start Starting year index.
#' @param end Ending year index.
#'
#' @return A numeric vector containing the temperature series with
#'   leap days inserted.
#'
#' @export
add_29 <- function(arr, start, end) {
  
  arr_29 <- arr[1:((31 + 28) * 8)]
  
  for (i in start:(end - 1)) {
    
    start_i <-
      (365 * 8 * (i - start)) + (31 + 28) * 8
    
    end_i <-
      365 * 8 * (i - start + 1) + (31 + 28) * 8
    
    if (i %% 4 == 0) {
      
      avg <- (arr[start_i] + arr[start_i + 1]) / 2
      
      arr_29 <- c(
        arr_29,
        rep(avg, 8),
        arr[(start_i + 1):end_i]
      )
      
    } else {
      
      arr_29 <- c(
        arr_29,
        arr[(start_i + 1):end_i]
      )
    }
  }
  
  arr_29 <- c(
    arr_29,
    arr[
      ((365 * 8 * (end - start)) + (31 + 28) * 8 + 1):
        length(arr)
    ]
  )
  
  arr_29
}



## Simulation granularity : 
# daily data:

#' Aggregate 3-hourly temperature data to daily data
#'
#' Calculates the daily mean temperature from a temperature series
#' containing eight observations per day.
#'
#' @param arr Numeric temperature series with eight observations per day.
#'
#' @return A numeric vector containing daily mean temperatures.
#'
#' @export
daily_temp <- function(arr) {
  
  nbr_days <- as.integer(length(arr) / 8)
  
  daily <- numeric(nbr_days)
  
  for (i in seq_len(nbr_days)) {
    
    daily[i] <- mean(
      arr[(8 * (i - 1) + 1):(8 * i)]
    )
  }
  
  daily
}


#hourly data:

#' Convert 3-hourly temperature data to hourly data
#'
#' Interpolates a temperature series sampled every three hours to
#' obtain hourly observations.
#'
#' @param arr Numeric temperature series sampled every three hours.
#'
#' @return A numeric vector containing hourly temperatures.
#'
#' @export
hourly_temp <- function(arr) {
  
  nbr_hours <- as.integer((length(arr) - 1) * 3) + 1
  
  hourly <- rep(NaN, nbr_hours)
  
  for (i in seq(1, nbr_hours, by = 3)) {
    hourly[i] <- arr[1 + (i - 1) %/% 3]
  }
  
  zoo::na.approx(hourly)
}



#' Generate temperature scenarios
#'
#' Generates multiple temperature scenarios at yearly,
#' daily, and monthly horizons. Scenarios containing temperatures above
#' 50 degrees or below -50 degrees are rejected.
#'
#' @param nbr_scenarios Number of scenarios to generate.
#' @param nb_years Number of years to simulate.
#' @param model_mt Fitted mean temperature model.
#' @param model_variance Fitted variance model.
#' @param sigma_model Fitted volatility model.
#' @param fourier_basis A data frame containing the Fourier basis.
#' @param anomalies_model Fitted anomaly model.
#'
#' @return A data frame containing the generated scenarios.
#'
#' @export
generate_scenarios <- function(
    nbr_scenarios,
    nb_years,
    model_mt,
    model_variance,
    sigma_model,
    fourier_basis,
    anomalies_model
) {
  
  # Generate first scenario
  first_sim <- simulate_temperature(
    nb_years = nb_years,
    model_mt = model_mt,
    model_variance = model_variance,
    sigma_model = sigma_model,
    fourier_basis = fourier_basis,
    anomalies_model = anomalies_model,
    eps = 0.001
  )
  
  df_scenarios <- data.frame(
    scenario_0 = first_sim
  )
  
  # Generate remaining scenarios
  for (i in seq_len(nbr_scenarios)) {
    
    sim <- simulate_temperature(
      nb_years = nb_years,
      model_mt = model_mt,
      model_variance = model_variance,
      sigma_model = sigma_model,
      fourier_basis = fourier_basis,
      anomalies_model = anomalies_model,
      eps = 0.001
    )
    
    # Reject aberrant scenarios
    while (max(sim) > 50 || min(sim) < -50) {
      
      sim <- simulate_temperature(
        nb_years = nb_years,
        model_mt = model_mt,
        model_variance = model_variance,
        sigma_model = sigma_model,
        fourier_basis = fourier_basis,
        anomalies_model = anomalies_model,
        eps = 0.001
      )
    }
    
    df_scenarios[[paste0("scenario_", i)]] <- sim
  }
  
  
  df_scenarios
}
  
