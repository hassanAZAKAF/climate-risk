#' Estimate Value-at-Risk using bootstrap resampling
#'
#' Computes a bootstrap estimate of the Value-at-Risk (VaR) by repeatedly
#' resampling the input data with replacement and calculating the specified
#' quantile.
#'
#' @param alpha Probability level used to calculate the quantile.
#' @param data Numeric vector containing the historical observations.
#' @param nbr_bootstrap Number of bootstrap samples.
#'
#' @return The mean of the bootstrap quantile estimates.
#'
#' @export
bootstrap_var <- function(alpha, data, nbr_bootstrap) {
  
  x <- stats::na.omit(data)
  m <- length(x)
  
  bootstraps_var <- numeric(nbr_bootstrap)
  
  for (i in seq_len(nbr_bootstrap)) {
    
    Sample <- sample(
      x = x,
      size = m,
      replace = TRUE
    )
    
    bootstraps_var[i] <-
      stats::quantile(
        Sample,
        probs = alpha
      )
  }
  
  mean(bootstraps_var)
}


#' Calculate climate Value-at-Risk
#'
#' Calculates cold and hot climate Value-at-Risk using Monte Carlo
#' scenarios, historical observations, and bootstrap estimation.
#' Histograms of the historical temperature distribution are produced
#' with the different VaR estimates displayed as vertical lines.
#'
#' @param alpha Probability level used for the cold and hot VaR.
#' @param historical_data Data frame containing historical temperature
#'   observations in a column named `Temp`.
#' @param df_scenarios Data frame containing simulated temperature scenarios.
#'
#' @return A list containing the cold and hot Monte Carlo VaR estimates.
#'
#' @export
climate_var <- function(alpha, historical_data, df_scenarios) {
  
  # Convert scenarios into a matrix
  scenarios <- matrix(
    as.matrix(df_scenarios),
    nrow = nrow(df_scenarios),
    byrow = TRUE
  )
  
  # Monte Carlo VaR
  cold_monte_carlo_VaR <-
    stats::quantile(
      scenarios,
      probs = alpha
    )
  
  hot_monte_carlo_VaR <-
    stats::quantile(
      scenarios,
      probs = 1 - alpha
    )
  
  # ------------------------------------------------------------------
  # Hot VaR
  # ------------------------------------------------------------------
  
  graphics::hist(
    historical_data$Temp,
    main = "Hot Value At Risk",
    breaks = 50,
    xlab = "Temperature"
  )
  
  q <- stats::quantile(
    stats::na.omit(historical_data$Temp),
    probs = 1 - alpha
  )
  
  bootstrap_var_mean <- bootstrap_var(
    1 - alpha,
    stats::na.omit(historical_data$Temp),
    1000
  )
  
  graphics::abline(
    v = q,
    col = "red"
  )
  
  graphics::abline(
    v = bootstrap_var_mean,
    col = "maroon"
  )
  
  graphics::abline(
    v = hot_monte_carlo_VaR,
    col = "pink"
  )
  
  graphics::legend(
    "topleft",
    legend = paste(
      c(
        "Historical VaR = ",
        "Bootstrap VaR = ",
        "Monte Carlo VaR = "
      ),
      round(
        c(
          q,
          bootstrap_var_mean,
          hot_monte_carlo_VaR
        ),
        2
      )
    ),
    col = c("red", "maroon", "pink"),
    lty = 1,
    bty = "n"
  )
  
  
  # ------------------------------------------------------------------
  # Cold VaR
  # ------------------------------------------------------------------
  
  graphics::hist(
    historical_data$Temp,
    main = "Cold Value At Risk",
    breaks = 50,
    xlab = "Temperature"
  )
  
  q <- stats::quantile(
    stats::na.omit(historical_data$Temp),
    probs = alpha
  )
  
  bootstrap_var_mean <- bootstrap_var(
    alpha,
    stats::na.omit(historical_data$Temp),
    1000
  )
  
  graphics::abline(
    v = q,
    col = "blue"
  )
  
  graphics::abline(
    v = bootstrap_var_mean,
    col = "midnightblue"
  )
  
  graphics::abline(
    v = cold_monte_carlo_VaR,
    col = "royalblue"
  )
  
  graphics::legend(
    "topright",
    legend = paste(
      c(
        "Historical VaR = ",
        "Bootstrap VaR = ",
        "Monte Carlo VaR = "
      ),
      round(
        c(
          q,
          bootstrap_var_mean,
          cold_monte_carlo_VaR
        ),
        2
      )
    ),
    col = c("blue", "midnightblue", "royalblue"),
    lty = 1,
    bty = "n"
  )
  
  
  # Return Monte Carlo VaR
  list(
    cold_monte_carlo_VaR = cold_monte_carlo_VaR,
    hot_monte_carlo_VaR = hot_monte_carlo_VaR
  )
}