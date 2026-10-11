bootstrap_var <- function(alpha, data, nbr_bootstrap){
  x <- na.omit(data)
  m <- length(x)
  bootstraps_var <- c(nbr_bootstrap)
  for(i in 1:nbr_bootstrap){
    Sample <- sample(x = x,size = m,replace = TRUE)
    bootstraps_var[i] <- quantile(Sample, probs = alpha)
  }
  return(mean(bootstraps_var))
}

climate_var <- function(alpha, historical_data, df_scenarios) {
  
  scenarios <- matrix(as.matrix(df_scenarios), nrow = 1001*2920, byrow = TRUE)
  cold_monte_carlo_VaR <- quantile(scenarios, probs = alpha)
  hot_monte_carlo_VaR <- quantile(scenarios, probs = 1-alpha)
  
  # Hot
  
  hist(historical_data$Temp, main = "Hot Value At Risk", breaks = 50,xlab = "Temperature")
  q <- quantile(na.omit(historical_data$Temp), probs = 1-alpha)
  bootstrap_var_mean <- bootstrap_var(1-alpha,na.omit(historical_data$Temp),1000)
  abline(v = q, col = "red")
  abline(v = bootstrap_var_mean, col = "maroon")
  abline(v = hot_monte_carlo_VaR, col = "pink")
  legend("topleft", legend = paste(c("Hostorical VaR = ", "Bootstrap VaR  = ", "Monte Carlo VaR = "),
                                   round(c(q, bootstrap_var_mean, hot_monte_carlo_VaR), 2)), 
         col = c("red", "maroon", "pink"), lty = 1, bty = "n")
  
  # Cold
  
  hist(historical_data$Temp, main = "Cold Value At Risk", breaks = 50,xlab = "Temperature")
  q <- quantile(na.omit(historical_data$Temp), probs = alpha)
  bootstrap_var_mean <- bootstrap_var(alpha,na.omit(historical_data$Temp),1000)
  abline(v = q, col = "blue")
  abline(v = bootstrap_var_mean, col = "midnightblue")
  abline(v = cold_monte_carlo_VaR, col = "royalblue")
  legend("topright", legend = paste(c("Hostorical VaR = ", "Bootstrap VaR  = ", "Monte Carlo VaR = "),
                                    round(c(q, bootstrap_var_mean, cold_monte_carlo_VaR), 2)),
         col = c("blue", "midnightblue", "royalblue"), lty = 1, bty = "n")
  
  return(list(
    cold_monte_carlo_VaR=cold_monte_carlo_VaR,
    hot_monte_carlo_VaR=hot_monte_carlo_VaR
  ))
}
