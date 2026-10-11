#Define the seasonal coefficients of the auto-regressive model.
phi_1 <- function(t, fourier_basis, anomalies_model) {
  phi1 <- as.numeric(coef(anomalies_model)[1])
  for(i in 2:49){
    phi1 <- phi1 + as.numeric(coef(anomalies_model)[i])*fourier_basis[t,paste("Beta",as.character(i-1))]
  }
  return(phi1)
}

phi_8 <- function(t, fourier_basis, anomalies_model) {
  phi8 <- as.numeric(coef(anomalies_model)[50])
  for(j in 1:48){
    phi8 <- phi8 + as.numeric(coef(anomalies_model)[j+50])*fourier_basis[t,paste("Beta",as.character(j))]
  }
  return(phi8)
}


simulate <- function(nb_years, model_mt, model_variance, sigma_model, eps=0.001){
  
  number <- nb_years*8*365
  sim_mt <- rep(na.approx(fitted(model_mt)[1:(365*8)]),times=nb_years)
  
  # truncate the negative variances
  sim_var <- pmax(na.approx(fitted(model_variance)[1:(365*8)]),eps)
  sim_epsilon <- rep(sqrt(sim_var),times=nb_years)
  
  # truncate the negative sigmas
  sim_sigma <- na.approx(fitted(sigma_model)[((365*8)+1):(365*8*2)])
  sim_sigma <- rep(sqrt(pmax(sim_sigma,eps)),times=nb_years)
  
  # generate white noise for the whole simulation
  sim_epsilon <- rnorm(number,0,1)
  phi1 <- rep(phi_1(1:(365*8), fourier_basis, anomalies_model), times=nb_years)
  phi8 <- rep(phi_8(1:(365*8), fourier_basis, anomalies_model), times=nb_years)
  sim_anomalies <- numeric(number)
  sim_temperature <- numeric(number)
  sim_anomalies[1] <- sim_sigma[1]*sim_epsilon[1]
  
  for(i in 2:8){
    sim_anomalies[i] <- phi1[i]*sim_anomalies[i-1]+sim_sigma[i]*sim_epsilon[i]
  }
  
  for(j in 9:number){
    sim_anomalies[j] <-phi1[j]*sim_anomalies[j-1]+phi8[j]*sim_anomalies[j-8]+sim_sigma[j]*sim_epsilon[j]
  }
  
  sim_temperature <- sim_mt + sim_anomalies*sim_epsilon
  
  return(sim_temperature)
} 


# Adding leap days 29/02
add_29 <- function(arr, start,end){
  arr_29 <- arr[1:((31+28)*8)]
  for(i in start:(end-1)){
    start_i <- (365*8*(i-start))+(31+28)*8
    end.i <- 365*8*(i-start+1)+(31+28)*8
    if(i %% 4 == 0){
      avg <- (arr[start_i]+arr[start_i+1])/2
      arr_29 <- c(arr_29, rep(avg,8),arr[(start_i+1):end.i])
    }else{
      arr_29 <- c(arr_29, arr[(start_i+1):end.i])
    }
  }
  arr_29 <- c(arr_29, arr[((365*8*(end-start))+(31+28)*8+1):length(arr)])
  return(arr_29)
}


## Simulation granularity : 
# daily data:

daily_temp <- function(arr){
  nbr_days <- as.integer(length(arr)/8)
  daily <- numeric(nbr_days)
  for(i in 1:nbr_days){
    daily[i] <- mean(arr[(8*(i-1)+1):(8*i)])
  }
  return(daily)
}


#hourly data:

hourly_temp <- function(arr){
  nbr_hours <- as.integer((length(arr)-1)*3)+1
  hourly <- rep(NaN, nbr_hours)
  for(i in seq(1,nbr_hours,3)){
    hourly[i] <- arr[1+(i-1)%/%3]
  }
  return(na.approx(hourly))
}


## Ploting scenarios
generate_scenarios <- function(nbr_scenarios, nb_years, model_mt, model_variance, sigma_model) {
  
  #We reject scenarios with aberrant values above 50 and under -50
  df_scenarios <- data.frame(simulate(nb_years=nb_years, model_mt=model_mt, model_variance=model_variance, sigma_model=sigma_model, eps=0.001))
  for(i in 1:nbr_scenarios){
    sim <- simulate(nb_years=nb_years, model_mt=model_mt, model_variance=model_variance, sigma_model=sigma_model, eps=0.001)
    # Reject scenarios with aberrant values
    while(max(sim) > 50 | min(sim) < -50){
      sim <- simulate(nb_years=nb_years, model_mt=model_mt, model_variance=model_variance, sigma_model=sigma_model, eps=0.001)
    }
    df_scenarios[paste("scenario",as.character(i))] <- sim
  }
   
  colors <- rainbow(nbr_scenarios)
  plot(df_scenarios$simulate.nb_years., type = "l", col = colors[1],ylim=c(-10,40),
       ylab = "Temperature",xlab="Time",main="Scenarios during 1 year")
  for(i in 1:nbr_scenarios){
    lines(df_scenarios[paste("scenario",as.character(i))], col = colors[i])
  }
  
  
  par(xpd=TRUE)
  plot(df_scenarios$simulate.nb_years.[1:8], type = "l", col = colors[1],ylim=c(-2,12),
       ylab = "Temperature",xlab="Time",main="Scenarios during 1 day")
  for(i in 1:nbr_scenarios){
    lines(df_scenarios[,i+1][1:8], col = colors[i])
  }
  
  
  par(xpd=TRUE)
  plot(df_scenarios$simulate.nb_years.[1:(8*30)], type = "l", col = colors[1],ylim=c(-10,23),
       ylab = "Temperature",xlab="Time",main="Scenarios during 1 month")
  for(i in 1:nbr_scenarios){
    lines(df_scenarios[,i+1][1:(8*30)], col = colors[i])
  }
  
  return(df_scenarios)
}