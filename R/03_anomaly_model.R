
anomaly_model <- function(fourier_basis, eps=0.001) {
  
  ## Construct the laged fourier matrix:
  fourier_base_lagged <- fourier_basis %>% select(anomalies)%>%
                                           mutate(anomalies_lag1 = lag(anomalies, 1),
                                                  anomalies_lag8 = lag(anomalies, 8))
  
  #Adding the lagged fourier coefficients 
  fourier_base_lagged[paste("Beta(1)",as.character(0))] <- fourier_base_lagged$anomalies_lag1
  
  for (i in 1:48){
    fourier_base_lagged[paste("Beta(1)",as.character(i))] <- fourier_basis[paste("Beta",as.character(i))]*fourier_base_lagged$anomalies_lag1
  }
  
  fourier_base_lagged[paste("Beta(8)",as.character(0))] <- fourier_base_lagged$anomalies_lag8
  
  for (i in 1:48){
    fourier_base_lagged[paste("Beta(8)",as.character(i))] <- fourier_basis[paste("Beta",as.character(i))]*fourier_base_lagged$anomalies_lag8
  }
  
  anomalies_model <- lm(data = fourier_base_lagged %>% select(-c(anomalies_lag1,anomalies_lag8)), 
                        anomalies~.-1,na.action=na.exclude) # remove intercept
  
  return(anomalies_model)
}


sigm_model <- function(fourier_basis, anomalies_model, eps=0.001) {
  ## Sigma squared Modeling
  sigma <- resid(anomalies_model)
  fourier_basis$sigma <- sigma^2
  
  
  #Fitting Linear model to sigma squared
  sigma_model <- lm(data = fourier_basis %>% select(-c(Temp,Time,variance,anomalies)), sigma~.,na.action=na.exclude)
  predicted_sigma <- fitted(sigma_model)
  
  ## truncating sigma using a small positive threshold 
  predicted_sigma <- pmax(predicted_sigma, eps)
  epsilon <- sigma/sqrt(predicted_sigma)
  
  return(list(
    sigma_model = sigma_model,
    fourier_basis = fourier_basis
  ))
}