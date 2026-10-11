## Modeling

# function that extract the tri-hour in 1:8
ht <- function(arr){
  return((arr-1)%%8+1)
}

# function that extract the day in 1:365
dt <- function(arr){
  
  arr <- (arr-1)%%(8*365)+1
  day <- ((arr-1)%/%(8))+1
  
  return(day)
}

# Adding fourier series variables

construct_fourier_basis <- function(data) {
  
  n = length(data$Temp)
  
  fourier_basis <- data.frame(
                      Time = 1:n,
                      Temp = data$Temp
                    )
  
  for (i in 1:48){
    fourier_basis[paste("Beta",as.character(i))] = 1:n
  }
  
  #Building the fourier matrix
  
  for (j in 1:3){
    fourier_basis[paste("Beta",as.character(2*j-1))] = cos(((2*pi*j)/365)*dt(fourier_basis$Time))
    fourier_basis[paste("Beta",as.character(2*j))] = sin(((2*pi*j)/365)*dt(fourier_basis$Time))
    fourier_basis[paste("Beta",as.character(2*j-1+6))] = cos(((2*pi*j)/8)*ht(fourier_basis$Time))
    fourier_basis[paste("Beta",as.character(2*j+6))] = sin(((2*pi*j)/8)*ht(fourier_basis$Time))
    for(i in 1:3){
      fourier_basis[paste("Beta",as.character(12*j+4*i-15+12))] = cos(((2*pi*j)/365)*dt(fourier_basis$Time))*cos(((2*pi*i)/8)*ht(fourier_basis$Time))
      fourier_basis[paste("Beta",as.character(12*j+4*i-14+12))] = cos(((2*pi*j)/365)*dt(fourier_basis$Time))*sin(((2*pi*i)/8)*ht(fourier_basis$Time))
      fourier_basis[paste("Beta",as.character(12*j+4*i-13+12))] = sin(((2*pi*j)/365)*dt(fourier_basis$Time))*cos(((2*pi*i)/8)*ht(fourier_basis$Time))
      fourier_basis[paste("Beta",as.character(12*j+4*i-12+12))] = sin(((2*pi*j)/365)*dt(fourier_basis$Time))*sin(((2*pi*i)/8)*ht(fourier_basis$Time))
    }
  }
  
  return(fourier_basis)
  
}


average_model <- function(fourier_basis) {
  # Average temperature model
  model_mt <- lm(data = fourier_basis %>% select(-c(Time)), Temp~.,na.action=na.exclude)
  
  return (model_mt)
  
}


variance_model <- function(fourier_basis, model_mt) {
  ## Variance Modeling
  error <- resid(model_mt)
  fourier_basis$variance <- error^2
  ## regression on R^2
  model_variance <- lm(data = fourier_basis %>% select(-c(Time,Temp)), variance~.,na.action=na.exclude)
  
  ## Temperature anomalies
  prediction_variance <- fitted(model_variance)
  fourier_basis$anomalies <- error/sqrt(prediction_variance)
  
  return(list(
    model_variance = model_variance,
    fourier_basis = fourier_basis
  ))
  
}











