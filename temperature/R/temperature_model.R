## Modeling

# function that extract the tri-hour in 1:8
ht <- function(arr){
  (arr-1)%%8+1
}

# function that extract the day in 1:365
dt <- function(arr){
  
  arr <- (arr-1)%%(8*365)+1
  day <- ((arr-1)%/%(8))+1
  
  day
}

# Adding fourier series variables
#' Construct Fourier basis for temperature modeling
#'
#' Creates Fourier basis variables representing seasonal and
#' intra-day temperature patterns.
#'
#' @param data A data frame containing a `Temp` column.
#'
#' @return A data frame containing `Time`, `Temp`, and Fourier basis
#' variables.
#'
#' @export
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
  
  fourier_basis
  
}



#' Fit the average temperature model
#'
#' Fits a linear regression model for temperature using the
#' Fourier basis variables.
#'
#' @param fourier_basis A data frame produced by
#' `construct_fourier_basis()`.
#'
#' @return A fitted linear model.
#'
#' @importFrom rlang .data
#' @export
average_model <- function(fourier_basis) {
  
  model_mt <- stats::lm(
    data = dplyr::select(fourier_basis, -.data$Time),
    Temp ~ .,
    na.action = stats::na.exclude
  )
  
  model_mt
}


#' Fit the temperature variance model
#'
#' Models the squared residuals from the average temperature model
#' and calculates standardized temperature anomalies.
#'
#' @param fourier_basis A data frame produced by
#' `construct_fourier_basis()`.
#' @param model_mt The average temperature model produced by
#' `average_model()`.
#'
#' @return A list containing the variance model and the updated
#' Fourier basis with variance and anomaly variables.
#'
#' @importFrom rlang .data
#' @export
variance_model <- function(fourier_basis, model_mt) {
  
  # Residuals from average temperature model
  error <- stats::resid(model_mt)
  
  fourier_basis$variance <- error^2
  
  # Variance regression
  model_variance <- stats::lm(
    data = dplyr::select(
      fourier_basis,
      -.data$Time,
      -.data$Temp
    ),
    variance ~ .,
    na.action = stats::na.exclude
  )
  
  # Standardized temperature anomalies
  prediction_variance <- stats::fitted(model_variance)
  
  fourier_basis$anomalies <-
    error / sqrt(prediction_variance)
  
  list(
    model_variance = model_variance,
    fourier_basis = fourier_basis
  )
}

