#' Prepare temperature data
#'
#' Converts the Date column to a date-time object, creates calendar
#' variables, removes February 29, and replaces the -99.9 temperature
#' code with NA.
#'
#' @param data A data frame containing at least `Date` and `Temp` columns.
#'
#' @return A data frame with the transformed date variables and cleaned
#' temperature observations.
#' 
#' @importFrom rlang .data
#' @export
prepare_data <- function(data) {
  
  # Date transformation
  dt <- data
  
  dt$Date <- strptime(
    dt$Date,
    format = "%d/%m/%Y %H:%M:%S"
  )
  
  dt$month <- lubridate::month(dt$Date)
  dt$year  <- lubridate::year(dt$Date)
  dt$week  <- lubridate::week(dt$Date)
  dt$day   <- lubridate::day(dt$Date)
  
  # Remove February 29
  new_data <- dplyr::filter(
    dt,
    !(.data$year %% 4 == 0 &
        .data$month == 2 &
        .data$day == 29)
  )
  
  # Replace the extreme value -99.9 with NA
  new_data$Temp[new_data$Temp == -99.9] <- NA_real_
  
  new_data
}