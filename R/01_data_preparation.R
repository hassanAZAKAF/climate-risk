prepare_data <- function(data) {
  
  #Date transformation
  
  dt <- data
  dt$Date <- strptime(dt$Date,"%d/%m/%Y %H:%M:%S")
  
  dt$month <- month(as.Date(data$Date,"%d/%m/%Y"))
  dt$year <- year(as.Date(data$Date,"%d/%m/%Y"))
  dt$week <- week(as.Date(data$Date,"%d/%m/%Y"))
  dt$day <- day(as.Date(data$Date,"%d/%m/%Y"))
  
  #Removing 29 February
  
  new_data <- filter(dt, !(year %% 4 == 0 & month == 2 & day == 29)) 
  
  #Replacing the extreme value -99.90 with NaN 
  
  new_data$Temp[new_data$Temp == -99.9] <- NaN
  
  return(new_data)
  
} 


