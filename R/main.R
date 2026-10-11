# loading packages

library(temperature)

data <- read.csv("data/temperatures.csv",
                 sep = ";",
                 dec = ",")

data <- temperature::prepare_data(data)
fourier_basis <- temperature::construct_fourier_basis(data)

model_mt <- temperature::average_model(fourier_basis)

variance_model_result <- temperature::variance_model(fourier_basis, model_mt)
model_variance <- variance_model_result$model_variance
fourier_basis <- variance_model_result$fourier_basis

anomalies_model <- temperature::anomaly_model(fourier_basis, eps=0.001)

sigm_model_result <- temperature::sigm_model(fourier_basis, anomalies_model, eps=0.001)
sigma_model <- sigm_model_result$sigma_model
fourier_basis <- sigm_model_result$fourier_basis

# simulations
#sims <- simulate(nb_years=1, model_mt=model_mt, model_variance=model_variance, sigma_model=sigma_model, eps=0.001)

# scenario generations
df_scenarios <- temperature::generate_scenarios(
  nbr_scenarios=1000, 
  nb_years=10, 
  model_mt=model_mt, 
  model_variance=model_variance, 
  sigma_model=sigma_model,
  fourier_basis=fourier_basis,
  anomalies_model=anomalies_model
  ) 

# Value at Risk
climate_var_result <- temperature::climate_var(alpha=0.01, historical_data=data, df_scenarios=df_scenarios)
print(climate_var_result)


# # Plot yearly scenarios
# plot(
#   df_scenarios[[1]],
#   type = "l",
#   ylim = c(-10, 40),
#   ylab = "Temperature",
#   xlab = "Time",
#   main = "Scenarios during 1 year"
# )
#
# for (i in seq_len(nbr_scenarios)) {
#   lines(df_scenarios[[i + 1]])
# }
#
# # Plot one day
# plot(
#   df_scenarios[[1]][1:8],
#   type = "l",
#   ylim = c(-2, 12),
#   ylab = "Temperature",
#   xlab = "Time",
#   main = "Scenarios during 1 day"
# )
#
# for (i in seq_len(nbr_scenarios)) {
#   lines(df_scenarios[[i + 1]][1:8])
# }
#
# # Plot one month
# plot(
#   df_scenarios[[1]][1:(8 * 30)],
#   type = "l",
#   ylim = c(-10, 23),
#   ylab = "Temperature",
#   xlab = "Time",
#   main = "Scenarios during 1 month"
# )
#
# for (i in seq_len(nbr_scenarios)) {
#   lines(df_scenarios[[i + 1]][1:(8 * 30)])
# }
