# Dissertation
Code for the UK inflation forecast in CPI,I used is mentioned here:-

library(readr)
library(forecast)
library(ggplot2)
library(tseries)

# Read your data
cpi_data <- read_table("C:/Users/DELL/OneDrive/Desktop/CPIInflation.txt")

ts_data <- ts(cpi_data$CPIRate, start = c(2000), frequency = 1)

# ARIMA(1,0,1)
arima110 <- arima(ts_data, order=c(1,0,1))

# ARIMA(1,1,1)
arima111 <- arima(ts_data, order=c(1,1,1))

# Create normal probability plot for ARIMA(1,0,1) residuals
par(mfrow=c(1,2))
qqnorm(residuals(arima110), main="Normal Probability Plot - ARIMA(1,0,1)")
qqline(residuals(arima110), col = "blue")
points(qqnorm(residuals(arima110), plot.it = FALSE)$x,
       qqnorm(residuals(arima110), plot.it = FALSE)$y,
       col = "red")

# Create normal probability plot for ARIMA(1,1,1) residuals
qqnorm(residuals(arima111), main="Normal Probability Plot - ARIMA(1,1,1)")
qqline(residuals(arima111), col = "blue")
points(qqnorm(residuals(arima111), plot.it = FALSE)$x,
       qqnorm(residuals(arima111), plot.it = FALSE)$y,
       col = "red")
arima001 <- arima(ts_data, order=c(0,0,1))
summary(arima001)

# Create normal probability plot for ARIMA(0,0,1) residuals
par(mfrow=c(1,1))
qqnorm(residuals(arima001), main="Normal Probability Plot - ARIMA(0,0,1)")
qqline(residuals(arima001), col = "blue")
points(qqnorm(residuals(arima001), plot.it = FALSE)$x,
       qqnorm(residuals(arima001), plot.it = FALSE)$y,
       col = "red")

# Estimates and Standard Errors for ARIMA models
estimates <- c(0.0034, -0.4956, 0.9144)
standard_errors <- c(0.2126, 0.2162, 0.1285)

# Calculate T-values
t_values <- estimates / standard_errors

# Create a data frame to store the results
results <- data.frame(
  Model = c("ARIMA(1,0,1)", "ARIMA(1,1,1)", "ARIMA(0,0,1)"),
  Estimate = estimates,
  Standard_Error = standard_errors,
  T_Value = t_values
)

# Print the results
print(results)

#This code generates a normal probability plot for the residuals of the ARIMA(0,0,1) model. It includes a blue reference line and red bubbles as you suggested. The par(mfrow=c(1,1)) command ensures that the plot is displayed on a single plot canvas.


# Fit the ARIMA(0,0,1) model
arima_model <- arima(ts_data, order = c(0, 0, 1))

# Calculate AIC
aic <- AIC(arima_model)

# Calculate Mean Absolute Error (MAE)
fitted_values <- fitted(arima_model)
actual_values <- cpi_data$CPIRate
mae <- mean(abs(fitted_values - actual_values))

#------------------ Print the AIC and MAE values----------------------------
print(paste("AIC:", aic))
print(paste("MAE:", mae))

# Get the fitted values from the ARIMA model
fitted_values <- fitted(arima_model)
actual_values <- cpi_data$CPIRate

# Combine the original data and fitted values
combined_data <- data.frame(
  Year = time(ts_data),
  Inflation = cpi_data$CPIRate,
  FittedValues = fitted_values
)
#-------------------------------------Time Series Plot of Inflation with ARIMA---------------------------------
# Create a plot using ggplot2
ggplot(combined_data, aes(x = Year)) +
  geom_line(aes(y = Inflation), color = "blue", size = 1) +
  geom_line(aes(y = FittedValues), color = "red", size = 1, linetype = "dashed") +
  labs(title = "Time Series Plot of Inflation with ARIMA(0,0,1) Fitted Values",
       x = "Year", y = "Inflation Rate") +
  theme_minimal()


##########################################################################
arima_model <- arima(ts_data, order = c(0, 0, 1))

# Create forecasts for each year 2024 to 2027
forecast_results <- forecast(arima_model, h = 4)

# Extract the relevant values
forecast_data <- data.frame(
  Year = 2024:2027,
  Prediction = forecast_results$mean,
  Confidence_Interval_Lower = forecast_results$lower[, "95%"],
  Confidence_Interval_Upper = forecast_results$upper[, "95%"]
)

#------------------------------Print the results------------------------------------------
print(forecast_data)


#--------------------ACF & PCF-------------------------------------------
# Augmented Dickey-Fuller test for stationarity
adf_test <- adf.test(ts_data)

# Plot time series
plot(ts_data, main="CPI Inflation", 
     ylab="Inflation Rate", xlab="Year")

# ACF and PACF plots  
par(mfrow=c(1,2))
acf(ts_data, main="ACF")
pacf(ts_data, main="PACF")

# ARIMA(1,0,1)
arima110 <- arima(ts_data, order=c(1,0,1))
summary(arima110)

# ACF and PACF of residuals
par(mfrow=c(1,2))
acf(residuals(arima110), main="ACF Residuals of 110")
pacf(residuals(arima110), main="PACF Residuals of 110")

# ARIMA(1,1,1) 
arima111 <- arima(ts_data, order=c(1,1,1))
summary(arima111)

# ACF and PACF of residuals
par(mfrow=c(1,2))
acf(residuals(arima111), main="ACF Residuals of 111")
pacf(residuals(arima111), main="PACF Residuals of 111")


# ARIMA(0,0,1) 
arima001 <- arima(ts_data, order=c(0,0,1))
summary(arima001)

#ACF and PACF of residuals
par(mfrow=c(1,2))
acf(residuals(arima001), main="ACF Residuals of 001")
pacf(residuals(arima001), main="PACF Residuals of 001")

#-----------------calculate MSE-----------------------------------

mse_arima110 <- mean(arima110$residuals^2)

# Calculate MSE for ARIMA(1,1,1)
mse_arima111 <- mean(arima111$residuals^2)

# Calculate MSE for ARIMA(0,0,1)
mse_arima001 <- mean(arima001$residuals^2)

# Print the MSE values
cat("MSE for ARIMA(1,1,1):", mse_arima110, "\n")
cat("MSE for ARIMA(0,0,1):", mse_arima001, "\n")


THE CPI INFLATION RATE DATA USED :-
Date	CPIRate
2000	0.80
2001	1.20
2002	1.30
2003	1.40
2004	1.30
2005	2.10
2006	2.30
2007	2.30
2008	3.60
2009	2.20
2010	3.30
2011	4.50
2012	2.80
2013	2.60
2014	1.50
2015	0
2016	0.70
2017	2.70
2018	2.50
2019	1.80
2020	0.90
2021	2.60
2022	9.10
2023	6.10
2024	0.90
2025	0.10
2026	0.50
2027	1.60























