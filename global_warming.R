library(ggplot2)
library(forecast)
library(ggfortify)

# Extract data from file
myReadData_byDate <- function(file, date_ini, date_fin, column){
  df <- read.csv(file, sep = ';', row.names = 1, stringsAsFactors = FALSE)
  idx_Dates <- as.character(seq(as.Date(date_ini, format = '%d/%m/%Y'), as.Date(date_fin, format = '%d/%m/%Y'), 'days'))
  return(na.omit(df[idx_Dates, column, drop = FALSE]))
}

# Read file
raw_data <- read.csv("0200E-19200101-20181231.csv", sep = ";", row.names = 1, stringsAsFactors = FALSE)

# Convert the timestamp to column
data <- cbind(rownames(raw_data), raw_data, row.names = NULL)

# Name column
colnames(data) <- c("date", colnames(raw_data))

# Select only temperature variables
temps <- data[, c("date", "TMEDIA", "TMIN", "TMAX")]

# Convert date to a suitable format
temps$date <- as.POSIXct(temps$date)

# Keep only temperature data without Nan
temps <- na.omit(temps)

# Plot with three temperature time series
ggplot(temps, aes(date)) + 
  geom_line(aes(y = TMEDIA, colour = "TMEDIA")) + 
  geom_line(aes(y = TMIN, colour = "TMIN")) +
  geom_line(aes(y = TMAX, colour = "TMAX")) +
  expand_limits(x = c(min(temps$date), max(temps$date)))

# Create the first linear model for average temperature (TMED) over time
model_tmed <- lm(TMEDIA ~ date, data = temps)

# Model summary mean temp
summary(model_tmed)

# Another model for minimum temperature
model_tmin <- lm(TMIN ~ date, data = temps)

# Model summary minimum temperature
summary(model_tmin)

# Create another linear model for maximum temperature (TMAX) over time
model_tmax <- lm(TMAX ~ date, data = temps)

# Model summary maximum temperature
summary(modelo_tmax)

# Convert data into a time series with an annual frequency (365 days)
temp_media_ts <- ts(temps$TMEDIA, frequency = 365, start = c(1926, 1, 1))

# Plot the time series
autoplot(temp_media_ts, ts.colour = "blue", ts.linetype = "dashed")

# Decompose the time series into seasonal and trend components
autoplot(stl(temp_media_ts, s.window = "periodic"), ts.colour = "blue")

# Calculate data for the entire year 2100 using the average temperature model
new_df <- data.frame(date = as.Date(seq(from = as.Date("2100-1-1"), to = as.Date("2100-12-31"), by = "days"), origin = "1-1-1926"), TMED = NA)

# Calculate the average temperature in 2100
mean(predict(object = model_tmax, newdata = new_df))

# Compare with the overall average temperature
mean(temp_media_ts)

# Compare with the average temperature in 1926
mean(window(temp_media_ts, start = c(1926, 1), end = c(1926, 365)))

# STL Model from 2017 onwards and average temperatures
a2018 <- window(temp_media_ts, start = c(2017, 1))
modelo_stl <- stl(a2018, s.window = "periodic")
prediccion_a_stl <- forecast(modelo_stl, h = 365)
plot(prediccion_a_stl)

# Prediction for 10 years - 3650 days
prediccion_10a_stl <- forecast(modelo_stl, h = 3650)
plot(prediccion_10a_stl)

# ARIMA 
arima_2018 <- window(temp_media_ts, start = c(2017, 1))
modelo_arima <- auto.arima(arima_2018)

# Prediction for one year or 365 days
prediccion_a <- forecast(modelo_arima, 365)

# Plot with smoothed lines for TMAX and TMIN
ggplot(temps, aes(date)) + 
  geom_line(aes(y = TMEDIA, colour = "TMEDIA"), linewidth = 1.2) + 
  geom_smooth(aes(y = TMAX, colour = "TMAX"), se = FALSE, method = "loess", formula = y ~ x, size = 1) +
  geom_smooth(aes(y = TMIN, colour = "TMIN"), se = FALSE, method = "loess", formula = y ~ x, size = 1) +
  expand_limits(x = c(min(temps$date), max(temps$date))) +
  labs(y = "Temperature (°C)") +
  scale_color_manual(name = "Temperature", values = c("TMEDIA" = "black", "TMAX" = "red", "TMIN" = "blue")) +
  theme_minimal()


