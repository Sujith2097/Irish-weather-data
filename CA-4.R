# reading the cleaned and analysed data
# structure of the modified data
cork_modified <- read.csv("cork_modified.csv")
str(cork_modified)
View(cork_modified)


# Importing the libraries
#install.packages("dplyr")
library("dplyr")
#install.packages("lubridate")
library(lubridate)
# Taking the mean of hourly recored data and grouping into months
monthly_modified <- cork_modified  %>% 
  mutate(processed_hour = floor_date(date, "hour")) %>%
  group_by(processed_hour) %>%
  summarise(Temp = (mean(median_temp)))

# structure of the modified data
str(monthly_modified)
View(monthly_modified)

# Installing the necesary time series and forecasting libraries.
#install.packages("tseries")
library(tseries)
# converting the monthly modified dataframe into time series object
myts <- ts(monthly_modified$Temp, start=c(2004, 1),end = c(2017,12), frequency=12)
myts
plot(myts,xlab="Year", 
     ylab = "Temperature",
     main="Temperature from 2004  to 2017", col = "blue")



# Checking the cycle, start, end, frequency, and class of the dataframe
cycle(myts)
start(myts)
end(myts)
frequency(myts)
class(myts)
print(summary(myts))

# chechking null values if any
na_records <- myts[!complete.cases(myts)]
sum(na_records)

# plotting a abline for the graph monthly temperatures from 2004 to 2017
plot(myts,
     xlab="Year", 
     ylab = "Temperature",
     main="Temperature from 2004  to 2017", col = "blue")
# add a straight line to show the linearity
# between temperature and time
abline(reg=lm(myts~time(myts)))

#calculate mean and Check for seasonality with trend
plot(aggregate(myts,FUN=mean))
#examine any seasonal effects within the data using a boxplot
boxplot(myts ~ cycle(myts),
        xlab="month", 
        ylab = "Temperature" ,
        main ="Monthly Temperature Boxplot from 2004 to 2017",
        col="orange",
        border="brown")

# Decomposing the time series into seaonal, trend, and irregular components.
# In this case, s.window is set to "period", 
# to ensure that extracted seasonality remains constant.
seasonal_decomposition <- stl(myts, s.window = "periodic")
plot(seasonal_decomposition)



# Checking the significance level by doing the Augmented Dickey-Fuller Test and kpss test
adf.test(myts, alternative = "stationary")
kpss.test(myts)


# P-value is 0.01, which is lower than the significance level of
# 0.05, which indiciates that this time series is now stationary.

library(forecast)
# Plotting the autocorrelation function (ACF). This
# measures how the observations in time series relate to each other.
# If the measure of autocorrelation crosses the dashed blue line,
# then that specific lag is significantly correlated with the associated
# time series. For a stationary time series, each measure of autocorrelation
# will quickly drop to zero. A non-stationary time series will drop gradually.
Acf(myts)
# Time series is non-stationary.
Pacf(myts)
# From the Acf plot we can observe that there is maximum peaks in the chart at first 
# and decrease to zero at 3rd month


# Removing the trend. -----------------------------------------------------------------------------------------------------

# Testing whether the time series with removed seasonal element needs differenced.
nsdiffs(myts)
# d=1

# There is a trend within the data, so the series is differenced 
# once. This is specified by lag = 12.
diff_myts <- diff(myts, lag=12, differences = 1)
ndiffs(diff_myts)


#opar <- par(no.readonly=TRUE)
#par(mfrow=c(1,2))
plot(myts, main = "Original Temperatures")
plot(diff_myts, main = "Differenced Temperatures dataset")
#par(opar)


adf.test(diff_myts, alternative = "stationary")
# P-value is < 0.01, which is lower than the significance level of
# 0.05, which indiciates that this time series is now stationary.

kpss.test(diff_data)
# Plotting the acf and pacf for the differenced time series data
acf(diff_myts)
# q = 6 as the autocorrelation is zero after 6th lag.
pacf(diff_myts)
# We use the undifferenced time series with the removed seasonal element  
# for the ARIMA model. As the number of differences required previously
# was 1, then d = 1. p = 1, and q = 6.

# building a seasonal arima model
fit <- arima(myts, 
             c(1,1,1), 
             seasonal = list(order = c(1,1,1), 
                             period = 12))

fit

# The 'Mean Absolute Percentage Error' can be used to
# find the prediction accuracy of the model.
accuracy(fit)

prediction <- predict(fit, n.ahead = 3 * 12)
prediction

# forecasting values based on the estimated arima model with confidence level=95
forecast_myts <- forecast(fit, level = c(95), h = 36)
forecast_myts
autoplot(forecast_myts)

plot(forecast(forecast_myts, 3), xlab = "Year", ylab = "Annual temperatures")

# ARIMA MODEL AUTOMATION. -------------------------------------------------------------------------------------------------

# Using auto.arima() to find the best possible model for the
# undifferenced time series with the removed seasonal element.
auto_arima_model <- auto.arima(myts)
auto_arima_model


# The AIC = and BIC = . These are lower than the
# respective AIC and BIC values for the estimated model, therefore it is
# a better fit.
# Checking the accuracy of the model
accuracy(auto_arima_model)


# plotting the comparisions forecast
plot(forecast(auto_arima_model, 3 * 12), xlab = "Year", ylab = "Annual temperatures")

plot(forecast(fit, 3 * 12), xlab = "Year", ylab = "Annual outcome")

# If model fits well, the residuals should be normally
# and independently distributed.
qqnorm(fit$residuals)
qqline(fit$residuals)

# Box test. The null hypothesis H0 states the autocorrelations are
# all zero.
Box.test(fit$residuals, type = "Ljung-Box")
# In this case, the p-value is . Accept H0.

# Evaluating the auto_arima_models
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)

# Box.test() function to test that the autocorrelations are all zero
Box.test(auto_arima_model$residuals, type = "Ljung-Box")

# Splitting the train and test data----------------------------------------------------------------------------------
# installing packages TSstudio splitting data
#install.packages("TSstudio")
library(TSstudio)
# Splitting the data with sample.out 
split_myts <- ts_split(ts.obj = myts, sample.out = 36)

training <- split_myts$train
testing <- split_myts$test

paste("length of myts time series:" , length(myts))

paste("length of training data:" , length(split_myts$train))

paste("length of testing data:" , length(split_myts$test))

# model forecasting for manual and auto arima model on training data
# and plotting the first three forecasted values after along with the time series

library(forecast)      
myts_model <- auto.arima(training)
myts_model
#one step forecast on test data
myts_test <- Arima(testing, model=myts_model)
myts_test
accuracy(myts_test)
#repeating using manual arima model
myts_manual_ARIMA <- arima(training, 
                           c(1,1,6), 
                           seasonal = list(order = c(1,1,5), 
                                           period = 12))
myts_manual_ARIMA


# Predictions for manaul_arima_model
mytsprediction <- predict(myts_manual_ARIMA, n.ahead = 3 * 12)
mytsprediction$pred
training
testing

