# Utkarsh Kulshrestha
# kuls.utkarsh1205@gmail.com

#Load the packages and the data
library('ggplot2')
library('forecast')
library('tseries')

daily_data = read.csv('filepath', header=TRUE, stringsAsFactors=FALSE)
View(daily_data)

#Examine the data for any outliers, volatility, or irregularities
daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")

#Remove the outliers by tsclean() identifies and replaces outliers using series smoothing and decomposition
count_ts = ts(daily_data[, c('cnt')])
daily_data$clean_cnt = tsclean(count_ts)
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

# weekly or monthly moving average, smoothing the series into something more stable and therefore predictable
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

#Decompose the data Seasonal, Trend, cycle component, residual error
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#Note that stl() by default assumes additive model structure. 
#Use allow.multiplicative.trend=TRUE to incorporate the multiplicative model.

#Stationarity
#Augumented Dickey's-Fuller Test
adf.test(count_ma, alternative = "stationary")

#Auto-corelation & Model order
Acf(count_ma, main='')
Pacf(count_ma, main='')

#Plotting the series with differencing order 1
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

#Obtaining d,f Parameters
Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#Fitting ARIMA model
auto.arima(deseasonal_cnt, seasonal=FALSE)

#Evaluation & Iteration
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

#Take optimally observed parameters
fit2 = arima(deseasonal_cnt, order=c(1,1,7))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

#Forecast horizon
fcast <- forecast(fit2, h=30)
plot(fcast)

#For future forecasting take the holdout sample for test
hold <- window(ts(deseasonal_cnt), start=700)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

#Forecast with seasonal components
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)

