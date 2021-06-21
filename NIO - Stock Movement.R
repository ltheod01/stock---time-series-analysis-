library(devtools)
library(tidyquant)
library(crypto)
library(ggplot2)
library(tseries)
library(zoo)
library(dplyr)
library(xts)


options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

# ===============================================================================
#We first collect our data

getSymbols("NIO", from= '2011-01-01', to = '2021-06-21', warnings= FALSE, auto.assign = TRUE)

nio <- as.data.frame(date = index(NIO), coredata(NIO))
nio<-nio[4]
nio$date <- index(NIO)

#================================================================================
#Then we check for N/A values in our dataset
sum(is.na(nio))
dtaset <- ts(data = nio$NIO.Close,  frequency = 1)
plot.ts(dtaset)

# ===============================================================================

###################################################################

reg_1<-lm(nio$NIO.Close~nio$date)
windows()
chart_Series(NIO)
plot(x=nio$date, y=nio$NIO.Close)
abline(reg_1)
#From the plot, we see that the series is not covariance stationary, as it's mean
#and variance do not stay the same over a long period of time.  So we will use first
#order differencing to check if we remove non-stationarity by detrending.  Because 
#out time series is exponential, we will differantiate the logarithm of our time-series.

#================================================================================
frst_diff<-diff(log(nio$NIO.Close))
ts.plot(frst_diff)
#From the first order differentiation we have managed to detrend the time series, 
#while the existence of outliers remains.  

#================================================================================
#We will start by applying the AR(1) model and test if the residuals have any serial
#correlation
ar1<- arima(frst_diff, order=c(1,0,0))
ar2<- arima(frst_diff, order=c(2,0,0))
ar1

ar2#To extract the residuals use: ar1_resid<- residuals(ar1)
#To extract the fitted values, use: ar1_fitted <- ar1 - ar1_resid
ar1_resid<- residuals(ar1)

#We use the t-test to check for error autocorrelation.  Because we 
#will use an autoregressive model, the Durbin watson test is invalid (because the
#independent variables include past values of the dependent variable).  Hence we 
#will use a t-test involving a residual autocorrelation and the std error of the 
#residual autocorrelation.
ttest <- t.test(ar1_resid, mu = 0)
ttest[1]>qt(0.025,695,lower.tail= FALSE)


#Since the critical value is higher than the t-statistic of our test, we assume that
#our model is correctly specified and that we can use OLS.  Since no significant serial
#correlation is found, then there is no need for the AR(2) model and we can proceed with 
#the AR(1) model.



#From the summary table, we see that the t-statistic is t = -0.0263939 which is included
#in the 95% confidence interval.  Since p-value = 0.979 > t-statistic, we cannot reject H0
#and we accept that our mean equals to zero. 

#Plotting our time series along with our fitted values.
ar1_fitted <- frst_diff - ar1_resid
windows()
ts.plot(frst_diff)
points(ar1_fitted)

#Now we need to check the autocorrelations of the residuals from the model
windows()
ts.plot(ar1_resid)
acf(ar1_resid)

#================================================================================
#We will now try to use our model to predict 2 periods later
predictions <- predict(ar1, n.ahead = 3)

#Our models values are in a logarithmic scale and differenced.  So we will need 
#to apply reverse difference and then use the exponential.
predictions$pred <- exp(predictions$pred)
predictions$pred <- diffinv(predictions$pred, lag = 1, xi = nio$NIO.Close[length(nio$NIO.Close)])

#Below we plot our data with the predictions of our model.
windows()
plot.ts(nio$NIO.Close, xlim = c(0,700))
points(predictions$pred, col = 2)

#================================================================================

