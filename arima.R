#install.packages("keras", lib= "/home/giuseppe/anaconda3/envs/rstudio/lib/R/library",  repos="http://cran.cnr.berkeley.edu" )
plot(data$Actual)
setwd("~/Desktop/Thesis/code")
library(tseries)
library(forecast)
 library(nortest)
 citation("tseries")
 citation("xts")
 citation("nortest")

ts.plot(residuals)
adf.test(residuals)
pp.test(residuals) # all unit root tests indicate stationarity but...
jarque.bera.test(ts(residuals))# normality is refused
Box.test(residuals, lag=20, type= "Ljung-Box") # observations are not indipendent
acf(residuals, lag.max=  20 )
pacf(residuals,lag.max=  20 )
# As already explored in the Italian market by Gianfreda and Grossi (2010),
# an ARFIMA model will be fitted
residuals= ts(residuals)
model = arima(ts(residuals), order = c(1L, 0L, 1L),
      seasonal = list(order = c(4L, 0L, 0L), period = 7), fixed= c( ),
      include.mean = FALSE, transform.pars= FALSE)
summary(model)
tsdiag(model)
acf(model$residuals, lag.max=  60)
pacf(model$residuals, lag.max=  40)
pm3 <- predict( model, n.ahead = length(forecastData$Actual))
ts.plot(cbind(residuals, pm3$pred, pm3$pred - 2 * pm3$se, pm3$pred + 2 * pm3$se), col = c("black","black", "red", "red"))
abline(v = 1980)
h= 250
ts.plot(ts( pm3$pred[1:h]+ forecastData$Forecast[1:h]), forecastData$Actual[1:h])
ts.plot( ts(forecastData$Forecast[1:h]), forecastData$Actual[1:h])
WAPE(  exp(pm3$pred[1:h])* Lea_forecast[1:h], forecastData$Actual[1:h]  )
WAPE(  Lea_forecast[1:h], forecastData$Actual[1:h]  )


# Fit trend+season time series
trend_week<- trend_week
ts.plot(trend_week) # the series is not stationary
ts.plot((diff(trend_week, lag=1))) # the series presents eteroschedasticity, but is mean stationary
adf.test(diff(trend_week))
pp.test(diff(trend_week)) # all unit root tests indicate stationarity but...
jarque.bera.test(ts(diff(trend_week)))# trend seems normally distibuted
Box.test(diff(trend_week), lag=20, type= "Ljung-Box") # there are some dependencies
acf(diff(trend_week) , lag.max=  40) # ACF
pacf(diff(trend_week), lag.max=  40 ) # neither PACF
rm(model_level)
model_level = arima(ts(diff(trend_week)), order = c(1L, 0L, 1L),
              seasonal = list(order = c(0L, 0L, 0L), period = 52), 
               transform.pars= FALSE, include.mean = F)
if(exists("model_level")){

tsdiag(model_level)
#acf( (model_level$residuals), lag.max = 10)
#pacf( model_level$residuals, lag.max = 10)
pm_trend <- predict( model_level, n.ahead = 53)


plot(pm_trend$pred)
predictions_model_trend =  pm_trend$pred
} else { predictions_model_trend = ts(rep(mean(diff(trend_week)),53),start = (length(trend_week)+1)) }
prediction_trend_Lea= diff(trend_week)[ length( diff(trend_week))] + 0* pm_trend$pred
ts.plot(cbind(ts(diff(trend_week)),prediction_trend_Lea)) 
abline(v = length(diff(trend_week)), col= 7)# prediction with LEA

plot(predictions_model_trend)
ts.plot(cbind(ts(diff(trend_week)), predictions_model_trend, predictions_model_trend - 2 * pm_trend$se, predictions_model_trend + 2 * pm_trend$se), col = c("black","black", "red", "red"))
abline(v = length(trend_week), col= 7) # prediction with ARMA
predictions_model_level <- predictions_model_trend*0 + cumsum(predictions_model_trend)
ts.plot(cbind(ts(trend_week),predictions_model_level+ trend_week[ length(trend_week)]))
abline(v = length(diff(trend_week))+1, col= 2)
plot(predictions_model_level)

indices= as.numeric(as.factor(forecastData$Yearweek))
arima_forecast = ( exp(predictions_model_level[ indices]) * forecastData$Dayfactor * forecastData$Season)  
#Lea_forecast = forecastData$Dayfactor* 1.596321
h= 269
WAPE(  arima_forecast[1:h], forecastData$Actual[1:h]  )
#WAPE(  Lea_forecast[1:h], forecastData$Actual[1:h]  )
WAPE(  forecastData$Forecast[1:h], forecastData$Actual[1:h]  ) 
ts.plot( ts(arima_forecast[1:h]), forecastData$Actual[1:h])
#ts.plot(  ts(Lea_forecast[1:h]), forecastData$Actual[1:h] )
ts.plot(  ts(forecastData$Forecast[1:h]), forecastData$Actual[1:h] ) # LEA

plot(smooth.spline(fitData$Actual, df=1))
garch(residuals)
library(rugarch)
citation("rugarch")
modelspec1 <- ugarchspec(variance.model = list(model = "sGARCH"),
                              mean.model = list(armaOrder = c(1,14),
                                                    external.regressors = as.matrix(cb
                                                                                       ind(lagged1_variance_sicily,
                                                                                            lagged1_vari
                                                                                           ance_south, sundays_bin)),
                                                    arfima= TRUE),
                              distribution.model = "sstd" ,# Student's t.distribution
                             fixed.pars = list(arfima= 0.49, omega= 0,ma1=0, ma2=0,
                                                  ma3=0, ma4=0,ma5=0, ma6=0, ma8=0, ma9=
                                                     0, ma10=0,ma11=0, # d parameter is fixed to the value found in Gianfreda(2010)
                                                  . ma12=0, ma13=0 )
                             )
modelspec2 <- ugarchspec(variance.model = list(model = "sGARCH"),
                             mean.model = list(armaOrder = c(1,7),
                                                   arfima= TRUE),
                             distribution.model = "sstd" ,# Student's t.distribution
                             
                             fixed.pars = list(arfima= 0.49, omega= 0) 
                         
                         
                         AR1 <- ugarchspec(variance.model = list(model = "sGARCH"),
                                           mean.model = list(armaOrder = c(1,0)),
                                           distribution.model = "norm" # Student's t.distribution
                                            )
                         model1 <- ugarchfit(spec = modelspec1, data = ts(daily_median_sicily), out.sampl
                                                 e=50)
                        model2 <- ugarchfit(spec = modelspec2, data = ts(daily_median_sicily), out.sampl
                                                 e=50)
                        model3 <- ugarchfit(spec = AR1, data = ts(daily_median_sicily), out.sample=50)
                        
                        model1
                        model2
                        model3
                        plot(model1)
                        #Because of AIC, we prefer model1. Moreover, we see that
                        # most of fitted parameters are highly significant.
                        # The shape parameter of the Student's t-distribution is estimated 4.34
                        # Now we check the forecasting performance:
                         
                         forecast1 = ugarchforecast(model1, data = ts(daily_median_sicily), n.ahead =1, n
                                                        .roll= 50)
                         fpm(forecast1) # Final model
                        forecast2 = ugarchforecast(model2, data = ts(daily_median_sicily), n.ahead =1, n
                                                        .roll= 50)
                         fpm(forecast2) # ARFIMA(1,7)-GARCH(1,1)
                         forecast_ar1 = ugarchforecast(model3, data = ts(daily_median_sicily), n.ahead =
                                                             1, n.roll= 50)
                         fpm(forecast_ar1) # AR(1)-GARCH(1,1)
                         plot(forecast1) 