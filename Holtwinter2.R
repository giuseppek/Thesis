library(ggplot2)
library(forecast)
#hyndsight <- ts((data$Actual+1), frequency = 7)
#fc <- hw(subset(hyndsight,end=length(hyndsight)-35),
 #        damped = TRUE, seasonal="multiplicative", h=500)


################################## pre-process #####################################################
#remove  seasonalities 

ts.plot(fitData_no_closed_days$Actual[1:30]  )
y= log( fitData_no_closed_days$Actual+1 )
plot(y)
weekday= factor(fitData_no_closed_days$Weekday)
week=  factor(fitData_no_closed_days$Week)
month= factor(fitData_no_closed_days$Month)
time_index= c(1: length(y))
time_index_2= time_index^2
time_index_3= time_index^3
time_index_4= time_index^4

## fitting and outlier detection 
for(i in 1:10){
  model_1 <- lm(y ~ weekday + month + time_index + time_index_2+ time_index_3+ time_index_4)
  fitted <- predict( model_1, data.frame( weekday,month, time_index,time_index_2,time_index_3,time_index_4) )
  distances<- abs( exp(fitted) -  (fitData_no_closed_days$Actual+1) )
  outliers<- distances/(exp(fitted)) > 0.7 & distances> 3*sqrt(pmax(0,(exp(fitted)-1)))
  y=  log( fitData_no_closed_days$Actual+1 )
  y[outliers]= NA 
}
y=  log( fitData_no_closed_days$Actual+1 )
sum(outliers)
fitted= predict( model_1, data.frame( weekday,month, time_index,time_index_2,time_index_3,time_index_4) )
############################################### FIT WITH HOLT WINTERS METHOD################################
cleaned_data <- fitData_no_closed_days$Actual
cleaned_data[ outliers] =  exp(fitted[outliers])+1
fc= dshw( (cleaned_data+1), period1 = (7-length(closed_days)),period2 = ((7-length(closed_days))*52),armethod= FALSE,
   h=  length(forecastData_no_closed_days$Actual))
#fc= dshw( (cleaned_data+1), period1 = 5,period2 = 260,armethod= TRUE,
      #    h= 10000)

plot( fc$mean)
#autoplot(ts(fitData$Actual[1900: length(fitData$Actual)])) +
 # autolayer(fc, series="HW multi damped", PI=FALSE)+
 # guides(colour=guide_legend(title="Daily forecasts"))
fitted_hw= ts( fc$fitted )- 1
forecast_hw= ts( fc$mean )- 1

ts.plot( cleaned_data , fitted_hw , col=c(1,2))
ts.plot(forecastData_no_closed_days$Actual ,forecast_hw , col=c(1,2))


h= length(forecastData_no_closed_days$Actual)
WAPE( forecastData_no_closed_days$Actual[1:h] , forecast_hw[1:h]  )
        
########################################      
y <- msts( cleaned_data , seasonal.periods=c((7-length(closed_days)),((7-length(closed_days))*52)))

fc = auto.arima(y,D=1,trace=T,stepwise = FALSE)

for_fc = forecast(fc, h)
plot(for_fc)
for_fc$mean[1]
WAPE(for_fc$mean[1:h], forecastData_no_closed_days$Actual[1:h])
ts.plot( ts(for_fc$mean[1:h]), forecastData_no_closed_days$Actual[1:h], col= c(2,1))
####################
prova= auto.arima( fitData_no_closed_days$Actual )
diretto = nnetar( fitData_no_closed_days$Actual, repeats= 50, maxit= 150, skip = TRUE)
nn_forecast_diretto= forecast(diretto, 100)
plot(nn_forecast_diretto)
#autolayer(fc)
#fc$fitted[1:1035]
