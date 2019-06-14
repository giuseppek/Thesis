library(ggplot2)
library(forecast)
#hyndsight <- ts((data$Actual+1), frequency = 7)
#fc <- hw(subset(hyndsight,end=length(hyndsight)-35),
 #        damped = TRUE, seasonal="multiplicative", h=500)



fc= dshw( (fitData_no_closed_days$Actual+1), period1 = 5,period2 = 260,armethod= TRUE,
   h= (length(data_no_closed_days$Actual) - length(fitData_no_closed_days$Actual)))
fc= dshw( (fitData_no_closed_days$Actual+1), period1 = 5,period2 = 260,armethod= TRUE,
          h= 10000)

plot( fc$mean)
#autoplot(ts(fitData$Actual[1900: length(fitData$Actual)])) +
 # autolayer(fc, series="HW multi damped", PI=FALSE)+
 # guides(colour=guide_legend(title="Daily forecasts"))
fitted_hw= ts( fc$fitted )- 1
forecast_hw= ts( fc$mean )- 1

ts.plot( fitData_no_closed_days$Actual , fitted_hw , col=c(1,2))
ts.plot(data_no_closed_days$Actual[(length(fitData_no_closed_days$Actual)+1):length(data_no_closed_days$Actual)] ,forecast_hw , col=c(1,2))


h= 15
WAPE( data_no_closed_days$Actual[(length(fitData_no_closed_days$Actual)+1):length(data_no_closed_days$Actual)][1:h] , forecast_hw[1:h]  )
        
        
        
autolayer(fc)
fc$fitted[1:1035]
