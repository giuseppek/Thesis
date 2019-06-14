library(MASS)



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
summary(model_1)


plot( model_1$residuals)


residuals =  y- fitted   # so to mantain the order
residuals[outliers]= 0  # substitute outliers with zero

#########################For Graphs #########################################################################
#fitted= predict( model_1, data.frame( weekday,week, time_index,time_index_2,time_index_3,time_index_4) )  # for data bigger than 3 years
fitted= predict( model_1, data.frame( weekday,month, time_index,time_index_2,time_index_3,time_index_4) ) # for data smaller than 3 years
#prova= predict( model_1, data.frame(weekday= factor(1), week=factor(1), time_index,time_index_2,time_index_3,time_index_4))  for data longer than 3 years
level= predict( model_1, data.frame(weekday= factor(1), month =factor(1), time_index,time_index_2,time_index_3,time_index_4)) # for data smaller 

season_week =  predict( model_1, data.frame(weekday, month =factor(1), time_index= median(time_index),time_index_2 = median(time_index_2),time_index_3= median(time_index_3),time_index_4= median(time_index_4)))
season_year =  predict( model_1, data.frame(weekday= factor(1), month, time_index= median(time_index),time_index_2 = median(time_index_2),time_index_3= median(time_index_3),time_index_4= median(time_index_4)))
ts.plot( exp(fitted)+1, ts(fitData_no_closed_days$Actual), col= c(2,1))
##################################################################################################################
WAPE( (exp(fitted)+1),  fitData_no_closed_days$Actual)


acf( residuals)
#newdata <- data.frame( weekday= factor(forecastData_no_closed_days$Weekday),  week= factor(forecastData_no_closed_days$Week), time_index= c((length(y)+1): (length(y)+ length(forecastData_no_closed_days$Actual)) ))
no_trend = rep( length(y), length(forecastData_no_closed_days$Week))
no_trend_2 = no_trend^2
no_trend_3= no_trend^3
no_trend_4= no_trend^4
#newdata <- data.frame( weekday= factor(forecastData_no_closed_days$Weekday),  week= factor(forecastData_no_closed_days$Week), time_index= no_trend,time_index_2= no_trend_2,time_index_3= no_trend_3,time_index_4 = no_trend_4 ) # we assume no trend
# for data > 3 years
newdata <- data.frame( weekday= factor(forecastData_no_closed_days$Weekday),  month= factor(forecastData_no_closed_days$Month), time_index= no_trend,time_index_2= no_trend_2,time_index_3= no_trend_3,time_index_4 = no_trend_4 ) # we assume no trend
# for data < 3
predicted= predict( model_1, newdata ) 


summary(model_1)
ts.plot(predicted)
# predict new data






#as.factor( c(as.numeric(forecastData_no_closed_days$Weekday[ length(forecastData_no_closed_days$Weekday)]:52), c(1:52)))


library(RSNNS)

#
# simulate an arima time series example of the length n
#

normalize <- function(x){
  
  return(((x  - min(x)) / (max(x) - min(x)  ))*2 -1)
}
rescale <- function(x,series){
  return( ((x+1)/2)* (max(series) - min(series)  ) + min(series))
}
set.seed(10001)



plot(residuals)
#we differentiate in order to remove correlations relative to the level

residuals_dif <- diff(residuals)
acf(residuals)
acf(residuals_dif)

n <- length(residuals)
#ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = n-1)
ts.sim<- normalize(residuals)

plot(ts.sim)


#
# create an input data set for ts.sim
# sw = sliding-window size
#
# the last point of the time series will not be used
#   in the training phase, only in the prediction/validation phase
# 
sw <- 1
X <- lapply(sw:(n-2),
            function(ind){
              ts.sim[(ind-sw+1):ind]
            })
X <- do.call(rbind, X)
X= cbind(X)
Y <- sapply(sw:(n-2),
            function(ind){
              ts.sim[ind+1]
            })

# used to validate prediction properties
# on the last point of the series
newX <- cbind( ts.sim[(n-sw):(n-1)] )
newY <- ts.sim[n]

# build an elman network based on the input
model <- elman(X, Y,
               size = c(3),
               learnFuncParams = c(0.001),
               maxit = 1500,
               linOut = TRUE)

#summary(model)
extractNetInfo(model)
#model.weights()#
# plot the results
#
limits <- range(c(Y, model$fitted.values))

plot(Y, type = "l", col="red",
     ylim=limits, xlim=c(0, length(Y)),
     ylab="", xlab="")
lines(model$fitted.values, col = "green", type="l")
points(length(Y)+1, newY, col="red", pch=16)
points(length(Y)+1, predict(model, newdata= newX),
       pch="X", col="green")

#ts.plot(predict(model, newdata=matrix(rep(c(1:7),10))))

L= 100
predictions <- rep(0,L)
yhat= newY 

for(i in 1:L){ 
  # forecast
  yhat =  predict(model, newdata = yhat)
  
  # invert scaling
  predictions[i] = yhat
  
  #prova[i]= yhat
  
  # invert differencin
  #yhat  = yhat + Series[(n+i)] 
  
  # save prediction
  
}

#ts.plot(predictions, ts(Series[(n+1):(N)]), col= c(1,2))

acf( ts.sim[2: ( length(ts.sim)-1)]- model$fitted.values)
ts.plot(predictions[1:30] )
ts.plot(c( ts.sim,predictions[1:300]))
###plot for thesis
df1 = data.frame(date = fitData_no_closed_days$Date, scaled_residual =  ts.sim, legenda = "residuals")
df2 = data.frame(date = forecastData_no_closed_days$Date, scaled_residual = predictions , legenda = "elman_forecast")
df = rbind(df1[c(580:610),], df2)
ggplot(df, aes(x = date, y = scaled_residual, color= legenda)) + geom_line()
#######################################################################################

predictions_rescaled <- rescale( predictions, residuals)
residuals_fitted <- rescale( model$fitted.values, residuals)


#predictions_level <- cumsum( predictions_rescaled)
prediction_final_model =  exp( predictions_rescaled + predicted[1:L] )-1 
#prediction_final_model[ prediction_final_model<10] =0
#predictions_rescaled[ predictions_rescaled<10] =0
final_fitted=   exp( c(0, residuals_fitted, 0) + fitted )-1 
#WAPE(  fitted, fitData$Actual  )
level_elman= level+ c(0,0, diff(residuals_fitted), 0)   ##### we upload the level with Elman neural Network for graph.



h= length(forecastData_no_closed_days$Actual)

WAPE( prediction_final_model[1:h] , forecastData_no_closed_days$Actual[1:h]  ) # after fitting residuals
WAPE( (exp(predicted)-1 )[1:h] , forecastData_no_closed_days$Actual[1:h]  ) # only linear regression
#WAPE(  forecastData_no_closed_days$Forecast[1:h], forecastData_no_closed_days$Actual[1:h]  )
#WAPE(  predictions_rescaled[1:h], forecastData_no_closed_days$Actual[1:h]  )



ts.plot(  ts(prediction_final_model[1:h]), forecastData_no_closed_days$Actual[1:h], col=c(2,1))

ts.plot( ts((exp(predicted)-1 )[1:h]), forecastData_no_closed_days$Actual[1:h], col=c(2,1))


#################### Graph for Thesis ###################################


df_actuals = data.frame(date = fitData_no_closed_days$Date, Volume =  fitData_no_closed_days$Actual, legenda = "Actuals")
df_fit = data.frame(date = fitData_no_closed_days$Date, Volume = final_fitted , legenda = "Fitted")
df_df = rbind(df_actuals, df_fit)
ggplot(df_df, aes(x = date, y = Volume, color= legenda)) + geom_line() + ggtitle("Actuals and Fit from the proposed Algorithm (Skill: 'Salesteam Energie')")


################  Graph of different components #####################################

df_elman =  data.frame(date = fitData_no_closed_days$Date, Volume =  (exp(level_elman)+1), legenda = "Recurrent sequence")
df_big_level= data.frame(date = fitData_no_closed_days$Date, Volume =  (exp(level)+1), legenda = "Polynomial Level")
df_week = data.frame(date = fitData_no_closed_days$Date, Volume = (exp(season_week)+1) , legenda = "Weekly Season")
df_year = data.frame(date = fitData_no_closed_days$Date, Volume = (exp(season_year)+1) , legenda = "Yearly Season")
df_comp = rbind(df_elman, df_big_level, df_week, df_year)
ggplot(df_comp, aes(x = date, y = Volume, color= legenda)) + geom_line() + ggtitle("Different Components of the Fit (Skill: 'Salesteam Energie')")





plot

round( model$fitted.values, 2)
