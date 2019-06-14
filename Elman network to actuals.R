install.packages("RSNNS", lib= "/home/giuseppe/anaconda3/envs/rstudio/lib/R/library",  repos="http://cran.cnr.berkeley.edu" )
residuals = cleaned_actuals
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

#residuals[ residuals<0.2]= 1
plot(residuals)
n <- length(residuals)
trend_week[ trend_week<0]=0
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
X= cbind(X, fitData$Weekday[2:(n-1)], fitData$Week[2:(n-1)])
Y <- sapply(sw:(n-2),
            function(ind){
              ts.sim[ind+1]
            })

# used to validate prediction properties
# on the last point of the series
newX <- cbind( ts.sim[(n-sw):(n-1)], fitData$Weekday[(n)], fitData$Week[(n)] )
newY <- ts.sim[n]

# build an elman network based on the input
model <- elman(X, Y,
               size = c(3,3),
               learnFuncParams = c(0.01),
               maxit = 50000,
               linOut = TRUE)

#summary(model)
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

L= 250
predictions <- rep(0,L)
yhat= newY 

for(i in 1:L){ 
  # forecast
  yhat =  predict(model, newdata = cbind(yhat,forecastData$Weekday[i], forecastData$Week[i] ))
  
  # invert scaling
  predictions[i] = yhat

  #prova[i]= yhat
  
  # invert differencin
  #yhat  = yhat + Series[(n+i)] 
  
  # save prediction
  
}

#ts.plot(predictions, ts(Series[(n+1):(N)]), col= c(1,2))

acf( ts.sim[2: ( length(ts.sim)-1)]- model$fitted.values)
acf(ts.sim)
ts.plot(predictions[1:20] )
ts.plot(c( ts.sim,predictions[1:300]))
predictions_rescaled <- rescale( predictions, residuals)
#predictions_rescaled[ predictions_rescaled<10] =0

h= L
h= 100

WAPE(  predictions_rescaled[1:h]* forecastData$Forecast[1:h], forecastData$Actual[1:h]  )
WAPE(  forecastData$Forecast[1:h], forecastData$Actual[1:h]  )
WAPE(  predictions_rescaled[1:h], forecastData$Actual[1:h]  )

ts.plot
ts.plot(  ts(predictions_rescaled[1:h]* forecastData$Forecast[1:h]), forecastData$Actual[1:h], col=c(2,1))

ts.plot( ts(forecastData$Forecast[1:h]), forecastData$Actual[1:h], col=c(2,1))













plot

round( model$fitted.values, 2)
