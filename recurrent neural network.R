
library(keras)
library(tensorflow)



Series = ts( AirPassengers)  # your time series 
diffed = Series
# transform data to stationarity
#diffed = diff(Series, differences = 1)

k=1
# create a lagged dataset, i.e to be supervised learning

lags <- function(x, k){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lags(diffed, k)


## split into train and test sets

N = nrow(supervised)
n = round(N *0.66, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]


## scale data
normalize <- function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}


## inverse-transform
inverter = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  n = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(n)
  
  for( i in 1:n){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}


Scaled = normalize(train, test, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]

## fit the model

dim(x_train) <- c(length(x_train), 1, 1)
dim(x_train)
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1
units = 10

model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)



model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02 , decay = 1e-4 ),  
  metrics = c('accuracy')
)



summary(model)

nb_epoch = 300
for(i in 1:nb_epoch ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}


L = length(x_test)
dim(x_test) = c(length(x_test), 1, 1)

scaler = Scaled$scaler

predictions = numeric(L)
yhat = x_test[1 , , ]
X= 0
for(i in 1:L){
  
  
  dim(yhat) = c(1,1,1)
  # forecast
  yhat = model %>% predict(yhat, batch_size=batch_size)
  
  # invert scaling
  predictions[i] = inverter(yhat, scaler,  c(-1, 1))
  #prova[i]= yhat
  
  # invert differencing
  #yhat  = yhat + Series[(n+i)] 
  
  # save prediction
   
}

ts.plot(predictions, ts(Series[(n+1):(N)]), col= c(1,2))

length(predictions)
length(Series[(n+1):(N+1)] )

##########################################################################another method

#library("nnfor", lib.loc="~/anaconda3/envs/rstudio/lib/R/library")
## Not run: 
fit <- mlp(ts(AirPassengers),lags= 21)
print(fit)
plot(fit)
frc <- forecast(fit,h=100)
plot(frc)

## End(Not run)
fit <- mlp(ts(trend_week))
print(fit)
plot(fit)
frc <- forecast(fit,h=100)
plot(frc)
ts.plot(diff( data$Trend))


###Good performance

boh= nnetar( ts( residuals),  repeats= 50, maxit= 70, skip = TRUE)
diretto = nnetar( fitData$Actual, repeats= 50, maxit= 150, skip = TRUE)


h= 150
nn_forecast= forecast(boh, h) 
nn_forecast_diretto= forecast(diretto, h)


plot(nn_forecast)
plot(nn_forecast_diretto)
nn_forecast2<-  nn_forecast$mean[1:h] * forecastData$Forecast[1:h]

performance = rep(0,200)
for(h in 1:200)performance[h]= (WAPE(forecastData$Forecast[1:h], forecastData$Actual[1:h]) > WAPE(nn_forecast2[1:h], forecastData$Actual[1:h])) 
mean(performance)
h=150
WAPE(  forecastData$Forecast[1:h], forecastData$Actual[1:h]  ) 

WAPE( nn_forecast_diretto$mean[1:h], forecastData$Actual[1:h] )
WAPE(nn_forecast2[1:h], forecastData$Actual[1:h])


ts.plot( ts(nn_forecast2[1:h]), forecastData$Actual[1:h], col= c(1,2))
#ts.plot(  ts(Lea_forecast[1:h]), forecastData$Actual[1:h] )
ts.plot(  ts(forecastData$Forecast[1:h]), forecastData$Actual[1:h], col= c(1,2) ) # LEA


fit <- nnetar(sunspotarea, lambda=0)
autoplot(forecast(fit,h=30))



