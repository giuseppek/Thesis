library(MASS)
library(magrittr)
library(imputeTS)

##### fuunction for selecting data between two specific dates
DataInPeriod <- function(x, startDate, endDate) {
  startIndex = which(x[, 1] == startDate)[1]
  endIndex = which(x[, 1] == endDate)[1]
  result <- x[startIndex:endIndex, ]
}

#### function for computing the WAPE
WAPE <- function(x, y) {
  # Computes the Weighted Absolute Percentage Error (WAPE) between two vectors.
  sum(abs(x - y)) / sum(x)
}

#######function for removing missing levels in test data sets for lm models # https://stackoverflow.com/a/39495480/4185785
remove_missing_levels <- function(fit, test_data) {
# drop empty factor levels in test data
  test_data %>%
    droplevels() %>%
    as.data.frame() -> test_data
  
  # 'fit' object structure of 'lm' and 'glmmPQL' is different so we need to
  # account for it
  if (any(class(fit) == "glmmPQL")) {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$contrasts))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    map(fit$contrasts, function(x) names(unmatrix(x))) %>%
      unlist() -> factor_levels
    factor_levels %>% str_split(":", simplify = TRUE) %>%
      extract(, 1) -> factor_levels
    
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  } else {
    # Obtain factor predictors in the model and their levels
    factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                     names(unlist(fit$xlevels))))
    # do nothing if no factors are present
    if (length(factors) == 0) {
      return(test_data)
    }
    
    factor_levels <- unname(unlist(fit$xlevels))
    model_factors <- as.data.frame(cbind(factors, factor_levels))
  }
  
  # Select column names in test data that are factor predictors in
  # trained model
  
  predictors <- names(test_data[names(test_data) %in% factors])
  
  # For each factor predictor in your data, if the level is not in the model,
  # set the value to NA
  
  for (i in 1:length(predictors)) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i], ]$factor_levels
    if (any(!found)) {
      # track which variable
      var <- predictors[i]
      # set to NA
      test_data[!found, predictors[i]] <- NA
      # drop empty factor levels in test data
      test_data %>%
        droplevels() -> test_data
      # issue warning to console
      message(sprintf(paste0("Setting missing levels in '%s', only present",
                             " in test data but missing in train data,",
                             " to 'NA'."),
                      var))
    }
  }
  return(test_data)
}





########## Pre-process data ###########################
# Set input parameters
paramTable <- as.matrix(read.table("input_settings.csv", sep=",", header=TRUE))
params <- as.list(paramTable[, "Value"])
names(params) <- paramTable[, "Parameter"]


##### pre-process input data

# Import data
data <- read.table("input_table.csv", header=TRUE, sep=",", quote="")
colnames(data)[colnames(data)=="Volume"] = "Actual" 

# Update date parameters if needed
#data = data[as.numeric(as.Date(data$Date))> as.numeric(as.Date(params$MinDateFit)),] 
firstObservedDate <- data$Date[1]
if (as.Date(firstObservedDate) > as.Date(params$MinDateFit)) {
  params$MinDateFit <- firstObservedDate
}
lastObservedDate <- data$Date[length(data$Date)]
if (as.Date(params$MaxDateFit) > as.Date(lastObservedDate)) {
  params$MaxDateFit <- lastObservedDate
}
if (as.Date(params$MinDateFit) > as.Date(params$MaxDateFit)) {
  params$MaxDateFit <- params$MinDateFit
}
# Process imported data
data = data[as.numeric(as.Date(data$Date))>= as.numeric(as.Date(params$MinDateFit)),] ## discard data not used for fit
data = data[as.numeric(as.Date(data$Date))<= as.numeric(as.Date(params$MaxDateForecast)),] ## discard data for which we do not forecast
data$Date <- as.Date(data$Date)
data$Year <- as.numeric(format(data$Date, "%G")) # Year of date: %Y
data$Month <- as.numeric(format(data$Date, "%m"))
data$Week <- as.numeric(format(data$Date, "%V"))
data$Yearweek <- as.numeric(format(data$Date, "%G%V"))
data$Weeknumber <- as.numeric(factor(data$Yearweek))
data$Weekday <- as.numeric(format(data$Date, "%u"))
data$IsHoliday <- 0        
data$IsHoliday[data$Holiday!=""] <- 1
data$Actual[which(data$Actual< 0)] = NA

# Create fit data set 
fitData <- DataInPeriod(data, as.Date(params$MinDateFit), as.Date(params$MaxDateFit)) # the index cleanes if code is runned multiple time
# Include outlier column

# detect closed days in last month of data

if(length(fitData$Date)>30){
  recent<- 30
}else{
  recent<- length(fitData$Date)+1
}

most_recent_data<- fitData[(as.integer(length(fitData$Date)-recent)):(length(fitData$Date)),]
most_recent_data[is.na(most_recent_data)] = 0  
closed_days_binary <- rep(0,7)
for(i in 1:7){
  
  if (sum(most_recent_data$Actual[most_recent_data$Weekday == i]) == 0){
    closed_days_binary[i]<- 1
  }
}

closed_days <-which(closed_days_binary==1)

# create data_set excluding closed days
data_no_closed_days<- data[!data$Weekday %in% closed_days,]
# fitData_set excluding closed days
fitData_no_closed_days<- fitData[!fitData$Weekday %in% closed_days,]
#--------------------------------------------------------
forecastData_no_closed_days <- data_no_closed_days[data_no_closed_days$Date > as.Date(params$MinDateForecast),]

#ts.plot(fitData_no_closed_days$Actual[1:30]  )
y= log( fitData_no_closed_days$Actual+1 )
#plot(y)
weekday= factor(fitData_no_closed_days$Weekday)
week=  factor(fitData_no_closed_days$Week)
month= factor(fitData_no_closed_days$Month)
time_index= c(1: length(y))
time_index_2= time_index^2
time_index_3= time_index^3
time_index_4= time_index^4

## fitting and outlier detection 
################################# less than one year data ###################################
if( length(y)< 365){
  
  for(i in 1:10){
  model_1 <- lm(y ~ weekday + time_index + time_index_2+ time_index_3+ time_index_4)
  fitted <- predict( model_1, data.frame( weekday,time_index,time_index_2,time_index_3,time_index_4) )
  distances<- abs( exp(fitted) -  (fitData_no_closed_days$Actual+1) )
  outliers<- distances/(exp(fitted)) > 0.7  & distances> 3*sqrt(pmax(0,(exp(fitted)-1)))
  y=  log( fitData_no_closed_days$Actual+1 )
  y[outliers]= NA 
}
y=  log( fitData_no_closed_days$Actual+1 )
outliers[is.na(outliers)]= FALSE
residuals =  y- fitted   # so to mantain the order
residuals[outliers]= 0  # substitute outliers with zero
fitted= predict( model_1, data.frame( weekday,month, time_index,time_index_2,time_index_3,time_index_4) ) # for data smaller than 3 years
WAPE( (exp(fitted)+1),  fitData_no_closed_days$Actual)
no_trend = rep( length(y), length(forecastData_no_closed_days$Week))
no_trend_2 = no_trend^2
no_trend_3= no_trend^3
no_trend_4= no_trend^4
newdata <- data.frame( weekday= factor(forecastData_no_closed_days$Weekday), time_index= no_trend,time_index_2= no_trend_2,time_index_3= no_trend_3,time_index_4 = no_trend_4 ) # we assume no trend
predicted= predict( model_1, newdata ) 
################################################## between 1 and 3 years ###############################################
}else if( length(y)> 365 && length(y) < 1095) {
for(i in 1:10){
model_1 <- lm(y ~ weekday + month + time_index + time_index_2+ time_index_3+ time_index_4)
fitted <- predict( model_1, data.frame( weekday,month, time_index,time_index_2,time_index_3,time_index_4) )
distances<- abs( exp(fitted) -  (fitData_no_closed_days$Actual+1) )
outliers<- distances/(exp(fitted)) > 0.7  & distances> 3*sqrt(pmax(0,(exp(fitted)-1)))
y=  log( fitData_no_closed_days$Actual+1 )
y[outliers]= NA 
}
y=  log( fitData_no_closed_days$Actual+1 )
outliers[is.na(outliers)]= FALSE
residuals =  y- fitted   # so to mantain the order
residuals[outliers]= 0  # substitute outliers with zero
fitted= predict( model_1, data.frame( weekday,month, time_index,time_index_2,time_index_3,time_index_4) ) # for data smaller than 3 years

WAPE( (exp(fitted)+1),  fitData_no_closed_days$Actual)
no_trend = rep( length(y), length(forecastData_no_closed_days$Week))
no_trend_2 = no_trend^2
no_trend_3= no_trend^3
no_trend_4= no_trend^4
newdata <- data.frame( weekday= factor(forecastData_no_closed_days$Weekday),  month= factor(forecastData_no_closed_days$Month), time_index= no_trend,time_index_2= no_trend_2,time_index_3= no_trend_3,time_index_4 = no_trend_4 ) # we assume no trend
predicted= predict( model_1, newdata ) 
#################################################### more than 3 years ###############################
} else if( length(y) > 1095) {
  
  
  
  for(i in 1:10){
    model_1 <- lm(y ~ weekday + week + time_index + time_index_2+ time_index_3+ time_index_4)
    #data_no_levels= remove_missing_levels(fit=model_1, test_data=data.frame( weekday, week, time_index,time_index_2,time_index_3,time_index_4))
    
    #fitted <- predict( model_1, data.frame( weekday, week, time_index,time_index_2,time_index_3,time_index_4) )
    fitted_constant_week <- predict( model_1, data.frame( weekday,week= factor(1), time_index,time_index_2,time_index_3,time_index_4) ) 
    data_only_weeks= remove_missing_levels(model_1,data.frame( weekday= factor(1),week= factor(c(1:53,1:53,1:53)) , time_index=0,time_index_2=0,time_index_3=0,time_index_4=0)) # make data set larger so to interpolate correctly beginning and end of the year
    weeks_coefficient <- predict( model_1, data_only_weeks)  - model_1$coefficients[1] 
    weeks_coefficient<- as.vector(na.interpolation( weeks_coefficient)[54:106]) 
    fitted = fitted_constant_week + weeks_coefficient[ fitData_no_closed_days$Week]
    #ts.plot(exp(fitted)+1, ts(fitData_no_closed_days$Actual), col= c(1,2))
    #ts.plot(exp(fitted))
    distances<- abs( (exp(fitted)+1)-  fitData_no_closed_days$Actual)
    outliers<- distances/(exp(fitted)) > 0.7  & distances> 3*sqrt(pmax(0,(exp(fitted)-1)))
    y=  log( fitData_no_closed_days$Actual+1 )
    y[outliers]= NA 
  }
  y=  log( fitData_no_closed_days$Actual+1 )
  outliers[is.na(outliers)]= FALSE
  sum(outliers)
  summary(model_1)
  residuals =  y- fitted   # so to mantain the order
  residuals[outliers]= 0  # substitute outliers with zero
  WAPE( (exp(fitted)+1),  fitData_no_closed_days$Actual)
  no_trend = rep( length(y), length(forecastData_no_closed_days$Week))
  no_trend_2 = no_trend^2
  no_trend_3= no_trend^3
  no_trend_4= no_trend^4
  
  ######################fare la stessa cosa di su !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  newdata <- data.frame( weekday= factor(forecastData_no_closed_days$Weekday),  week= factor(forecastData_no_closed_days$Week), time_index= no_trend,time_index_2= no_trend_2,time_index_3= no_trend_3,time_index_4 = no_trend_4 ) # we assume no trend
  predicted= predict( model_1, newdata ) 
  
}

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
