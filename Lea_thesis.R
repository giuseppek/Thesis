#--------------------------------------------------------
# README
#--------------------------------------------------------
# Author: Karin van Eeden / GK/ Siqiao Li/ Giuseppe C.
# 19-03-2019
# The following algorithm should be able to handle data with (large) holes. Later on we should add functionality that:
# - evaluates whether the impact of known events is significant
# - detects outliers and asks the user whether this was a known event
# 
# install.packages("lpSolve")
library(lpSolve)
Sys.setenv(TZ="UTC")


#--------------------------------------------------------
# FUNCTIONS
#--------------------------------------------------------
WAPE <- function(x, y) {
  # Computes the Weighted Absolute Percentage Error (WAPE) between two vectors.
  #
  # Args:
  #	x: numeric vector having the same lenght as y.
  #	y: numeric vector having the same lenght as x.
  #
  # Returns:
  #	The WAPE of x with respect to y. The WAPE is calculated as
  #	WAPE = sum_i |x_i - y_i| / x_i
  
  sum(abs(x - y)) / sum(x)
}

DataInPeriod <- function(x, startDate, endDate) {
  # Get all data elements between two dates.
  # 
  # Args:
  #	x: matrix where the first column contain dates ordered ascending.
  #	startDate: start date of period to return, in format "yyyy-mm-dd".
  #	endDate: end date of period to return, in format "yyyy-mm-dd".
  # 
  # Returns:
  # 	All rows in matrix x with date between startDate and endDate.
  
  startIndex = which(x[, 1] == startDate)[1]
  endIndex = which(x[, 1] == endDate)[1]
  result <- x[startIndex:endIndex, ]
}



# the input contains a column "Holiday"which is 0 when is not holiday and 1 otherwise
ForecastLO <- function(fitData, data, params) {
  # indicate holes 
  hole<- rep(0, length(fitData$Actual))
  hole[is.na(fitData$Actual)] <- 1e10
  hole[which(fitData$IsHoliday!=0)] <- 1e10
  new_map <- data.frame( real_day= sort( unique(fitData$Weekday)), index= c(1: length( unique(fitData$Weekday))) ) #bug fixed 19/03/2019
  # Parameters
  S = 53                              # length season
  D= (7- sum(closed_days_binary))   #number of day factors to consider (because of fixed closed days). This does not need to be used when multiplicative
  N <- length(fitData$Actual)   # Number of observations in the fit period
  W<-  as.numeric(length(unique(fitData$Yearweek))) # number of weeks in fitData
  g <- as.numeric(params$IncrementCoefficient)/(1.00001 - as.numeric(params$IncrementCoefficient))     		# Increment coefficient
  c_s<-  as.numeric(params$SeasonSmoothness)/(1.001 - as.numeric(params$SeasonSmoothness))	# Increment coefficient of seasonal factors
  damping<- as.numeric(params$TrendDamping)
  if (params$LogActuals == 1) {
    a <- log(fitData$Actual+1)
  } else {
    a <- fitData$Actual
  }
  a[is.na(a)] <- 0   # replace NA with zero, to be suitable for the lp function
  # Set objective function, see pdf for order variables
  objective = c(rep(0,S+4*W-2),rep(g,2*(W-2)),rep(1,2*N),rep(c_s,2*S), rep(0,D) ) 
  # Set constraints (row = constraint, column = variable)
  constraints = matrix(0,nrow=3*W-5+S+2*N,ncol=3*S+6*W+2*N-6+D) #
  # constraint 1
  
  for(n in 1:(W-1))constraints[n,S+2*W+n]=1 # d+
  for(n in 1:(W-1))constraints[n,S+3*W-1+n]=-1 # d-
  for(n in 1:(W-1))constraints[n,S+1+n]=-1 # t+(n)
  for(n in 1:(W-1))constraints[n,S+W+1+n]=1 # t-(n)
  for(n in 1:(W-1))constraints[n,S+n]=1 # t+(n-1)
  for(n in 1:(W-1))constraints[n,S+W+n]=-1 # t-(n-1)
  
  # constraint 2 
  for(n in 1:(W-2))constraints[W-1+n,S+4*W-2+n]=1 # d2+ in S+4*N-1:S+5*N-4
  for(n in 1:(W-2))constraints[W-1+n,S+2*W+1+n]=-1 # d+ in S+2*N+2:
  for(n in 1:(W-2))constraints[W-1+n,S+3*W+n]=1 # d- in S+3N+1:
  for(n in 1:(W-2))constraints[W-1+n,S+2*W+n]=1 # d+ in S+2N+1:
  for(n in 1:(W-2))constraints[W-1+n,S+3*W-1+n]=-1 # d- in S+3N:
  
  # constraint 3 
  for(n in 1:(W-2))constraints[2*W-3+n,S+5*W-4+n]=1 # d2+ in S+5N-3:
  for(n in 1:(W-2))constraints[2*W-3+n,S+2*W+1+n]=1 # d+ in S+2*N+2:
  for(n in 1:(W-2))constraints[2*W-3+n,S+3*W+n]=-1 # d- in S+3N+1:
  for(n in 1:(W-2))constraints[2*W-3+n,S+2*W+n]=-1 # d+ in S+2N+1:
  for(n in 1:(W-2))constraints[2*W-3+n,S+3*W-1+n]=1 # d- in S+3N:
  
  # constraint 4
  for(n in 1:N)constraints[3*W-5+n,S+6*W-6+n]=1 # simpler....
  for(n in 1:N)constraints[3*W-5+n,S+fitData$Weeknumber[n]]=-1
  for(n in 1:N)constraints[3*W-5+n,S+W+fitData$Weeknumber[n]]=1
  for(n in 1:N)constraints[3*W-5+n,fitData$Week[n]]=-1
  for(n in 1:N)constraints[3*W-5+n,3*S+6*W+2*N-6+new_map$index[new_map$real_day== fitData$Weekday[n]]]=-1   #day factor
  # constraint 5
  for(n in 1:N)constraints[3*W-5+N+n,S+6*W-6+N+n]=1 
  for(n in 1:N)constraints[3*W-5+N+n,S+fitData$Weeknumber[n]]=1
  for(n in 1:N)constraints[3*W-5+N+n,S+W+fitData$Weeknumber[n]]=-1
  for(n in 1:N)constraints[3*W-5+N+n,fitData$Week[n]]=1
  for(n in 1:N)constraints[3*W-5+N+n,3*S+6*W+2*N-6+new_map$index[new_map$real_day== fitData$Weekday[n]]]=1  #day factor
  # constraint 6
  constraints[3*W-4+2*N,1]=1 
  constraints[3*W-4+2*N,S]=-1
  constraints[3*W-4+2*N,S+6*W+2*N-5]=-1
  constraints[3*W-4+2*N,2*S+6*W+2*N-5]=1
  # constraint 7
  for(n in 1:(S-1))constraints[3*W-4+2*N+n,1+n]=1 
  for(n in 1:(S-1))constraints[3*W-4+2*N+n,n]=-1
  for(n in 1:(S-1))constraints[3*W-4+2*N+n,S+6*W+2*N-5+n]=-1
  for(n in 1:(S-1))constraints[3*W-4+2*N+n,2*S+6*W+2*N-5+n]=1
  
  # directions of LO problem
  directions <- c(rep("=",W-1),rep(">=",2*W+2*N-4), rep("=",S))
  # finally rhs's
  righthandsides <- c(rep(0,3*W-5),-a[1:N]- hole,a[1:N]-hole ,rep(0,S) ) ## release constraints for holes, 6 Dec 2018
  # Solve the LP 
  object <- lp("min", objective, constraints, directions, righthandsides)
  
  s <- object$solution[1:S] #seasonal factor
  i <- object$solution[(3*S+6*W+2*N-5):(3*S+6*W+2*N-5+D-1)] # day factor
  ## send the seasonality solutions directly to data$Season first
 # residuals<<-  object$solution[(S+ 6*W-5):(S+ 6*W+N - 6 )] - object$solution[(S+ 6*W+N -5):(S+6*W + 2*N-6)] #S+ 6W−5,...,S+ 6W+N−6  S+ 6W+N−5,...,S+ 6W+ 2N−6
  data$Season <- s[data$Week]
  #data$Dayfactor <-  i[data$Weekday]
  for(n in 1:length(data$Weekday))data$Dayfactor[n]= i[new_map$index[new_map$real_day== data$Weekday[n]]] # fixed 19/03/2019
  data$Trend=0
  trend_week <<- object$solution[(S+1):(S+W)] - object$solution[(S+W+1):(S+2*W)]
  data$Trend[1:N]= trend_week[  as.numeric(factor(fitData$Yearweek))]
  fcIndices <- (N+1):length(data$Yearweek)
  fc_week_Indices <-  as.numeric(as.factor(data$Yearweek[fcIndices]))
  damping_terms= cumsum(damping ^(c(1:fc_week_Indices[length(fc_week_Indices)])))
 
  
  
  
  if(params$LogActuals== 1){
    season_log=data$Season
    day_log= data$Dayfactor
    trend_log=data$Trend[1:N]
    fit_log=data$Trend[1:N]+data$Season[1:N] + data$Dayfactor[1:N]
    fit_logback=exp(fit_log)-1
    season_logback=exp(season_log)
    day_logback= exp(day_log)
    trend_logback=fit_logback/(season_logback[1:N]*day_logback[1:N])
    #difference2=trend_log[N]-trend_log[N-1]
    #difference2=exp(difference2)
    difference=trend_week[W]-trend_week[W-1]
    #only forecast part
    forecast_trend= (damping_terms[fc_week_Indices] *difference +trend_logback[N]) # as numeric factor 
    forecast_logback= forecast_trend*(season_logback[fcIndices])*(day_logback[fcIndices])
    #combine the fit part and forecast part
    fit_forecast=c(fit_logback,forecast_logback)
    #extend the forecast part with 0 in the fit part and real forecast in the forecast part, give back to forecast_logback
    data$Trend[1:N]<- trend_logback
    data$Trend[fcIndices]=(damping_terms[fc_week_Indices] *difference+trend_logback[N])
    data$Season=season_logback
    data$Dayfactor= day_logback
    data$Forecast<-c(fit_logback,forecast_logback)
    
    
  }else{
    data$Trend[fcIndices] = data$Trend[N] + damping_terms[fc_week_Indices] * (object$solution[S+3*W-1]-object$solution[S+4*W-2]) #damping trend 
    data$Forecast <- data$Trend + data$Season + data$Dayfactor
  }
  #dataWeek$Trend[fcIndices] = dataWeek$Trend[N] + (fcIndices - N) * (object$solution[S+3*N-1]-object$solution[S+4*N-2])
  
  # if(params$LogActuals) {
  #   dataWeek$Forecast <- exp(dataWeek$Trend + dataWeek$Season)
  # } else {
  #   dataWeek$Forecast <- dataWeek$Trend + dataWeek$Season
  # }
  # Return data set including forecast
  data
}
#--------------------------------------------------------
# INITIALIZATION
#--------------------------------------------------------
# Set working directory
workingDirectory <- commandArgs(TRUE)[1] # Get working directory from command line input

if (is.na(workingDirectory)) {
  workingDirectory = "D:/Websites/GitHub/cc-forecast-2nd-generation/engines/forecast_algorithm"
}

setwd(workingDirectory)

# Set input parameters
paramTable <- as.matrix(read.table("input_settings.csv", sep=",", header=TRUE))
params <- as.list(paramTable[, "Value"])
names(params) <- paramTable[, "Parameter"]



# Import data
#data <- read.table("input_table.csv", header=TRUE, sep=",", quote="")
#colnames(data)[colnames(data)=="Volume"] = "Actual" # bug fix Ger 12/5/2018



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
data$Actual[which(data$Actual< 0)] = NA
# Create fit data set 
fitData <- DataInPeriod(data, params$MinDateFit, params$MaxDateFit)[,1:9] # the index cleanes if code is runned multiple time
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



# data excluding closed days
data_no_closed_days<- data[!data$Weekday %in% closed_days,]
# fitData excluding closed days
fitData_no_closed_days<- fitData[!fitData$Weekday %in% closed_days,]
#--------------------------------------------------------
# FORECAST + OUTLIER DETECTION 
#--------------------------------------------------------

outlierfactor=  pnorm(as.numeric(params[["OutlierFactor"]]))^15 # range between 8.74% and 86.01% ( adapted to old pot interface)
n_iterations<- 3
for(i in 1: n_iterations){
  data_no_closed_days <- ForecastLO(fitData_no_closed_days, data_no_closed_days, params)
  distances<- abs(data_no_closed_days$Actual[1: length( fitData_no_closed_days$Actual)]- data_no_closed_days$Forecast[1: length( fitData_no_closed_days$Actual)])
  outliers<- distances/(data_no_closed_days$Forecast[1: length( fitData_no_closed_days$Actual)]+1) > outlierfactor & distances> 3*sqrt(max(0,data_no_closed_days$Forecast[1: length( fitData_no_closed_days$Actual)]))
  
  fitData_no_closed_days <-  fitData[!fitData$Weekday %in% closed_days,] # restore original values 
  
  if(i>1){ fitData_no_closed_days$outlier<- outliers_previous_iteration }
  if(i < n_iterations){
    fitData_no_closed_days$Actual[outliers]<- NA # substitute new outliers with NA only if there is still another iteration to perform
    outliers_previous_iteration <- as.numeric(outliers) # upload column of outliers
    
    }
}
forecastData_no_closed_days <- data_no_closed_days[data_no_closed_days$Date > as.Date(params$MinDateForecast),]

####  
fitData= merge(fitData, fitData_no_closed_days, by= c(names(fitData)), all= T) 
data= data[,c(1:9)]  # clean data from previous forecasts coulumns
data= merge(data, data_no_closed_days, by= c(names(data))[1:9], all= T)  #upload data with forecast
data[is.na(data)] <- 0
fitData[is.na(fitData)]<- 0
fitData$outlier[is.na(fitData$IsHoliday)] <- 0      # do not consider outliers the holidays


#sum(fitData$outlier)/length(fitData_no_closed_days$outlier) # check if outlier detection works

# Set holiday forecast    * It was used in old Lea, here we assume that holiday are always zero
#holidayActuals <- fitData[fitData$IsHoliday == 1, "Actual"]
#if (!is.null(holidayActuals)) {
 # holidayForecast <- median(holidayActuals)
#  data$Forecast[data$IsHoliday == 1] <- holidayForecast
#}  # the forecast for the holidays is the median of previous holidays. Maybe can be improved
#--------------------------------------------------------
# Export forecast to file
#--------------------------------------------------------
outputTable <- data[, c("Date", "Trend", "Season","Dayfactor", "IsHoliday", "Forecast")] # output in old Lea format 

# creating table resembling pot output for cc_forecast # modified 17/01/2018
if (params$LogActuals == 1) {
  day_index<- seq.int(nrow(data))-1
  forecast<- data$Forecast
  data_outlier<-  c(fitData[, "outlier"], rep(0, length(data$Date)-length(fitData$Date)))
  coeff_constant<- rep(1,length(data$Date))
  coeff_trend <- data[,"Trend" ]
  coeff_power <- rep(1, length(data$Date))
  coeff_pef <- rep(1,length(data$Date))  
  coeff_season <- data[,"Season" ]/ mean( data[,"Season" ]) # we do not have monthly component
  coeff_day_of_week <- data[,"Dayfactor" ]
  coeff_holiday <- as.numeric( data$IsHoliday==0)
  coeff_scalar <- rep(1, length(data$Date))  # modified for better output
  coeff_workload  <- rep(1,length(data$Date)) *  mean( data[,"Season" ])
     
} else {
  day_index<- seq.int(nrow(data))-1
  forecast<- data$Forecast
  data_outlier<-  c(fitData[, "outlier"], rep(0, length(data$Date)-length(fitData$Date)))
  coeff_constant<- rep(1,length(data$Date))
  coeff_trend <- rep(1, length(data$Date))
  coeff_power <- rep(1, length(data$Date))
  coeff_pef <- rep(1,length(data$Date))  
  coeff_season <- rep(1, length(data$Date)) # we do not have monthly component
  coeff_day_of_week <- rep(1, length(data$Date))
  coeff_holiday <- as.numeric( data$IsHoliday==0)
  coeff_scalar <- rep(1, length(data$Date))
  coeff_workload  <- data$Forecast
}

outputTable1 <- data.frame( day_index, forecast, data_outlier, coeff_constant, coeff_trend, coeff_power, coeff_pef, coeff_season, coeff_day_of_week, coeff_holiday, coeff_scalar, coeff_workload)        

#outputTable[, c("Trend", "Season", "Dayfactor")] <- data[as.character(data$Yearweek), c("Trend", "Season", "Dayfactor")]
#outputTable$IsHoliday <- rep(1, length(outputTable[,1])) - outputTable$IsHoliday
write.table(outputTable1, file="output_table.csv", quote=FALSE, sep=",", row.names=FALSE, col.names = FALSE)


DoNotRun <- function() {
  
  plot(data$Actual,type="l")
  plot(data$Forecast,type="l")
  plot(outputTable[,length(outputTable)],type="l")
  # get fit period and cleaned actuals
  fit <- DataInPeriod(data, params$MinDateFit, params$MaxDateFit)
  cleaned_actuals <- fitData$Actual                      # zero for closed days and fit for outliers
  cleaned_actuals[fitData$outlier==1] <- fit$Forecast[fitData$outlier==1]
  matplot(fit[,c("Actual", "Forecast")], xaxt="n", type="l", lty="solid")
  matplot(fit[,c("Trend")][1:365], xaxt="n", type="l", lty="solid")
  
  # get residuals of fit
  residuals <-  cleaned_actuals / fit$Forecast
  residuals[ is.nan(residuals)] = 1
  ts.plot(residuals[1:200])
  #selectedPoints <- identify(residuals)
  #plot(restaurants_data$average_popular_time[[selectedPoints]])
  #restaurants_data$name[selectedPoints]
  # Get forecast period
  forecastData <- DataInPeriod(data, params$MinDateForecast, params$MaxDateForecast)
  WAPE(data$Actual, data$Forecast)					# WAPE week: fit + forecast
  WAPE(forecastData$Actual, forecastData$Forecast)			# WAPE day: forecast
  
  #--------------------------------------------------------
  # Normal plots
  #--------------------------------------------------------
  # Plot fit and forecast
  matplot(data[,c("Actual", "Forecast")], xaxt="n", type="l", lty="solid")
  matplot(data[,c("Actual", "Season", "Trend", "Dayfactor")], xaxt="n", type="l", lty="solid")
  
  axis(1, at= 1:length(data$Actual), labels= data$Yearweek)
  
  # Plot daily forecast
  matplot(forecastData[,c("Actual", "Forecast")], xaxt="n", type="l", lty="solid")
  matplot(forecastData[,c("Actual", "Forecast", "Trend", "Season", "Dayfactor")], xaxt="n", type="l", lty="solid")
  matplot(forecastData[,c(  "Trend")], xaxt="n", type="l", lty="solid")
  
  axis(1, at=1:length(forecastData$Yearweek), labels=forecastData$Yearweek)
  matplot(forecastData[,c("Season")], xaxt="n", type="l", lty="solid")
  
}


 # this is our original vector, which I'm just making up


