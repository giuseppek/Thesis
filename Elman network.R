install.packages("RSNNS", lib= "/home/giuseppe/anaconda3/envs/rstudio/lib/R/library",  repos="http://cran.cnr.berkeley.edu" )

library(RSNNS)

#
# simulate an arima time series example of the length n
#
set.seed(10001)
n <- 100
ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = n-1)
ts.sim<- scale(cleaned_actuals)

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
X= cbind(X,fitData$Weekday[2:99])
Y <- sapply(sw:(n-2),
            function(ind){
              ts.sim[ind+1]
            })

# used to validate prediction properties
# on the last point of the series
newX <- ts.sim[(n-sw):(n-1)]
newY <- ts.sim[n]

# build an elman network based on the input
model <- elman(X[,2], Y,
               size = c(2),
               learnFuncParams = c(0.01),
               maxit = 5000,
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
points(length(Y)+1, predict(model, newdata=newX),
       pch="X", col="green")
ts.plot(predict(model, newdata=matrix(rep(c(1:7),10))))
plot

round( model$fitted.values, 2)
