# pre-process code for thesis


DataInPeriod <- function(x, startDate, endDate) {
  startIndex = which(x[, 1] == startDate)[1]
  endIndex = which(x[, 1] == endDate)[1]
  result <- x[startIndex:endIndex, ]
}

WAPE <- function(x, y) {
sum(abs(x - y)) / sum(x)
}
# Set input parameters
paramTable <- as.matrix(read.table("input_settings.csv", sep=",", header=TRUE))
params <- as.list(paramTable[, "Value"])
names(params) <- paramTable[, "Parameter"]



aggregator_ccmath_excel_LEA= function(path){
  library(readr)
  library(readxl)
  data <- read_excel(path, col_types = c("numeric", "numeric", "numeric",  "date", "text", "text", "text", "text", "numeric", "text", "text", "text"))
  data = data[,c(4,9)]
  data[is.na(data)] = 0
  data = aggregate( data$`Vol Actuals`, list(date=data$Date), sum)
  data= data.frame( Date= data$date, Actual = data$x)
  data$Date <- as.Date(data$Date)
  data$Year <- as.numeric(format(data$Date, "%G")) # Year of date: %Y
  data$Month <- as.numeric(format(data$Date, "%m"))
  data$Week <- as.numeric(format(data$Date, "%V"))
  data$Yearweek <- as.numeric(format(data$Date, "%G%V"))
  data$Weekday <- as.numeric(format(data$Date, "%u"))
  data$IsHoliday <- 0
  data$IsHoliday[data$Holiday!=""] <- 1
  data$Date <- as.Date(data$Date)
  data$Year <- as.numeric(format(data$Date, "%G")) # Year of date: %Y
  data$Month <- as.numeric(format(data$Date, "%m"))
  data$Week <- as.numeric(format(data$Date, "%V"))
  data$Yearweek <- as.numeric(format(data$Date, "%G%V"))
  data$Weekday <- as.numeric(format(data$Date, "%u"))
  data$IsHoliday <- 0
  data$IsHoliday[data$Holiday!=""] <- 1
  data$Actual[which(data$Actual< 0)] = NA
  data$Actual[which(data$Actual< 0)] = NA
  # Create fit data set 
  
  firstObservedDate <- data$Date[1]
  if (as.Date(firstObservedDate) > as.Date(params$MinDateFit)) {
    params$MinDateFit <<- firstObservedDate
  }
  lastObservedDate <- data$Date[length(data$Date)]
  if (as.Date(params$MaxDateFit) > as.Date(lastObservedDate)) {
    params$MaxDateFit <<- lastObservedDate
  }
  if (as.Date(params$MinDateFit) > as.Date(params$MaxDateFit)) {
    params$MaxDateFit <<- params$MinDateFit
  }
  data
  }

## data is now suitable for the algorithm
path= rep(0,7)

# thesis scenarios # import.....
path[1] ='/home/giuseppe/Desktop/Thesis/Delta data/Energie.xls'
path[2] ='/home/giuseppe/Desktop/Thesis/Delta data/Retentiedesk.xls' 
path[3] ='/home/giuseppe/Desktop/Thesis/Delta data/Salesteam energie.xls'    ## BEST SCENARIO TO SHOW POWER OF RECURRENT NEURAL NETWORK
path[4] ='/home/giuseppe/Desktop/Thesis/Delta data/Salesteam telecom.xls' 
path[5] ='/home/giuseppe/Desktop/Thesis/Delta data/Telecom service.xls' 
path[6] ='/home/giuseppe/Desktop/Thesis/Delta data/Telecom techniek.xls' 
path[7] = '/home/giuseppe/Desktop/Leon/cz.xls'  #small skill experiment



data =  aggregator_ccmath_excel_LEA(path[3])
data$Actual[ data$Actual<10]=0
fitData <- DataInPeriod(data, params$MinDateFit, params$MaxDateFit)
forecastData <-  DataInPeriod(data, params$MinDateForecast, params$MaxDateForecast)
#as.Date(as.numeric(params$MinDateFit)/2)

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
forecastData_no_closed_days<- forecastData[!forecastData$Weekday %in% closed_days,]


library(ggplot2)
ggplot(data, aes(x= Date, Actual)) + geom_line() +
 scale_x_date(date_labels = "%b-%Y")+ xlab("") + ylab("Volume of Inbound calls") + ggtitle( "Product Energie") 

# plot

