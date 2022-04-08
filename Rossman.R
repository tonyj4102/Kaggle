#read in file

trainRM=read.csv("Rossman.csv")
str(trainRM)

trainRM$DayOfWeek=NULL

# reshape as data frame

# wide <- reshape(Indometh, v.names = "conc", idvar = "Subject", timevar = "time", direction = "wide")
TrainRMW <- reshape(trainRM, v.names = "Date", idvar = "Store", timevar = "Date", direction = "wide")
str(TrainRMW)

# operate_on_data_frame

rossmanRM.ts = ts(trainRM$Sales, frequency = 356, start=c(2013,1,1), end=c(2015,7,31)) 
class(rossmanRM.ts) 
str(rossmanRM.ts)
head(rossmanRM.ts)

#(optional) convert date to day of the week for each year

#separate file by store

#run arima for each store

#predict 6 weeks daily for each store

#combine predictions for all stores

### Method B

predict by store 1

find the differences of the last and the next 6 weeks

Interpolated the difference by store based on the last value for each store

Add the interpolated difference to the last date by store


