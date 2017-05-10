#STAT 261
#Final Project
#Central England Temperatures from 1659-2016


#setwd() # Set path
set.seed(42)

library(astsa)


## Data import and cleaning, define variables ##
temp = read.table("temp.txt", header=T) #import data
temp[temp == -99.9] = NA #change default NA value

#Quarterly Temperatures
year.qtr = seq(1659.25, 2016, 0.25)
temp.qtr = as.vector(t(as.matrix(temp[,2:5])))
temp.qtr = temp.qtr[2:(length(temp.qtr)-3)] #remove NA

#Yearly Average Temperatures
year.year = temp$Year[2:(length(temp$Year)-1)]
temp.year.avg = rowMeans(temp[,2:5])
temp.year.avg = temp.year.avg[2:(length(temp.year.avg)-1)]

#Periodogram
temp.qtr.per = spec.pgram(temp.qtr, taper=0, log="n")
temp.year.avg.per = spec.pgram(temp.year.avg, taper=0, log="n")

#ACF, PACF
acf2(temp.year.avg, max.lag = 100)

#Regression
reg = ar.ols(temp.year.avg, order=100)
plot(year.year, reg$resid, main="Residual Plot")

reg_predict = function(n){
  regr = ar.ols(temp.year.avg[1:(length(temp.year.avg)-n)], order=100)
  fore = predict(regr, n.ahead = n)
  start = length(year.year)-n+1
  end = length(year.year)
  plot(year.year[start:end], temp.year.avg[start:end], type="l", main=paste("Forecasting Ahead by ",n),
       ylab="Temp", xlab="Year")
  lines(year.year[start:end], fore$pred, lty="dashed")
}

reg_predict(5)
reg_predict(15)
reg_predict(50)


regr = ar.ols(temp.year.avg[1:300], order=100)
fore = predict(regr, n.ahead=50)
plot(year.year[301:350], temp.year.avg[301:350], type="l")
lines(year.year[301:350], fore$pred, lty="dashed")

#Smoothing
plot(year.year, temp.year.avg,ylab="Temperature", xlab="Year", main="Central England Temperature (Yearly)")
lines(ksmooth(year.year, temp.year.avg, "normal", bandwidth=10))

plot(year.year[330:356], temp.year.avg[330:356],ylab="Temperature", xlab="Year", main="Central England Temperature (Yearly)")
lines(ksmooth(year.year[330:356], temp.year.avg[330:356], "normal", bandwidth=3))



## Other Figures
plot(year.qtr, temp.qtr, type="l", ylab="Temperature", xlab="Year", main="Central England Temperature (raw)")
plot(year.year, temp.year.avg, type="l", ylab="Temperature", xlab="Year", main="CET Yearly Averages")


regr = ar.ols(temp.year.avg, order=100)


fore = predict(regr, n.ahead=50)
plot(year.year[301:350], temp.year.avg[301:350], type="l")
lines(year.year[301:350], fore$pred, lty="dashed")


