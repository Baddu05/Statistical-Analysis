library(fpp2)
rental_price <- read.csv("C:\\Data Analytics Classes\\Statistics\\Stats project\\rentalp.csv", header = TRUE)

View(rental_price)


rental_price <- rental_price[rental_price$SER_REF=='CPIM.SE3041F',]

RP<-rental_price$DATA_VAL


rental_price_series <- ts(RP, frequency = 12, start =c(2006,11) ) #taking second quarter
str(rental_price_series)

print(rental_price_series,calendar = TRUE)
start(rental_price_series)
end(rental_price_series)
frequency(rental_price_series)


#before smoothing
plot(rental_price_series,main='RPI changes for CPIM.SE3041F',xlab='Year',ylab='Rental Price Index')


#after smoothing
plot(ma(rental_price_series,order=4),main='RPI changes for CPIM.SE3041F',xlab='Year',ylab='Rental Price Index')


#Seasonal decomposition using decompose() : additive
fit.decumlt<- decompose(rental_price_series, type ='additive')
fit.decumlt
plot(fit.decumlt,col='red')


#*****************************Forecasting****************************************

holt_model <- holt(rental_price_series,h=5)
summary(holt_model)

holt_winter <- HoltWinters(rental_price_series,seasonal = 'additive')
summary(holt_winter)

forecast1 <- forecast(holt_winter,n.ahead = 5)
accuracy(forecast1)

autoplot(holt_model)+
  autolayer(fitted(holt_model),series = "Fitted")


