library(RMySQL)
library(dplyr)
library(lubridate) #To deal with TimeSeries
library(rstudioapi) #To set manually WD, but does not work
library(zoo) #As yearmon
library(tidyverse)  
library(forecast)
library(fpp)
library(urca)
library(lmtest)

#uploat weather forecast
weather_daily <- readRDS("~/M3T1/weather_daily.RDS")

#read dataser created Task 1.1.
EnergyConsumption <- readRDS("~/M3T1/EnergyConsumptionClean_3years.RDS")
TrainingEnergyConsumption <- dplyr::filter(EnergyConsumption, 
                                   Year =="2007" | Year =="2008" | Year =="2009")

TestEnergyConsumption <-  dplyr::filter(EnergyConsumption, 
                                        Year =="2010")

CleanEnergyConsumption <- dplyr::filter(EnergyConsumption, 
                                        Year !="2006")

Group_Month <-CleanEnergyConsumption %>% group_by(Date) %>%
  summarise(
    Sum_GAP=sum(GAP),
    Sum_Others=sum(Others),
    Sum_Kitchen=sum(Kitchen),
    Sum_Laundry=sum(Laundry),
    Sum_Heater_AC=sum(Heater_AC)
  )



Group_Month_Training <- TrainingEnergyConsumption %>% group_by(Date) %>%
  summarise(
    Sum_GAP=sum(GAP),
    Sum_Others=sum(Others),
    Sum_Kitchen=sum(Kitchen),
    Sum_Laundry=sum(Laundry),
    Sum_Heater_AC=sum(Heater_AC)
  )

Group_Month_Test<- TestEnergyConsumption %>% group_by(Date) %>%
  summarise(
    Sum_GAP=sum(GAP),
    Sum_Others=sum(Others),
    Sum_Kitchen=sum(Kitchen),
    Sum_Laundry=sum(Laundry),
    Sum_Heater_AC=sum(Heater_AC)
  )





weather_daily$time <- as.POSIXct(weather_daily$time, "%Y/%m/%d")
weather_daily$year<- year(weather_daily$time)
weather_daily$month<- month(weather_daily$time)


weather_monthly <- weather_daily %>% group_by(year,month) %>%
  summarise(
    avgtemp=mean(temperatureLow)
  )


ts_Month_AC_training <- ts(Group_Month_Training$Sum_Heater_AC, frequency=12, start=c(2007,1) )

#decompose
componentsForecastMonthAC <- decompose(ts_Month_AC_training)
plot(componentsForecastMonthAC)

#BETA SEASONALITY , GAMMA FALSE
HWModel_tsMonth_AC<- HoltWinters(ts_Month_AC_training)
autoplot(HWModel_tsMonth_AC)
forecastMonthHW <- forecast(HWModel_tsMonth_AC, h=11)
autoplot(forecastMonthHW)
ts_Month_AC_test <- ts(Group_Month_Test$Sum_Heater_AC, start=c(2010,1), frequency = 12)
x <- as.data.frame(t(accuracy(forecastMonthHW, ts_Month_AC_test)))
accuracytab <- NA
accuracytab$HW <- x$`Test set`


#AUTOARIMA
ggtsdisplay(ts_Month_AC)
autoplot(ts_Month_AC)
ur.kpss(ts_Month_AC)
ndiffs(ts_Month_AC)
nsdiffs(ts_Month_AC)

#AUTOARIMA####
#splitting
fm_autoarima_month_ac <- auto.arima(ts_Month_AC_training, trace=TRUE)
fc_autoarima_month_ac <- forecast(fm_autoarima_month_ac, h=11)
x <- as.data.frame(t(accuracy(fc_autoarima_month_ac,ts_Month_AC_test)))
accuracytab$AUTOARIMA <- x$`Test set`

#crossvalidation

fm_autoarima_month_ac_cv <- function(x,h){
  forecast(auto.arima(x), h=h)} 

error_fm_autoarima_month_ac_cv<- tsCV(ts_Month_AC_training,fm_autoarima_month_ac_cv, h=1)
sqrt(mean(error_fm_autoarima_month_ac_cv^2, na.rm=TRUE))/1000
accuracy(ts_Month_AC_training- error_fm_autoarima_month_ac_cv,ts_Month_AC) #accuracy training test

#SARIMA MANUAL####

fm_manualarima_month_ac <- Arima(ts_Month_AC_training, order=c(1,0,1),seasonal = list(order = c(1,1,0), period = 12),method="ML")
fc_manualarima_month_acforecast <-forecast(fm_manualarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_manualarima_month_acforecast, ts_Month_AC_test)))
accuracytab$ManualARIMA <- x$`Test set`

#AUTOARIMAX####
#Weather Forecast
ts_weather <- ts(weather_monthly$avgtemp, start=c(2007,1), frequency=12)
wf_autoarima <- auto.arima(ts_weather, trace=TRUE)
fc_weather2011 <- forecast(wf_autoarima,11)
autoplot(fc_weather2011)

#MODEL auto SARMIAX
fm_sarimax_ac <-auto.arima(ts_Month_AC_training, xreg=weather_monthly$avgtemp)
fm_sarimax_ac$x
fc_sarimax_ac <- forecast(fm_sarimax_ac, h=11, xreg=fc_weather2011$mean)
plot(fc_sarimax_ac)


x <- as.data.frame(t(accuracy(fc_sarimax_ac,ts_Month_AC_test)))
accuracytab$AutoSarimaX<- x$`Test set`

checkresiduals(fm_sarimax_ac)
#MODEL TUNED SARIMAX
fm_manualSarimax_month_ac <- Arima(ts_Month_AC_training, order=c(1,0,1),seasonal = list(order = c(1,1,0), period = 12),method="ML", xreg=weather_monthly$avgtemp)
fc_manualSarimax_ac <- forecast(fm_manualSarimax_month_ac, h=11, xreg=fc_weather2011$mean)
accuracy(fc_manualSarimax_ac,ts_Month_AC_test)
checkresiduals(fm_manualSarimax_month_ac)

x <- as.data.frame(t(accuracy(fc_manualSarimax_ac,ts_Month_AC_test)))
accuracytab$TunedSarimaX<- x$`Test set`


accuracytab <- as.data.frame(accuracytab)
accuracytab <- t(accuracytab)
accuracytab <- accuracytab[2:6,]
colnames(accuracytab) <- c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")


write.csv(accuracytab,'/Users/alessandro/M3T1/accuracytabAC.csv', row.names = TRUE)
#EVERYTHING WITHOUT OUTLIERS####
tsoutliers(ts_Month_AC_training)
ts_Month_AC_training[20]<-205301.5
ts_Month_AC_training[8]<-138199.6

#BETA SEASONALITY , GAMMA FALSE
HWModel_tsMonth_AC<- HoltWinters(ts_Month_AC_training)
autoplot(HWModel_tsMonth_AC)
forecastMonthHW <- forecast(HWModel_tsMonth_AC, h=11)
autoplot(forecastMonthHW)
ts_Month_AC_test <- ts(Group_Month_Test$Sum_Heater_AC, start=c(2010,1), frequency = 12)

x <- as.data.frame(t(accuracy(forecastMonthHW, ts_Month_AC_test)))
accuracytab2 <- NA
accuracytab2$HW_NO <- x$`Test set`

#AUTOARIMA
ggtsdisplay(ts_Month_AC)
autoplot(ts_Month_AC)
ur.kpss(ts_Month_AC)
ndiffs(ts_Month_AC)
nsdiffs(ts_Month_AC)

#AUTOARIMA####
#splitting
fm_autoarima_month_ac <- auto.arima(ts_Month_AC_training, trace=TRUE)
fc_autoarima_month_ac <- forecast(fm_autoarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_autoarima_month_ac,ts_Month_AC_test)))
accuracytab2$AUTOARIMA <- x$`Test set`


#crossvalidation

fm_autoarima_month_ac_cv <- function(x,h){
  forecast(auto.arima(x), h=h)} 

error_fm_autoarima_month_ac_cv<- tsCV(ts_Month_AC_training,fm_autoarima_month_ac_cv, h=1)
sqrt(mean(error_fm_autoarima_month_ac_cv^2, na.rm=TRUE))/1000
accuracy(ts_Month_AC_training- error_fm_autoarima_month_ac_cv,ts_Month_AC) #accuracy training test

#SARIMA MANUAL####

fm_manualarima_month_ac <- Arima(ts_Month_AC_training, order=c(1,0,1),seasonal = list(order = c(1,1,0), period = 12),method="ML")
fc_manualarima_month_acforecast <-forecast(fm_manualarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_manualarima_month_acforecast, ts_Month_AC_test)))
accuracytab2$TUNEDARIMA <- x$`Test set`

#AUTOARIMAX####
#Weather Forecast
ts_weather <- ts(weather_monthly$avgtemp, start=c(2007,1), frequency=12)
wf_autoarima <- auto.arima(ts_weather, trace=TRUE)
fc_weather2011 <- forecast(wf_autoarima,11)
autoplot(fc_weather2011)

#MODEL auto SARMIAX
fm_sarimax_ac <-auto.arima(ts_Month_AC_training, xreg=weather_monthly$avgtemp)
fm_sarimax_ac$x
fc_sarimax_ac <- forecast(fm_sarimax_ac, h=11, xreg=fc_weather2011$mean)
plot(fc_sarimax_ac)

x <- as.data.frame(t(accuracy(fc_sarimax_ac,ts_Month_AC_test)))
accuracytab2$AutoSARIMAx <- x$`Test set`

checkresiduals(fm_sarimax_ac)
#MODEL TUNED SARIMAX
fm_manualSarimax_month_ac <- Arima(ts_Month_AC_training, order=c(1,0,1),seasonal = list(order = c(1,1,0), period = 12),method="ML", xreg=weather_monthly$avgtemp)
fc_manualSarimax_ac <- forecast(fm_manualSarimax_month_ac, h=11, xreg=fc_weather2011$mean)

checkresiduals(fm_manualSarimax_month_ac)

x <- as.data.frame(t(accuracy(fc_manualSarimax_ac,ts_Month_AC_test)))
accuracytab2$manualSARIMAx <- x$`Test set`

accuracytab2 <- as.data.frame(accuracytab2)
accuracytab2 <- t(accuracytab2)
accuracytab2 <- accuracytab2[2:6,]
colnames(accuracytab2) <- c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")

write.csv(accuracytab2,'/Users/alessandro/M3T1/accuracytabWOAC.csv', row.names = TRUE)


externalFactor <- data.frame(matrix(NA, nrow=36))
externalFactor$august <- c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
externalFactor$weather <- weather_monthly$avgtemp
externalFactor <- externalFactor[,-1]
externalFactor <- ts(externalFactor, frequency=12, start=c(2007,01))

externalFactorForecast <- data.frame(matrix(NA, nrow=11))
externalFactorForecast$august <- c(0,0,0,0,0,0,0,1,0,0,0)
externalFactorForecast$weather <- fc_weather2011$mean
externalFactorForecast <- externalFactorForecast[,-1]
externalFactorForecast <- as.matrix(externalFactorForecast)



fm_manualSarimax_month_temp_august_ac <- auto.arima(ts_Month_AC_training, xreg=externalFactor[,c("august","weather")])
fc_manualSarimax_month_temp_august_ac <- forecast(fm_manualSarimax_month_temp_august_ac, h=11, xreg=externalFactorForecast[,c("august","weather")])
accuracy(fc_manualSarimax_month_temp_august_ac, ts_Month_AC)


#ANOTHER SUBMETERS####

ts_Month_AC_training <- ts(Group_Month_Training$Sum_Kitchen, frequency=12, start=c(2007,1) )

#decompose
componentsForecastMonthAC <- decompose(ts_Month_AC_training)
plot(componentsForecastMonthAC)
ggtsdisplay(ts_Month_AC_training, lag.max = 24)

autoplot(componentsForecastMonthAC)

ggtsdisplay(componentsForecastMonthAC$seasonal)




#EVERYTHING WITHOUT OUTLIERS####
tsoutliers(ts_Month_AC_training)

ts_Month_AC_training[8]<- 138199.6 
#BETA SEASONALITY , GAMMA FALSE
HWModel_tsMonth_AC<- HoltWinters(ts_Month_AC_training)
autoplot(HWModel_tsMonth_AC)
forecastMonthHW <- forecast(HWModel_tsMonth_AC, h=11)
autoplot(forecastMonthHW)
ts_Month_AC_test <- ts(Group_Month_Test$Sum_Kitchen, start=c(2010,1), frequency = 12)

x <- as.data.frame(t(accuracy(forecastMonthHW, ts_Month_AC_test)))
accuracytab2 <- NA
accuracytab2$HW_NO <- x$`Test set`

#AUTOARIMA



autoplot(ts_Month_AC)
ur.kpss(ts_Month_AC, test="kpss")
ndiffs(ts_Month_AC, test="kpss")
summary(ndiffs(ts_Month_AC))
nsdiffs(ts_Month_AC)

#AUTOARIMA####
#splitting
fm_autoarima_month_ac <- auto.arima(ts_Month_AC_training, trace=TRUE)

#nameofthemodel <- auto.arima(timeseries)
fc_autoarima_month_ac <- forecast(fm_autoarima_month_ac, h=11)
#nameoftheforecast <- forecast(nameofthemodel, h=11)



autoplot(fc_autoarima_month_ac)
finalprediction <- fc_autoarima_month_ac$mean

x <- as.data.frame(t(accuracy(fc_autoarima_month_ac,ts_Month_AC_test)))
accuracytab2$AUTOARIMA <- x$`Test set`


#crossvalidation

fm_autoarima_month_ac_cv <- function(x,h){
  forecast(auto.arima(x), h=h)} 

error_fm_autoarima_month_ac_cv<- tsCV(ts_Month_AC_training,fm_autoarima_month_ac_cv, h=1)
sqrt(mean(error_fm_autoarima_month_ac_cv^2, na.rm=TRUE))/1000
accuracy(ts_Month_AC_training- error_fm_autoarima_month_ac_cv,ts_Month_AC) #accuracy training test

#SARIMA MANUAL####

fm_manualarima_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(0,0,1), period = 12),method="ML")
fc_manualarima_month_acforecast <-forecast(fm_manualarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_manualarima_month_acforecast, ts_Month_AC_test)))
accuracytab2$TUNEDARIMA <- x$`Test set`

#AUTOARIMAX####
#Weather Forecast
ts_weather <- ts(weather_monthly$avgtemp, start=c(2007,1), frequency=12)
wf_autoarima <- auto.arima(ts_weather, trace=TRUE)
fc_weather2011 <- forecast(wf_autoarima,11)
autoplot(fc_weather2011)

#MODEL auto SARMIAX
fm_sarimax_ac <-auto.arima(ts_Month_AC_training, xreg=weather_monthly$avgtemp)
fm_sarimax_ac$x
fc_sarimax_ac <- forecast(fm_sarimax_ac, h=11, xreg=fc_weather2011$mean)
plot(fc_sarimax_ac)
checkresiduals(fm_sarimax_ac)


x <- as.data.frame(t(accuracy(fc_sarimax_ac,ts_Month_AC_test)))
accuracytab2$AutoSARIMAx <- x$`Test set`

checkresiduals(fm_sarimax_ac)
#MODEL TUNED SARIMAX
fm_manualSarimax_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(1,0,1), period = 12),method="ML", xreg=weather_monthly$avgtemp)
fc_manualSarimax_ac <- forecast(fm_manualSarimax_month_ac, h=11, xreg=fc_weather2011$mean)

accuracy(fc_manualSarimax_ac,ts_Month_AC_test)
checkresiduals(fm_manualSarimax_month_ac)

x <- as.data.frame(t(accuracy(fc_manualSarimax_ac,ts_Month_AC_test)))
accuracytab2$manualSARIMAx <- x$`Test set`

accuracytab2 <- as.data.frame(accuracytab2)
accuracytab2 <- t(accuracytab2)
accuracytab2 <- accuracytab2[2:6,]
colnames(accuracytab2) <- c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")

write.csv(accuracytab2,'/Users/alessandro/M3T1/kitchen.csv', row.names = TRUE)

#ANOTHER SUBMETERS####
ts_Month_AC_training <- ts(Group_Month_Training$, frequency=12, start=c(2007,1) )

#decompose
componentsForecastMonthAC <- decompose(ts_Month_AC_training)
plot(componentsForecastMonthAC)
ggtsdisplay(ts_Month_AC_training, lag.max = 24)

autoplot(componentsForecastMonthAC)

ggtsdisplay(componentsForecastMonthAC$seasonal)






#BETA SEASONALITY , GAMMA FALSE
HWModel_tsMonth_AC<- HoltWinters(ts_Month_AC_training)
autoplot(HWModel_tsMonth_AC)
forecastMonthHW <- forecast(HWModel_tsMonth_AC, h=11)
autoplot(forecastMonthHW)
ts_Month_AC_test <- ts(Group_Month_Test$Sum_Kitchen, start=c(2010,1), frequency = 12)
accuracy(forecastMonthHW, ts_Month_AC_test)


x <- as.data.frame(t(accuracy(forecastMonthHW, ts_Month_AC_test)))
accuracytab2 <- NA
accuracytab2$HW_NO <- x$`Test set`

#AUTOARIMA



autoplot(ts_Month_AC)
ur.kpss(ts_Month_AC)
ndiffs(ts_Month_AC_training) 
nsdiffs(ts_Month_AC)

#AUTOARIMA
#splitting
fm_autoarima_month_ac <- auto.arima(ts_Month_AC_training, trace=TRUE)
fc_autoarima_month_ac <- forecast(fm_autoarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_autoarima_month_ac,ts_Month_AC_test)))
accuracytab2$AUTOARIMA <- x$`Test set`


#crossvalidation

fm_autoarima_month_ac_cv <- function(x,h){
  forecast(auto.arima(x), h=h)} 

error_fm_autoarima_month_ac_cv<- tsCV(ts_Month_AC_training,fm_autoarima_month_ac_cv, h=1)
sqrt(mean(error_fm_autoarima_month_ac_cv^2, na.rm=TRUE))/1000
accuracy(ts_Month_AC_training- error_fm_autoarima_month_ac_cv,ts_Month_AC) #accuracy training test

#SARIMA MANUAL

fm_manualarima_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(0,0,1), period = 12),method="ML")
fc_manualarima_month_acforecast <-forecast(fm_manualarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_manualarima_month_acforecast, ts_Month_AC_test)))
accuracytab2$TUNEDARIMA <- x$`Test set`

#AUTOARIMAX
#Weather Forecast
ts_weather <- ts(weather_monthly$avgtemp, start=c(2007,1), frequency=12)
wf_autoarima <- auto.arima(ts_weather, trace=TRUE)
fc_weather2011 <- forecast(wf_autoarima,11)
autoplot(fc_weather2011)

#MODEL auto SARMIAX
fm_sarimax_ac <-auto.arima(ts_Month_AC_training, xreg=weather_monthly$avgtemp)
fm_sarimax_ac$x
fc_sarimax_ac <- forecast(fm_sarimax_ac, h=11, xreg=fc_weather2011$mean)
plot(fc_sarimax_ac)
checkresiduals(fm_sarimax_ac)


x <- as.data.frame(t(accuracy(fc_sarimax_ac,ts_Month_AC_test)))
accuracytab2$AutoSARIMAx <- x$`Test set`

checkresiduals(fm_sarimax_ac)
#MODEL TUNED SARIMAX
fm_manualSarimax_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(1,0,1), period = 12),method="ML", xreg=weather_monthly$avgtemp)
fc_manualSarimax_ac <- forecast(fm_manualSarimax_month_ac, h=11, xreg=fc_weather2011$mean)

accuracy(fc_manualSarimax_ac,ts_Month_AC_test)
checkresiduals(fm_manualSarimax_month_ac)

x <- as.data.frame(t(accuracy(fc_manualSarimax_ac,ts_Month_AC_test)))
accuracytab2$manualSARIMAx <- x$`Test set`

accuracytab2 <- as.data.frame(accuracytab2)
accuracytab2 <- t(accuracytab2)
accuracytab2 <- accuracytab2[2:6,]
colnames(accuracytab2) <- c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")





#ANOTHER SUBMETERS####
ts_Month_AC_training <- ts(Group_Month_Training$Sum_Laundry, frequency=12, start=c(2007,1) )

#decompose
componentsForecastMonthAC <- decompose(ts_Month_AC_training)
plot(componentsForecastMonthAC)
ggtsdisplay(ts_Month_AC_training, lag.max = 24)

autoplot(componentsForecastMonthAC)

ggtsdisplay(componentsForecastMonthAC$seasonal)






#BETA SEASONALITY , GAMMA FALSE
HWModel_tsMonth_AC<- HoltWinters(ts_Month_AC_training)
autoplot(HWModel_tsMonth_AC)
forecastMonthHW <- forecast(HWModel_tsMonth_AC, h=11)
autoplot(forecastMonthHW)
ts_Month_AC_test <- ts(Group_Month_Test$Sum_Laundry, start=c(2010,1), frequency = 12)
accuracy(forecastMonthHW, ts_Month_AC_test)


x <- as.data.frame(t(accuracy(forecastMonthHW, ts_Month_AC_test)))
accuracytab2 <- NA
accuracytab2$HW_NO <- x$`Test set`

#AUTOARIMA



autoplot(ts_Month_AC)
ur.kpss(ts_Month_AC)
ndiffs(ts_Month_AC_training) 
nsdiffs(ts_Month_AC)

#AUTOARIMA
#splitting
fm_autoarima_month_ac <- auto.arima(ts_Month_AC_training, trace=TRUE)
fc_autoarima_month_ac <- forecast(fm_autoarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_autoarima_month_ac,ts_Month_AC_test)))
accuracytab2$AUTOARIMA <- x$`Test set`


#crossvalidation

fm_autoarima_month_ac_cv <- function(x,h){
  forecast(auto.arima(x), h=h)} 

error_fm_autoarima_month_ac_cv<- tsCV(ts_Month_AC_training,fm_autoarima_month_ac_cv, h=1)
sqrt(mean(error_fm_autoarima_month_ac_cv^2, na.rm=TRUE))/1000
accuracy(ts_Month_AC_training- error_fm_autoarima_month_ac_cv,ts_Month_AC) #accuracy training test

#SARIMA MANUAL

fm_manualarima_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(0,0,1), period = 12),method="ML")
fc_manualarima_month_acforecast <-forecast(fm_manualarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_manualarima_month_acforecast, ts_Month_AC_test)))
accuracytab2$TUNEDARIMA <- x$`Test set`

#AUTOARIMAX
#Weather Forecast
ts_weather <- ts(weather_monthly$avgtemp, start=c(2007,1), frequency=12)
wf_autoarima <- auto.arima(ts_weather, trace=TRUE)
fc_weather2011 <- forecast(wf_autoarima,11)
autoplot(fc_weather2011)

#MODEL auto SARMIAX
fm_sarimax_ac <-auto.arima(ts_Month_AC_training, xreg=weather_monthly$avgtemp)
fm_sarimax_ac$x
fc_sarimax_ac <- forecast(fm_sarimax_ac, h=11, xreg=fc_weather2011$mean)
plot(fc_sarimax_ac)
checkresiduals(fm_sarimax_ac)


x <- as.data.frame(t(accuracy(fc_sarimax_ac,ts_Month_AC_test)))
accuracytab2$AutoSARIMAx <- x$`Test set`

checkresiduals(fm_sarimax_ac)
#MODEL TUNED SARIMAX
fm_manualSarimax_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(1,0,1), period = 12),method="ML", xreg=weather_monthly$avgtemp)
fc_manualSarimax_ac <- forecast(fm_manualSarimax_month_ac, h=11, xreg=fc_weather2011$mean)

accuracy(fc_manualSarimax_ac,ts_Month_AC_test)
checkresiduals(fm_manualSarimax_month_ac)

x <- as.data.frame(t(accuracy(fc_manualSarimax_ac,ts_Month_AC_test)))
accuracytab2$manualSARIMAx <- x$`Test set`

accuracytab2 <- as.data.frame(accuracytab2)
accuracytab2 <- t(accuracytab2)
accuracytab2 <- accuracytab2[2:6,]
colnames(accuracytab2) <- c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")



write.csv(accuracytab2,'/Users/alessandro/M3T1/laundry.csv', row.names = TRUE)


#ANOTHER SUBMETERS####
ts_Month_AC_training <- ts(Group_Month_Training$Sum_Kitchen, frequency=12, start=c(2007,1) )

#decompose
componentsForecastMonthAC <- decompose(ts_Month_AC_training)
plot(componentsForecastMonthAC)
ggtsdisplay(ts_Month_AC_training, lag.max = 24)

autoplot(componentsForecastMonthAC)

ggtsdisplay(componentsForecastMonthAC$seasonal)






#BETA SEASONALITY , GAMMA FALSE
HWModel_tsMonth_AC<- HoltWinters(ts_Month_AC_training)
autoplot(HWModel_tsMonth_AC)
forecastMonthHW <- forecast(HWModel_tsMonth_AC, h=11)
autoplot(forecastMonthHW)
ts_Month_AC_test <- ts(Group_Month_Test$Sum_Kitchen, start=c(2010,1), frequency = 12)
accuracy(forecastMonthHW, ts_Month_AC_test)


x <- as.data.frame(t(accuracy(forecastMonthHW, ts_Month_AC_test)))
accuracytab2 <- NA
accuracytab2$HW_NO <- x$`Test set`

#AUTOARIMA



autoplot(ts_Month_AC)
ur.kpss(ts_Month_AC)
ndiffs(ts_Month_AC_training) 
nsdiffs(ts_Month_AC)

#AUTOARIMA
#splitting
fm_autoarima_month_ac <- auto.arima(ts_Month_AC_training, trace=TRUE)
fc_autoarima_month_ac <- forecast(fm_autoarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_autoarima_month_ac,ts_Month_AC_test)))
accuracytab2$AUTOARIMA <- x$`Test set`


#crossvalidation

fm_autoarima_month_ac_cv <- function(x,h){
  forecast(auto.arima(x), h=h)} 

error_fm_autoarima_month_ac_cv<- tsCV(ts_Month_AC_training,fm_autoarima_month_ac_cv, h=1)
sqrt(mean(error_fm_autoarima_month_ac_cv^2, na.rm=TRUE))/1000
accuracy(ts_Month_AC_training- error_fm_autoarima_month_ac_cv,ts_Month_AC) #accuracy training test

#SARIMA MANUAL

fm_manualarima_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(0,0,1), period = 12),method="ML")
fc_manualarima_month_acforecast <-forecast(fm_manualarima_month_ac, h=11)

x <- as.data.frame(t(accuracy(fc_manualarima_month_acforecast, ts_Month_AC_test)))
accuracytab2$TUNEDARIMA <- x$`Test set`

#AUTOARIMAX
#Weather Forecast
ts_weather <- ts(weather_monthly$avgtemp, start=c(2007,1), frequency=12)
wf_autoarima <- auto.arima(ts_weather, trace=TRUE)
fc_weather2011 <- forecast(wf_autoarima,11)
autoplot(fc_weather2011)

#MODEL auto SARMIAX
fm_sarimax_ac <-auto.arima(ts_Month_AC_training, xreg=weather_monthly$avgtemp)
fm_sarimax_ac$x
fc_sarimax_ac <- forecast(fm_sarimax_ac, h=11, xreg=fc_weather2011$mean)
plot(fc_sarimax_ac)
checkresiduals(fm_sarimax_ac)


x <- as.data.frame(t(accuracy(fc_sarimax_ac,ts_Month_AC_test)))
accuracytab2$AutoSARIMAx <- x$`Test set`

checkresiduals(fm_sarimax_ac)
#MODEL TUNED SARIMAX
fm_manualSarimax_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(1,0,1), period = 12),method="ML", xreg=weather_monthly$avgtemp)
fc_manualSarimax_ac <- forecast(fm_manualSarimax_month_ac, h=11, xreg=fc_weather2011$mean)

accuracy(fc_manualSarimax_ac,ts_Month_AC_test)
checkresiduals(fm_manualSarimax_month_ac)

x <- as.data.frame(t(accuracy(fc_manualSarimax_ac,ts_Month_AC_test)))
accuracytab2$manualSARIMAx <- x$`Test set`

accuracytab2 <- as.data.frame(accuracytab2)
accuracytab2 <- t(accuracytab2)
accuracytab2 <- accuracytab2[2:6,]
colnames(accuracytab2) <- c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")





#ANOTHER SUBMETERS####
ts_Month_AC_training <- ts(Group_Month_Training$Sum_GAP, frequency=12, start=c(2007,1) )

#decompose
componentsForecastMonthAC <- decompose(ts_Month_AC_training)
plot(componentsForecastMonthAC)
ggtsdisplay(ts_Month_AC_training, lag.max = 24)

autoplot(componentsForecastMonthAC)

ggtsdisplay(componentsForecastMonthAC$seasonal)






#BETA SEASONALITY , GAMMA FALSE
HWModel_tsMonth_AC<- HoltWinters(ts_Month_AC_training)
autoplot(HWModel_tsMonth_AC)
forecastMonthHW <- forecast(HWModel_tsMonth_AC, h=11)
autoplot(forecastMonthHW)
ts_Month_AC_test <- ts(Group_Month_Test$Sum_GAP, start=c(2010,1), frequency = 12)
accuracy(forecastMonthHW, ts_Month_AC_test)


x <- as.data.frame(t(accuracy(forecastMonthHW, ts_Month_AC_test)))
accuracytab2 <- NA
accuracytab2$HW_NO <- x$`Test set`

#AUTOARIMA



autoplot(ts_Month_AC)
ur.kpss(Group_Month$Sum_Heater_AC)
ur.kpss(ts_Month_AC_training)


ndiffs(ts_Month_AC_training) 
nsdiffs(ts_Month_AC)

#AUTOARIMA
#splitting
fm_autoarima_month_ac <- auto.arima(ts_Month_AC_training, trace=TRUE)
fc_autoarima_month_ac <- forecast(fm_autoarima_month_ac, h=11)
Box.test(fm_autoarima_month_ac$residuals, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

x <- as.data.frame(t(accuracy(fc_autoarima_month_ac,ts_Month_AC_test)))
accuracytab2$AUTOARIMA <- x$`Test set`


#crossvalidation

fm_autoarima_month_ac_cv <- function(x,h){
  forecast(auto.arima(x), h=h)} 

error_fm_autoarima_month_ac_cv<- tsCV(ts_Month_AC_training,fm_autoarima_month_ac_cv, h=1)
sqrt(mean(error_fm_autoarima_month_ac_cv^2, na.rm=TRUE))/1000
accuracy(ts_Month_AC_training- error_fm_autoarima_month_ac_cv,ts_Month_AC) #accuracy training test

#SARIMA MANUAL

fm_manualarima_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(0,0,1), period = 12),method="ML")
fc_manualarima_month_acforecast <-forecast(fm_manualarima_month_ac, h=11)



x <- as.data.frame(t(accuracy(fc_manualarima_month_acforecast, ts_Month_AC_test)))
accuracytab2$TUNEDARIMA <- x$`Test set`

#AUTOARIMAX
#Weather Forecast
ts_weather <- ts(weather_monthly$avgtemp, start=c(2007,1), frequency=12)
wf_autoarima <- auto.arima(ts_weather, trace=TRUE)
fc_weather2011 <- forecast(wf_autoarima,11)
autoplot(fc_weather2011)

#MODEL auto SARMIAX
fm_sarimax_ac <-auto.arima(ts_Month_AC_training, xreg=weather_monthly$avgtemp)
fm_sarimax_ac$x
fc_sarimax_ac <- forecast(fm_sarimax_ac, h=11, xreg=fc_weather2011$mean)
plot(fc_sarimax_ac)
checkresiduals(fm_sarimax_ac)


x <- as.data.frame(t(accuracy(fc_sarimax_ac,ts_Month_AC_test)))
accuracytab2$AutoSARIMAx <- x$`Test set`

checkresiduals(fm_sarimax_ac)
#MODEL TUNED SARIMAX
fm_manualSarimax_month_ac <- Arima(ts_Month_AC_training, order=c(1,1,3),seasonal = list(order = c(1,0,1), period = 12),method="ML", xreg=weather_monthly$avgtemp)
fc_manualSarimax_ac <- forecast(fm_manualSarimax_month_ac, h=11, xreg=fc_weather2011$mean)

accuracy(fc_manualSarimax_ac,ts_Month_AC_test)
checkresiduals(fm_manualSarimax_month_ac)

x <- as.data.frame(t(accuracy(fc_manualSarimax_ac,ts_Month_AC_test)))
accuracytab2$manualSARIMAx <- x$`Test set`

accuracytab2 <- as.data.frame(accuracytab2)
accuracytab2 <- t(accuracytab2)
accuracytab2 <- accuracytab2[2:6,]
colnames(accuracytab2) <- c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")


write.csv(accuracytab2,'/Users/alessandro/M3T1/gap.csv', row.names = TRUE)

#####
#the other submeters
finalForecast <- data.frame(matrix(NA, nrow=11))
finalForecast$submeterAC <-fc_sarimax_ac$mean 
finalForecast$submeterKitchen<- forecastMonthHW$mean
finalForecast$submeterLaundry <- fc_sarimax_ac$mean
finalForecast$submeterTotal <- fc_autoarima_month_ac$mean

finalfinal <- finalForecast

finalForecast <- finalForecast[,-1]

View(finalForecast)

write.csv(finalForecast,'/Users/alessandro/M3T1/finalForecast.csv', row.names = TRUE)

write.csv(Group_Month,'/Users/alessandro/M3T1/MonthlyGroup.csv', row.names = TRUE)


Group_Month1 <-CleanEnergyConsumption %>% group_by(Year, Quarter, Month) %>%
  summarise(
    Sum_GAP=sum(GAP),
    Sum_Others=sum(Others),
    Sum_Kitchen=sum(Kitchen),
    Sum_Laundry=sum(Laundry),
    Sum_Heater_AC=sum(Heater_AC)
  )

Group_Month1$ForecastAC[37:47]<-finalForecast$submeterAC
Group_Month1$ForecasK[37:47]<-finalForecast$submeterKitchen
Group_Month1$ForecasL[37:47]<-finalForecast$submeterLaundry
Group_Month1$ForecasALL[37:47]<-finalForecast$submeterTotal

Group_Month1$MixAC[37:47]<-finalForecast$submeterAC
Group_Month1$MixK[37:47]<-finalForecast$submeterKitchen
Group_Month1$MixL[37:47]<-finalForecast$submeterLaundry
Group_Month1$MixT[37:47]<-finalForecast$submeterTotal


Group_Month1$MixAC[1:36]<-Group_Month1$Sum_Heater_AC[1:36]
Group_Month1$MixK[1:36]<-Group_Month1$Sum_Kitchen[1:36]
Group_Month1$MixL[1:36]<-Group_Month1$Sum_Laundry[1:36]
Group_Month1$MixT[1:36]<-Group_Month1$Sum_GAP[1:36]

Group_Month1$OnlyOrigAC<-Group_Month1$Sum_Heater_AC
Group_Month1$OnlyOrigAC[37:47]<-NA
Group_Month1$OnlyOrigK<-Group_Month1$Sum_Kitchen
Group_Month1$OnlyOrigK[37:47]<-NA
Group_Month1$OnlyOrigL<-Group_Month1$Sum_Laundry
Group_Month1$OnlyOrigL[37:47]<-NA
Group_Month1$OnlyOrigGAP<-Group_Month1$Sum_GAP
Group_Month1$OnlyOrigGAP[37:47]<-NA

Group_Month1$MonthDate <- Group_Month$Date

install.packages('reshape')
library(reshape)

write.csv(finalForecast,'/Users/alessandro/M3T1/finalForecast.csv', row.names = TRUE)

write.csv(Group_Month1,'/Users/alessandro/M3T1/MonthlyGroup.csv', row.names = TRUE)
write.csv2(Group_Month1,'/Users/alessandro/M3T1/MonthlyGroupNew.csv', row.names = FALSE)


CleanEnergyConsumption$DateTime<- as.POSIXct(CleanEnergyConsumption$DateTime, "%Y/%m/%d %H:%M:%S")


CleanEnergyConsumption$Date <- format(as.Date(CleanEnergyConsumption$DateTime), "%Y-%m-%d")

Group_Day <-CleanEnergyConsumption %>% group_by(Date) %>%
  summarise(
    Sum_GAP=sum(GAP)/1000,
    Sum_Others=sum(Others)/1000,
    Sum_Kitchen=sum(Kitchen)/1000,
    Sum_Laundry=sum(Laundry)/1000,
    Sum_Heater_AC=sum(Heater_AC)/1000
  )

Group_Day

Group_Day <- Group_Day[-1,]


Group_Day_training <- Group_Day[1:1094,]
Group_Day_test <- Group_Day[1095:1417,]
nrow(Group_Day)

#subAC
Group_Day_AC_training <- Group_Day_training[c(1,6)]
colnames(Group_Day_AC_training)<- c("ds","y")
m <- prophet(Group_Day_AC_training, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 323)
prediction <- predict(m,future)
plot(prediction$yhat)
prophet_plot_components(m,prediction)
plot(m,prediction)

cross <- cross_validation(m,1,units="days")

tseries <- prediction$yhat[1095:1417]
tseries2<- Group_Day_test$Sum_Heater_AC
performance_metrics(prediction)
accuracy(, tseries2)
library(dplyr)
accuracy()


installed.packages("Metrics")

performance_metrics(prediction)

Group_Day$forecastAC <- prediction$yhat

#subk


Group_Day_AC_training <- Group_Day_training[c(1,4)]
colnames(Group_Day_AC_training)<- c("ds","y")

Group_Day_training$Dateprova <- as.POSIXct(Group_Day$Date)


m <- prophet(Group_Day_AC_training, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 323)
prediction <- predict(m,future)
plot(prediction$yhat)
prophet_plot_components(m,prediction)
plot(m,prediction)
Group_Day$forecastK <- prediction$yhat

#subL


Group_Day_AC_training <- Group_Day_training[c(1,5)]
colnames(Group_Day_AC_training)<- c("ds","y")
m <- prophet(Group_Day_AC_training, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 323)
prediction <- predict(m,future)
plot(prediction$yhat)
prophet_plot_components(m,prediction)
plot(m,prediction)
Group_Day$forecastL <- prediction$yhat

#subTotal
Group_Day_AC_training <- Group_Day_training[c(1,2)]
colnames(Group_Day_AC_training)<- c("ds","y")
m <- prophet(Group_Day_AC_training, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 323)
prediction <- predict(m,future)
accuracy(prediction, Group_Day_test)
plot(prediction$yhat)
prophet_plot_components(m,prediction)
plot(m,prediction)
Group_Day$forecastTotal <- prediction$yhat


Group_Day$OnlyforecastAC <- Group_Day$forecastAC
 Group_Day$OnlyforecastAC[1:1094]<-NA 

 Group_Day$OnlyforecastK<- Group_Day$forecastK
 Group_Day$OnlyforecastK[1:1094]<-NA 

 Group_Day$OnlyforecastL<- Group_Day$forecastL
 Group_Day$OnlyforecastL[1:1094]<-NA 
 
 Group_Day$OnlyforecastTotal<- Group_Day$forecastTotal
 Group_Day$OnlyforecastTotal[1:1094]<-NA 

 Group_Day_new_forecast <- NA
 Group_Day_new_forecast <- Group_Day[,11:14]
 Group_Day_new_forecast <- na.omit(Group_Day_new_forecast)
 
 
 Group_Day_only2009andForecast <- c()
 Group_Day_only2009andForecast$AC <- c(Group_Day$Sum_Heater_AC[1:1094], Group_Day_new_forecast$OnlyforecastAC)
 Group_Day_only2009andForecast$K <- c(Group_Day$Sum_Kitchen[1:1094], Group_Day_new_forecast$OnlyforecastK)
 Group_Day_only2009andForecast$Laundry <- c(Group_Day$Sum_Laundry[1:1094], Group_Day_new_forecast$OnlyforecastL)
 Group_Day_only2009andForecast$Total <- c(Group_Day$Sum_GAP[1:1094], Group_Day_new_forecast$OnlyforecastTotal)
 
 
 Group_Day_only2009andForecast$Type[1:1094] <-"Training Test"
 Group_Day_only2009andForecast$Type[1095:1417] <-"Forecast"
 Group_Day_only2009andForecast$Date[1:1417] <- Group_Day$Date
 Group_Day_only2009andForecast <- as.data.frame(Group_Day_only2009andForecast)

 Group_Day_test <- c()
 Group_Day_testF <- Group_Day[1:6]
 Group_Day_testF$Type <-"Total Series" 
 Group_Day_testF <- Group_Day_testF[-3]
 
Group_Day_testF <- Group_Day_testF[c(6,4,2,3,1,5)]
Group_Day_testF <- Group_Day_testF[c(5,3,4,2,6,1)]
Group_Day_testF <- Group_Day_testF[c(5,3,4,2,6,1)]

Final_Group_daty2<- c()

colnames(Group_Day_testF) <- colnames(Group_Day_only2009andForecast)
Final_Group_daty2 <- rbind(Group_Day_testF,Group_Day_only2009andForecast)

str(Group_Day_AC_training$Date)


write.csv2(Final_Group_daty2,'/Users/alessandro/M3T1/FinalDay3.csv', row.names = FALSE)

 write.csv(Group_Day,'/Users/alessandro/M3T1/MonthlyGroupDay.csv', row.names = TRUE)
 view(Final_Group_daty)
 
trial <- NA
trial<-as.data.frame(trial)
trial$Sum_Kitchen <- c(Group_Month1$Sum_Kitchen,finalfinal$submeterKitchen)
trial$Sum_AC <- c(Group_Month1$Sum_Heater_AC,finalfinal$submeterAC)
trial$Sum_Laundry <- c(Group_Month1$Sum_Laundry,finalfinal$submeterLaundry)
trial$Sum_Total <- c(Group_Month1$Sum_GAP,finalfinal$submeterTotal)
trial$type <- NA
trial$type[1:47]<- "Real Value"
trial$type[48:58]<- "Forecasted"
trial$Date<- c("Jan 2007", "Feb 2007","Mar 2007", "Apr 2007", "May 2007","Jun 2007", "Jul 2007", "Aug 2007","Sep 2007", "Oct 2007", "Nov 2007","Dec 2007",
               "Jan 2008", "Feb 2008",
                    "Mar 2008", "Apr 2008", "May 2008",
                    "Jun 2008", "Jul 2008", "Aug 2008",
                    "Sep 2008", "Oct 2008", "Nov 2008",
                    "Dec 2008", "Jan 2009", "Feb 2009",
                    "Mar 2009", "Apr 2009", "May 2009",
                    "Jun 2009", "Jul 2009", "Aug 2009",
                    "Sep 2009", "Oct 2009", "Nov 2009",
                    "Dec 2009", "Jan 2010", "Feb 2010",
                    "Mar 2010", "Apr 2010", "May 2010",
                    "Jun 2010", "Jul 2010", "Aug 2010",
                    "Sep 2010", "Oct 2010", "Nov 2010",
                     "Jan 2010", "Feb 2010",
                    "Mar 2010", "Apr 2010", "May 2010",
                    "Jun 2010", "Jul 2010", "Aug 2010",
                    "Sep 2010", "Oct 2010", "Nov 2010" )
trial1 <- c()

trial1$Sum_Kitchen <- c(trial$Sum_Kitchen,Group_Month_Training$Sum_Kitchen)
trial1$Sum_AC <- c(trial$Sum_AC,Group_Month_Training$Sum_Heater_AC)
trial1$Sum_Laundry <- c(trial$Sum_Laundry,Group_Month_Training$Sum_Laundry)
trial1$Sum_Total <- c(trial$Sum_Total,Group_Month_Training$Sum_GAP)
trial1$Date <- NA
trial1$Date<- c("Jan 2007", "Feb 2007","Mar 2007", "Apr 2007", "May 2007","Jun 2007", "Jul 2007", "Aug 2007","Sep 2007", "Oct 2007", "Nov 2007","Dec 2007",
                "Jan 2008", "Feb 2008",
                "Mar 2008", "Apr 2008", "May 2008",
                "Jun 2008", "Jul 2008", "Aug 2008",
                "Sep 2008", "Oct 2008", "Nov 2008",
                "Dec 2008", "Jan 2009", "Feb 2009",
                "Mar 2009", "Apr 2009", "May 2009",
                "Jun 2009", "Jul 2009", "Aug 2009",
                "Sep 2009", "Oct 2009", "Nov 2009",
                "Dec 2009", "Jan 2010", "Feb 2010",
                "Mar 2010", "Apr 2010", "May 2010",
                "Jun 2010", "Jul 2010", "Aug 2010",
                "Sep 2010", "Oct 2010", "Nov 2010",
                "Jan 2010", "Feb 2010",
                "Mar 2010", "Apr 2010", "May 2010",
                "Jun 2010", "Jul 2010", "Aug 2010",
                "Sep 2010", "Oct 2010", "Nov 2010", 
                "Jan 2007", "Feb 2007","Mar 2007", "Apr 2007", "May 2007","Jun 2007", "Jul 2007", "Aug 2007","Sep 2007", "Oct 2007", "Nov 2007","Dec 2007",
                "Jan 2008", "Feb 2008",
                "Mar 2008", "Apr 2008", "May 2008",
                "Jun 2008", "Jul 2008", "Aug 2008",
                "Sep 2008", "Oct 2008", "Nov 2008",
                "Dec 2008", "Jan 2009", "Feb 2009",
                "Mar 2009", "Apr 2009", "May 2009",
                "Jun 2009", "Jul 2009", "Aug 2009",
                "Sep 2009", "Oct 2009", "Nov 2009",
                "Dec 2009")



trial1$type <- NA
trial1$type[1:47]<- "Total Series"
trial1$type[48:58]<- "2010 -Forecasted"
trial1$type[59:94] <- "Training Test"

trial1 <- as.data.frame(trial1)

write.csv(trial1,'/Users/alessandro/M3T1/trial1.csv', row.names = TRUE)

 
