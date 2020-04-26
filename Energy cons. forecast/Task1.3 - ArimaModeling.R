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


weather_daily <- readRDS("~/M3T1/weather_daily.RDS")

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
ts_Month_AC <- ts(Group_Month$Sum_Heater_AC,frequency = 12, start=c(2007,1))
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
ts_Month_AC <- ts(Group_Month$Sum_Heater_AC,frequency = 12, start=c(2007,1))
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

ts_Month_AC_training <- ts(Group_Month_Training$Sum_Laundry, frequency=12, start=c(2007,1) )

#decompose
componentsForecastMonthAC <- decompose(ts_Month_AC_training)
plot(componentsForecastMonthAC)
ggtsdisplay(ts_Month_AC_training, lag.max = 24)

autoplot(componentsForecastMonthAC)

ggtsdisplay(componentsForecastMonthAC$seasonal)




#EVERYTHING WITHOUT OUTLIERS####
tsoutliers(ts_Month_AC_training)


#BETA SEASONALITY , GAMMA FALSE
HWModel_tsMonth_AC<- HoltWinters(ts_Month_AC_training)
autoplot(HWModel_tsMonth_AC)
forecastMonthHW <- forecast(HWModel_tsMonth_AC, h=11)
autoplot(forecastMonthHW)
ts_Month_AC_test <- ts(Group_Month_Test$Sum_Laundry, start=c(2010,1), frequency = 12)

x <- as.data.frame(t(accuracy(forecastMonthHW, ts_Month_AC_test)))
accuracytab2 <- NA
accuracytab2$HW_NO <- x$`Test set`

#AUTOARIMA



autoplot(ts_Month_AC)
ur.kpss(ts_Month_AC)
ndiffs(ts_Month_AC_training) 
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
ur.kpss(ts_Month_AC)
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

#####
#the other submeters
finalForecast <- data.frame(matrix(NA, nrow=11))
finalForecast$submeterAC <-fc_sarimax_ac$mean 
finalForecast$submeterKitchen<- forecastMonthHW$mean
finalForecast$submeterLaundry <- fc_sarimax_ac$mean
finalForecast$submeterTotal <- fc_autoarima_month_ac$mean

finalfinal <- finalForecast

finalForecast <- finalForecast[,-1]

write.csv(finalForecast,'/Users/alessandro/M3T1/finalForecast.csv', row.names = TRUE)









