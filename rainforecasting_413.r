#MIDTERM SCRIPT
#AUTHOR: Nitin Gaonkar


library("lmtest")
library("lawstat")
library("Hmisc")
library("forecast")
library("cars")

#loading the source data

original_data <-read.csv("/Users/ngaonkar/Downloads/competitiondata.csv", header= TRUE)
summary(original_data)

##Converting a column from integer to date in a dataframe:
new_data<- transform(original_data, DATE = as.Date(as.character(DATE), "%Y%m%d"))

##Read  a file or a data frame and rollup data from monthly to daily.
x.zoo<-read.zoo(new_data)
consoldata <- apply.monthly(x.zoo,sum)

#convert to timeseries

consoldata_ts<- ts(consoldata, start=c(1946,9),frequency=12)

#Average/naïve/drift method:

meanf(consoldata_ts)
naive(consoldata_ts)

#plot for mean/drift/naive

consoldata_test <- window(consoldata_ts,start=2011)
consoldata_train <- window(consoldata_ts,end=2010)
plot(consoldata_train,main="Rainfall",  ylab="prcp",ylim=c(0,20),xlab="time",xlim=c(1940,2015))
lines(meanf(consoldata_train,h=42)$mean,col=4)
lines(rwf(consoldata_train,h=42)$mean,col=2)
lines(rwf(consoldata_train,drift=TRUE,h=42)$mean,col=3)
legend("topleft",lty=1,col=c(4,2,3),cex=0.7, title="FORECAST",
       +        legend=c(paste0("Mean Method  ($", sprintf("%4.2f",max(mean(consoldata_train))),")"),
                         +                 paste0("Naive Method ($", sprintf("%4.2f",max(rwf(consoldata_train,h=60)$mean)),")"),"Drift method"))


#function to forecast from naive method:
#forecasting naive method

forecastit_naive=function(model,forecast,level)
{
  fit<-naive(model)	#naive method
  forecast_df = data.frame(forecast_predicted=fit$mean,forecast_lower=fit$lower[,2],forecast_upper=fit$upper[,2])  # high 95%
  forecast_df_2 = data.frame(forecast_predicted=fit$mean,forecast_lower=fit$lower[,1],forecast_upper=fit$upper[,1])  # high 80%
  mylist=list(forecast_df,forecast_df_2)
  return(mylist)
}
#example
consoldata_ts
consoldata_ts_new<- consoldata_ts + 1
consoldata_ts_new >- log(consoldata_ts_new)
model=consoldata_ts_new
forecast=data.frame(new_data)
l=.05
forecastit_naive(model, forecast, level)

#ETS/ARIMA/TBATS METHODS:

m_ets = ets(consoldata_ts_new)
f_ets = forecast(m_ets, h=24) # forecast 24 months into the future
plot(f_ets)

#arima method
m_aa = auto.arima(consoldata_ts_new)
f_aa = forecast(m_aa, h=24)
plot(f_aa)

#TBATS

m_tbats = tbats(consoldata_ts_new)
f_tbats = forecast(m_tbats, h=24)
plot(f_tbats)

#comparing the models with AIC values

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#arima method implementation for forecasting

#forecasting Arima method:
forecastit_arima=function(model,forecast,level)
{
  fit<-auto.arima(model)	#runregression
  forecast_df = data.frame(forecast_predicted=f_aa$mean,forecast_lower=f_aa$lower[,2],forecast_upper=f_aa$upper[,2])  # high 95%
  forecast_df_2 = data.frame(forecast_predicted=f_aa$mean,forecast_lower=f_aa$lower[,1],forecast_upper=f_aa$upper[,1])  # high 80%
  mylist=list(forecast_df,forecast_df_2)
  return(mylist)
}
#example
consoldata_ts
consoldata_ts_new<- consoldata_ts + 1
consoldata_ts_new >- log(consoldata_ts_new)
model=consoldata_ts_new
forecast=data.frame(new_data)
l=.05
forecastit_arima(model, forecast, level)


#regression function for forecast

#forecasting regression
forecastit_reg=function(model,forecast,level)
{
  fit<-tslm(model)	#runregression
  frct <- forecast.lm(fit,forecast)
  mylist=list(frct)
  return(mylist)
}
#example
consoldata_ts
consoldata_ts_new<- consoldata_ts + 1
consoldata_ts_new >- log(consoldata_ts_new)
model=consoldata_ts_new ~ trend + season
forecast=data.frame(new_data)
l=.05
forecastit_reg(model, forecast, level)


#residual and other plots:
r1=fit$residuals #residuals
r2=shapiro.test(r1) #test for normality of residuals
r3a=ncvTest(fit) #non-constant variance test
r3=dwtest(fit)  #independence of residuals  #test for independence of errors
me=mean(fit$residuals)
mad=mean(abs(fit$residuals))
mse=mean(fit$residuals^2)
rmse=sqrt(mse)
all=c(me,mad,rmse,mpe,mape)
names(all)=c("ME","MAD","RMSE")
mybarplot=barplot(all)











