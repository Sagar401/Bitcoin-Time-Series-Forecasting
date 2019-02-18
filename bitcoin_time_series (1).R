library("anytime")
library("bsts")
library("car")
library("caret")
library("forecast")
library("keras")
library("MCMCpack")
library("smooth")
library("tensorflow")
library("tseries")
library("TTR")
library("ggplot2")
library("dplyr")
library("rvest")
library("anytime")
library("corrplot")
library("coinmarketcapr")
library("formatR")
library("yaml")  
library("googleVis")
library("knitr")


##########################################################################################################
#Data Collection

#scraping Bitcoin data
url <- "https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20130428&end=20190205"
bitcoin_data <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="historical-data"]/div/div[2]/table') %>%
  html_table()
bitcoin_data <- bitcoin_data[[1]]

head(bitcoin_data)
summary(bitcoin_data)


#scraping ethereum data
url_2 <- "https://coinmarketcap.com/currencies/ethereum/historical-data/?start=20130428&end=20190205"
ethereum_data <- url_2 %>%
  html() %>%
  html_nodes(xpath='//*[@id="historical-data"]/div/div[2]/table') %>%
  html_table()
ethereum_data <- ethereum_data[[1]]

head(ethereum_data)
summary(bitcoin_data)

#about data
#The Open, High, Low and Close columns in the data set signify the Opening, highest, lowest and 
#Closing price of Bitcoin against the USD on that particular day. 
#The volume refers to the volume of bitcoin transferred in the market on a particular day and 
#Market Cap refers to the total amount of bitcoin on circulation which is to be capped at 21 million.


##########################################################################################################
#Data Cleaning for bitcoin

#formating date in standard format from character
bitcoin_data$Date <- as.Date(anytime(bitcoin_data$Date))
head(bitcoin_data)

#formating market cap as numeric from character by replacing ',' from the data
bitcoin_data$`Market Cap` <- gsub(",","",bitcoin_data$`Market Cap`)
bitcoin_data$`Market Cap` <- as.numeric(bitcoin_data$`Market Cap`)

#formating volume as numeric from character by replacing ',' from the data
bitcoin_data$Volume <- gsub(",","",bitcoin_data$Volume)
bitcoin_data$Volume <- as.numeric(bitcoin_data$Volume)

head(bitcoin_data)

#calculating total number of NA values in each column
colSums(is.na(bitcoin_data))


#volume is consider one of the most important part for analysing trends in bitcoin market as it shows
#volume of bitcoin transfered on a particular day.
#this missing values can be filled by understanding patterns in data
#using this data, we can use the difference between the highest and the lowest price of the day to fill
#the missing values of the volume using the rest of the data set.


#Difference between high and low on each day
a <- matrix(c(0), nrow = 0, ncol = 1)
for(i in 1:nrow(bitcoin_data)){
  a <- rbind(a, bitcoin_data[i,3] - bitcoin_data[i,4])
  i <- i + 1
}
bitcoin_data <- cbind(bitcoin_data,a)

head(bitcoin_data)
summary(bitcoin_data$a)
boxplot(bitcoin_data$a)


#Volume has missing values#
#Data Manipulation#
fifty_avg <- round(mean(bitcoin_data$Volume[bitcoin_data$a < 50], na.rm = TRUE), digits = 2)
hun_avg <- round(mean(bitcoin_data$Volume[bitcoin_data$a > 50 & bitcoin_data$a < 100], na.rm = TRUE), digits = 2)
hf_avg <- round(mean(bitcoin_data$Volume[bitcoin_data$a > 100 & bitcoin_data$a < 150], na.rm = TRUE), digits = 2)
th_avg <- round(mean(bitcoin_data$Volume[bitcoin_data$a > 150 & bitcoin_data$a < 350], na.rm = TRUE), digits = 2)
for(i in 1:nrow(bitcoin_data)){
  if(is.na(bitcoin_data[i,6])){
    if(bitcoin_data$a[i] < 50){
      bitcoin_data$Volume[i] <- fifty_avg
    } else if(bitcoin_data$a[i] < 100){
      bitcoin_data$Volume[i] <- hun_avg
    } else if(bitcoin_data$a[i] < 150){
      bitcoin_data$Volume[i] <- hf_avg
    } else if(bitcoin_data$a[i] < 350){
      bitcoin_data$Volume[i] <- th_avg
    }else
      print("Uncaught Title")
  }
}

bitcoin_data <- bitcoin_data[,-8] #removing column 'a'

summary(bitcoin_data)

#writing data frame into csv file
class(bitcoin_data)
write.table(bitcoin_data,file =  "bitcoin.csv",sep = ',',row.names = F)


#data cleaning is not required for ethereum as it doesnt have any NA values
colSums(is.na(ethereum_data))


##########################################################################################################
#Data Visualization

#Comparing top 3 cryptocurrencies

#Market Cap of top 3 cryptocurrencies
ggplot(top_df, aes(x=top_df$id,y = top_df$market_cap_usd, fill=id)) +geom_bar(stat = "identity")+
  xlab("Leading CC")+ylab("USD")+theme_minimal()+ggtitle("Market Cap of Leading Cryptocurrency")
#From the plot it can be noticed that Bitcoin is having very high market.

#Variation in prices
dfa<-data.frame(currency=c("Bitcoin","Ethereum","Ripple"),change1h=c(0.14,0.14,0.19),change24h=c(0.09,-1.11,0.07),change7d=c(5.57,0.82,10.45))
bar<-gvisColumnChart(dfa, xvar = "currency",yvar = c("change1h","change24h","change7d"),options=list(title = "% change of prices", legend = "top", width=1000, height=800))
plot(bar)
#Negative value of percentage variation shows a drop in the prices and positive means increase in the prices.

#Volume
ggplot(top_df, aes(x=top_df$id,y = top_df$X24h_volume_usd, fill=id)) +geom_bar(stat = "identity")+
  xlab("Leading CC")+ylab("USD")+theme_minimal()+ggtitle("Volume")
#Volume is the number of transactions of a cry  ptocurrency. 
#It can be noticed from the plot that number of trancations of Bitcoin is very high compared to other currencies.

#Ploting bitcoin_data
#Bitcoin Closing Price and Opening Price
ggplot(bitcoin_data, aes(bitcoin_data$Date, bitcoin_data$`Close**`)) + 
  geom_line(color='blue') + scale_x_date("Year")+ ylim(0,20000) + ylab("Closing Price")+
  ggtitle("Bitcoin Closing Price")

ggplot(bitcoin_data, aes(bitcoin_data$Date, bitcoin_data$`Open*`))+ 
  geom_line(color='red') + scale_x_date("Year")+ ylim(0,15000) + ylab("Opening Price")+
  ggtitle("Bitcoin Opening Price")

#Boxplot and Hist of Closing Price
breaks<-c(50,1000,3000,5000,10000,15000,20000)
labels<-c("50-1000","1000-3000","3000-5000","5000-10000","10000-15000","15000-20000")
bins <- cut(bitcoin_data$`Close**`,breaks, include.lowest = T,right = F,labels = labels)
plot(bins,col=3, main="Bitcoin Closing Price", xlab="Prices", ylab="Frequency")
boxplot(bitcoin_data$`Close**`,col=7, main="Boxplot of Bitcoin CP", xlab="Closing Price",ylab="Prices" )

#Comparing bitcoin Opening and Closing price
ggplot(bitcoin_data, aes(bitcoin_data$Date))+ geom_line(aes(y = bitcoin_data$`Open*`, colour="Open"))+ 
  geom_line(aes(y = bitcoin_data$`Close**`, colour="Close"))+ scale_x_date("Year")+ ylim(0,20000) + ylab("Prices")+ 
  ggtitle("Comparison of Bitcoin Prices")

#Comparison of Bitcoin Market Cap and Volume
breakv<-c(2.858e+05,2.858e+07,2.858e+09,2.858e+11)
labelss<-c("2.858e+05-2.858e+07","2.858e+07-2.858e+09","2.858e+09-2.858e+11")
binss<-cut( bitcoin_data$Volume,breakv,include.lowest = T,right = F,labels = labelss)
boxplot(bitcoin_data$Volume,col=7, main="Boxplot of Bitcoin Volume", xlab="Volume",ylab="Prices" )
plot(binss,col=3,main="Bitcoin Volume", xlab="Prices", ylab="Frequency")

breakmp<-c(7.784e+08,7.784e+09,7.784e+10,7.784e+11)
labelsss<-c("7.784e+08-7.784e+09","7.784e+09-7.784e+10","7.784e+10-7.784e+11")
binsss<-cut(bitcoin_data$`Market Cap`,breakmp,include.lowest = T,right = F,labels =labelsss)
plot(binsss,col=4, main="Bitcoin Market Cap", xlab="Prices", ylab="Frequency")

ggplot(bitcoin_data, aes(bitcoin_data$Date))+ geom_line(aes(y = bitcoin_data$`Market Cap`, colour="Market Cap")) + 
  geom_line(aes(y = bitcoin_data$Volume, colour="Volume"))+ scale_x_date("Year") +ylab("Prices")+
  ggtitle("Comparing Bitcoin Market Cap and Volume")

#Ploting ethereum data
bitcoinext<- bitcoin_data[c(1:1279),]
View(bitcoinext)
bit_et<-cbind(bitcoinext,ethereum_data)
colnames(ethereum_data)[colnames(ethereum_data)=="Date"]<-"Et.Date"
colnames(ethereum_data)[colnames(ethereum_data)=="Open*"]<-"Et.Open"
colnames(ethereum_data)[colnames(ethereum_data)=="High"]<-"Et.High"
colnames(ethereum_data)[colnames(ethereum_data)=="Low"]<-"Et.Low"
colnames(ethereum_data)[colnames(ethereum_data)=="Close**"]<-"Et.Close"
colnames(ethereum_data)[colnames(ethereum_data)=="Volume"]<-"Et.Volume"
colnames(ethereum_data)[colnames(ethereum_data)=="Market Cap"]<-"Et.Market Cap"
bit_et<-cbind(bitcoinext,ethereum_data)
View(bit_et)
ggplot(bit_et,aes(bit_et$Date,bit_et$Et.Close)) %>%
  +geom_line()+scale_x_date("year")+ ylim(0,1500)+ylab("closing Price")+ggtitle("Closing Price of Ethereum")
ggplot(bit_et,aes(bit_et$Date,bit_et$Et.Open))+
  geom_line() + scale_x_date("Year")+ ylim(0,1500) + ylab("Opening Price")+ggtitle("Opening price of Ethereum")

#Bitcoin vs Ethereum

ggplot()+geom_line(data = bit_et,aes(Date, bit_et$`Open*`),color="green")+ 
  geom_line(data = bit_et,aes(Date, bit_et$Et.Open),color="red")+ylab("Opening Price")+
  xlab("Year")+ggtitle("Bitcoin OP vs Ethereum OP")
ggplot()+geom_line(data = bit_et,aes(Date, bit_et$`Close**`), color="green")+ 
  geom_line(data = bit_et,aes(Date, bit_et$Et.Close),color="red")+ylab("Closing Price")+
  xlab("Year")+ggtitle("Bitcoin CP vs Ethereum CP")

ggplot(bitcoin_data, aes(bitcoin_data$Date))+ geom_line(aes(y = bitcoin_data$High, colour="High"))+ 
  geom_line(aes(y = bitcoin_data$Low, colour="Low"))+ scale_x_date("Year")+ ylim(0,20000) + ylab("Prices")+
  ggtitle("Comparison of Bitcoin Trends")  
ggplot(bit_et, aes(bit_et$Date))+ geom_line(aes(y = bit_et$Et.High, colour="High"))+ 
  geom_line(aes(y = bit_et$Et.Low, colour="Low"))+ scale_x_date("Year")+ ylim(0,1500) + ylab("Prices")+
  ggtitle("Comparison of Ethereum Trends")  

#correlation Matrix
bitcoin_data %>% 
  select(`Open*`,`Close**`,High,Low,Volume,`Market Cap`) %>% 
  model.matrix(~.-1, .) %>% 
  cor(method = "spearman") %>%
  corrplot(type="lower", method = "number", tl.col = "black", diag=FALSE, tl.cex = 0.9, number.cex = 0.9)


#spliting data into train and test set
row.num <- c(nrow(bitcoin_data) : 21 )
train_bitcoin <- bitcoin_data[row.num,]
test_bitcoin <- bitcoin_data[20:1,]

#plot of train dataset
ggplot(train_bitcoin, aes(train_bitcoin$Date, train_bitcoin$`Close**`)) +
  geom_line(color = 'blue') + scale_x_date("year")+ ylim(0,20000) + ylab("Closing Price") + 
  ggtitle('train_dataset')

#plot of test dataset
ggplot(test_bitcoin, aes(test_bitcoin$Date, test_bitcoin$`Close**`)) +
  geom_line(color = 'blue') + scale_x_date("year")+ ylim(3000,4000) + ylab("Closing Price") + 
  ggtitle('test_dataset')


#forecasting model with closing prices of bitcoin
bitcoin_time_series  <- ts(train_bitcoin$`Close**`,frequency = 365.25,start = c(2013,4,27))
bitcoin_time_series_decompose <- decompose(bitcoin_time_series)
plot(bitcoin_time_series_decompose)

#It can be seen there is a uniform seasonal variation in the closing price of bitcoins over years and
#and trend has been almost constant till the end of 2018.

#lets plot linear model to our data
bitcoin_data_lm  <- data.frame(close=bitcoin_data$`Close**`,
                               open=log(bitcoin_data$`Open*`+1),
                               high=log(bitcoin_data$High),
                               low=log(bitcoin_data$Low+1),
                               market=log(bitcoin_data$`Market Cap`+1))
fit <- step(lm(close ~ open  + high 
               + low + market, data=bitcoin_data_lm))

summary(fit)

plot(fitted(fit),type = "p", bitcoin_data_lm$close,ylab="Closing Price", xlab="Predicted Closing price", main="Linear Model")



###########################################################################################################
#Data Modeling

#The following are the forecasting algorithms used in predicting the price of bitcoins and each of it 
#reported with the prediction plots and accuracy.

#Holt's Forecasting model:

#Holt's Forecasting or double exponential smoothing method uses exponential smoothing based on the given
#input values of alpha(Data Smoothing Factor) and beta(Trend Smoothing factor). 
#Exponential smoothing is a technique for smoothing time series data, exponential functions are used to 
#assign exponentially decreasing weigths overtime. Single exponential smoothing does not perform well when 
#there is a trend in data. In such situation, we use 'double exponential smoothing' or 
#'second-order exponential smoothing', which is a recursive application of an exponential smoothing twice.
#Basic idea is to take into account the possibilty of a series exhibiting some fx`orm of trend.

#Initially model is constructed using Holt's algorithm and this model is 
#used to predict the forecast prices of bitcoins for next 10 days.
dim(train_bitcoin)

holt_model <- holt(train_bitcoin[1000:2090,'Close**'],type="additive", damped =F, h=10)
holt_forecast <- forecast(holt_model,h = 10)
holt_df <- as.data.frame(holt_forecast)

#Forecast plot
plot(holt_forecast, ylab = 'Closing Prices(in USD)', xlab = 'Total number of Days' , main = 'Holts Forecast')

holtfdf <- cbind(test_bitcoin[1:10,], holt_df[,1])
testdata <- test_bitcoin[1:10,5]

accuracy(holt_df[,1], testdata) #accuracy

ggplot() + geom_line(data = holtfdf, aes(Date, holtfdf[,5]), color = "blue") + ylab('Closing Price') +
  geom_line(data = holtfdf, aes(Date, holtfdf[,8]), color = "Dark Red") + 
  ggtitle('Predicted vs Actual for Holts Method') 

#The mean accurate prediction error comes out to be 1.47% which is very good as it is under 10%. 
#Here we can see a major problem that model cannot predict the most of dips in prices of bitcoins.
#Reason can be that data might be showing some seasonality(periodicity) so only trends might not fit data good.

#---------------------------------------------------
#Exponential Triple Smoothing 
#Triple exponential smoothing applies exponential smoothing three times, whenever our data shows both 
#trends and seasonality(also called periodicity). There are two types of seasonality 'multiplicative' 
#and 'additive' in nature.The additive method is preferred when the seasonal variations are roughly
#constant through the series, while the multiplicative method is preferred when the seasonal variations 
#are changing proportional to the level of the series. 
#Our data shows multiplicative nature as, the seasonal component is 
#expressed in relative terms (percentages), and the series is seasonally adjusted by dividing through 
#by the seasonal component. 

ETS <- ets((train_bitcoin[,'Close**']),allow.multiplicative.trend=TRUE)
ETS_forecast <- forecast(ETS, h = 10)
ETS_df <- as.data.frame(ETS_forecast)

plot(forecast(ETS, h =10), ylim = c(0,20000), xlab = 'Closing Prices(in USD)', 
              ylab = 'Total number of Days' ,main = 'ETS Forecast')

ETS_p <- predict(ETS, n.ahead = 10, prediction.interval = T, level = 0.95)

testdata <- test_bitcoin[1:10,5]

accuracy(ETS_df[,1], testdata)

etsfdf <- cbind(test_bitcoin[1:10,], ETS_df[,1])

ggplot() + geom_line(data = etsfdf, aes(Date, etsfdf[,5]), color = "blue") + ylab('Closing Price') +
  geom_line(data = etsfdf, aes(Date, etsfdf[,8]), color = "Dark Red") + 
  ggtitle('Predicted vs Actual for ETS method') 


#The mean accurate prediction error comes out to be 0.82% which has improved in accuracy, error rate is 
#decreased by 0.7
#But still we can observe that our predicted value is not curving with the data, it still posses linear trend.


#------------------------------------------
#ARIMA Forecasting 
#Autoregressive Integrated Moving Average forecasting is a method extensively used for time series forecasting
#This Forecasting model aims to describe the autocorrelations in the data

#A Stational time series is one whose properties do not depend on the time at which the series is observed.
#Thus, time series with trends, orwith seasonality, are not stationary - the trend and seasonality will 
#affect the value of the time series at different times. A stationary series will look much the same 
#at any point in time(can be also said as cyclic in nature).

#If we look at our data, in long term, the timing of these cycle is not predictable. Hence the series is 
#stationary.

#Transformations such as logarithms can help to stabilise the variance of a time series.
#Differencing can help stabilise the mean of a time series by removing changes in the level of a time series,
#and therefore eliminating (or reducing) trend and seasonality.

tsdf <- diff(train_bitcoin[,5], lag = 2)
tsdf <- tsdf[!is.na(tsdf)]
adf.test(tsdf)
plot(tsdf,type = "l", xlab = 'Closing Price',ylab='Number_of_Days',ylim = c(-5000, 5000), main='Difference Plot')

#As well as looking at the time plot of the data, the ACF plot is also useful for identifying 
#non-stationary time series. For a stationary time series, the ACF will drop to zero relatively quickly,
#while the ACF of non-stationary data decreases slowly.

#Auto- and Cross- Covariance and -Correlation Function Estimation
#plots the estimates 
acf(tsdf)
#pacf is used for partial auto correlation function
pacf(tsdf)  


#It divides the time series data into models based on time lags in an autoregressive model, the 
#differencing and the order of the moving average model. R provides functionality between 2 kinds of model
#One is auto arima function which automatically fits the data to the best possible order and other one is
#ARIMA forecasting in which the order needs to be determined based on ACF and PACF plot. Both the models 
#are created for forecasting and are compared for accuracy. 

#fit best ARIMA model
auto.arima(train_bitcoin[,5])

bitcoin_time_series_forecast_auto <- forecast(auto.arima(train_bitcoin[,5]), h=10)
plot(bitcoin_time_series_forecast_auto, ylab = 'Closing Price', xlab = 'Number_of_Days', main='Forecast AUTO-ARIMA (1,1,1)')

bts_f_df_auto <- as.data.frame(bitcoin_time_series_forecast_auto)

testdata <- test_bitcoin[1:10,5]
accuracy(bts_f_df_auto[,1],testdata)

gegefct_auto <- cbind(test_bitcoin[1:10,], bts_f_df_auto[,1])
ggplot() + geom_line(data = gegefct_auto, aes(Date, gegefct_auto[,5]), color = "blue") + ylab('Closing Price') +
  geom_line(data = gegefct_auto, aes(Date, gegefct_auto[,8]), color = "Dark Red") +  
  ggtitle('Predicted vs Actual for Auto-ARIMA')
#The mean accurate prediction error comes out to be 1.50% for Auto ARIMA 


#predicting 10 interval based on selecting parameters from ACF and P-ACF ARIMA model
bitcoin_time_series_forecast <- forecast(arima(train_bitcoin[,5], order = c(1,1,8)), h=10)
arima(train_bitcoin[,5], order = c(1,1,8))

bitcoin_time_series_forecast 
plot(bitcoin_time_series_forecast)

bts_f_df <- as.data.frame(bitcoin_time_series_forecast)

testdata <- test_bitcoin[1:10,5]
accuracy(bts_f_df[,1],testdata)

gegefct <- cbind(test_bitcoin[1:10,], bts_f_df[,1])
plot(bitcoin_time_series_forecast,ylab = 'Closing Price', xlab = 'Number_of_Days', main='Forecast ARIMA (1,1,8)')
ggplot() + geom_line(data = gegefct, aes(Date, gegefct[,5]), color = "blue") + ylab('Closing Price') +
  geom_line(data = gegefct, aes(Date, gegefct[,8]), color = "Dark Red") +  ggtitle('Predicted vs Actual for ARIMA')

#The mean accurate prediction error comes out to be 0.68% for ARIMA in which we set parameters
#by observing ACF and PACF graph. This can be treated as overfitted as well. 

#-----------------------------------------------------------------------------------------------------
#ALL models line plot

all <- cbind(test_bitcoin[1:10,],bts_f_df[,1],ETS_df[,1],holt_df[,1], bts_f_df_auto[,1])
head(all)

ggplot() + geom_line(data = all, aes(Date, all[,5]), color = "blue") + ylab('Closing Price') +
  geom_line(data = all, aes(Date, all[,8]), color = "green") +  
  geom_line(data = all, aes(Date, all[,9]), color = "red") +  
  geom_line(data = all, aes(Date, all[,10]), color = "orange") +  
  geom_line(data = all, aes(Date, all[,11]), color = "dark green") +  
  ggtitle('Predicted vs Actual for all methods')


#------------------------------------------




