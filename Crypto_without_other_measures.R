##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos =
                                           "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos =
                                           "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",repos = 
                                       "http://cran.r-project.org")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos =
                                                      "http://cran.us.r-project.org")
if(!require(xts)) install.packages("xts", repos =
                                     "http://cran.us.r-project.org")
if(!require(rugarch)) install.packages("rugarch", repos = 
                                         "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = 
                                              "http://cran.us.r-project.org")


library(tidyverse)
library(dplyr)
library(caret)
library(PerformanceAnalytics)
library(xts)
library(rugarch)
library(randomForest)

#downloading the cryptos data

#bitcoin
dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/BTC-USD.csv", dl, method = "curl")
bitcoin <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
bitcoin <- bitcoin %>% mutate(Date=as.Date(Date)) %>% 
  filter(Date >= '2015-01-01') %>% 
  filter(Open!=is.na(Open),Volume!=is.na(Volume)) %>%
  filter(Open!='null',Volume!='null') %>% 
  mutate(Open= as.numeric(Open), Volume=as.numeric(Volume)) %>% 
  select(Date,Open,Volume)

#dogecoin
dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/DOGE-USD.csv", dl, method = "curl")
doge <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
doge <- doge %>% 
  mutate(Date=as.Date(Date)) %>% 
  filter(Date >= '2015-01-01') %>% 
  filter(Open!=is.na(Open),Volume!=is.na(Volume)) %>%
  filter(Open!='null',Volume!='null') %>% 
  mutate(Open_dgc= as.numeric(Open), Volume_dgc=as.numeric(Volume)) %>% 
  select(Date,Open_dgc,Volume_dgc)

#ethereum
dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/ETH-USD.csv", dl, method = "curl")
ethereum <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
ethereum  <- ethereum %>% mutate(Date=as.Date(Date)) %>% 
  filter(Date >= '2015-01-01') %>% 
  filter(Open!=is.na(Open),Volume!=is.na(Volume)) %>%
  filter(Open!='null',Volume!='null') %>% 
  mutate(Open_eth= as.numeric(Open), Volume_eth=as.numeric(Volume)) %>% 
  select(Date,Open_eth,Volume_eth)

#litecoin
dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/LTC-USD.csv", dl, method = "curl")
litecoin <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
litecoin <- litecoin %>% mutate(Date=as.Date(Date)) %>% 
  filter(Date >= '2015-01-01') %>%
  filter(Open!=is.na(Open),Volume!=is.na(Volume)) %>%
  filter(Open!='null',Volume!='null') %>% 
  mutate(Open_ltc= as.numeric(Open), Volume_ltc=as.numeric(Volume)) %>% 
  select(Date,Open_ltc,Volume_ltc)

#table together and data exploration
crypto <- inner_join(doge, ethereum, by='Date') %>% inner_join(.,litecoin, by='Date')%>% inner_join(.,bitcoin, by='Date')
#view(crypto)

#selecting all open prices of coins and take the average price of all the other coins than bitcoin
crypto_prices <- crypto %>% select(Date,Open,Open_dgc,Open_eth,Open_ltc) %>% mutate(Open_all_coins =(Open_dgc+Open_eth+Open_ltc)/3)


#test index all expect last 30 days
test_index <- crypto_prices %>% slice_head(n=nrow(crypto_prices)-30)

#transform into a time series
crypto_prices_xts <- as.xts(crypto_prices[, -1], order.by = crypto_prices$Date, dateFormat="POSIXct")
test_index_xts <- as.xts(test_index[, -1], order.by = test_index$Date, dateFormat="POSIXct")

#calculate all the returns
Returns_crypto <- CalculateReturns(crypto_prices_xts)
Return_test_index <- CalculateReturns(test_index_xts)

#remove first line
Returns_crypto <- Returns_crypto[-1,]
Return_test_index <- Return_test_index[-1,]

#only bitcoin prices
bitcoin_xts <- Returns_crypto$Open
dogecoin_xts <- Returns_crypto$Open_dgc
ethereum_xts <- Returns_crypto$Open_eth
litecoin_xts <- Returns_crypto$Open_ltc
all_coins_xts <- Returns_crypto$Open_all_coins

cor(Returns_crypto$Open, Returns_crypto$Open_ltc)
cor(Returns_crypto$Open, Returns_crypto$Open_eth)
cor(Returns_crypto$Open, Returns_crypto$Open_dgc)
cor(Returns_crypto$Open, Returns_crypto$Open_all_coins)

#garch model for all coins
garchspec <- ugarchspec(mean.model=list(armaOrder=c(0,0)),
                        variance.model = list(model="sGARCH"),
                        distribution.model = "norm")

#lets test
bitcoin_test_xts <- Return_test_index$Open
dogecoin_test_xts <- Return_test_index$Open_dgc
ethereum_test_xts <- Return_test_index$Open_eth
litecoin_test_xts <- Return_test_index$Open_ltc
all_coins_test_xts <- Return_test_index$Open_all_coins
garchfit_bitcoin_test <- ugarchfit(data=bitcoin_test_xts,spec=garchspec)
garchfit_dogecoin_test <- ugarchfit(data=dogecoin_test_xts,spec=garchspec)
garchfit_ethereum_test <- ugarchfit(data=ethereum_test_xts,spec=garchspec)
garchfit_litecoin_test <- ugarchfit(data=litecoin_test_xts,spec=garchspec)
garchfit_all_coins_test <- ugarchfit(data=all_coins_test_xts,spec=garchspec)
garchvol_bitcoin_test <- sigma(garchfit_bitcoin_test)
garchvol_dogecoin_test <- sigma(garchfit_dogecoin_test)
garchvol_ethereum_test <- sigma(garchfit_ethereum_test)
garchvol_litecoin_test <- sigma(garchfit_litecoin_test)
garchvol_all_coins_test <- sigma(garchfit_all_coins_test)
volatility_test <- data.frame(Date=index(garchvol_bitcoin_test), vol_bitcoin=coredata(garchvol_bitcoin_test),
                         vol_dogecoin= coredata(garchvol_dogecoin_test),
                         vol_ethereum= coredata(garchvol_ethereum_test),
                         vol_litecoin= coredata(garchvol_litecoin_test),
                         vol_all_coins= coredata(garchvol_all_coins_test))

volatility_test %>% ggplot(aes(x=Date)) + geom_line(aes(y=vol_bitcoin), color="green") 

#correaltion between different volatilities and select which one will help create the model
cor(volatility_test$vol_bitcoin,volatility_test$vol_dogecoin)
cor(volatility_test$vol_bitcoin,volatility_test$vol_ethereum)
cor(volatility_test$vol_bitcoin,volatility_test$vol_litecoin)
cor(volatility_test$vol_bitcoin,volatility_test$vol_all_coins)

# forecast for the next 30 days based 
garchforecast_bitcoin_test <- ugarchforecast(fitORspec = garchfit_bitcoin_test,n.ahead = 30 )
garchforecast_dogecoin_test <- ugarchforecast(fitORspec = garchfit_dogecoin_test,n.ahead = 30 )
garchforecast_ethereum_test <- ugarchforecast(fitORspec = garchfit_ethereum_test,n.ahead = 30 )
garchforecast_litecoin_test <- ugarchforecast(fitORspec = garchfit_litecoin_test,n.ahead = 30 )
garchforecast_all_coins_test <- ugarchforecast(fitORspec = garchfit_all_coins_test,n.ahead = 30 )

forecast_bitcoin <- sigma(garchforecast_bitcoin_test)
forecast_dogecoin <- sigma(garchforecast_dogecoin_test)
forecast_ethereum <- sigma(garchforecast_ethereum_test)
forecast_litecoin <- sigma(garchforecast_litecoin_test)
forecast_all_coins <- sigma(garchforecast_all_coins_test)
#view(forecast_bitcoin)

volatility_bitcoin_forecast <- data.frame(index=index(forecast_bitcoin), coredata(forecast_bitcoin)) %>% mutate(vol_bitcoin=X2021.05.07) %>% select(index,vol_bitcoin)
volatility_dogecoin_forecast <- data.frame(index=index(forecast_dogecoin), coredata(forecast_dogecoin)) %>% mutate(vol_dogecoin=X2021.05.07) %>% select(index,vol_dogecoin)
volatility_ethereum_forecast <- data.frame(index=index(forecast_ethereum), coredata(forecast_ethereum)) %>% mutate(vol_ethereum=X2021.05.07) %>% select(index,vol_ethereum)
volatility_litecoin_forecast <- data.frame(index=index(forecast_litecoin), coredata(forecast_litecoin)) %>% mutate(vol_litecoin=X2021.05.07) %>% select(index,vol_litecoin)
volatility_all_coins_forecast <- data.frame(index=index(forecast_all_coins), coredata(forecast_all_coins)) %>% mutate(vol_all_coins=X2021.05.07) %>% select(index,vol_all_coins)

#getting the correct dates and joining all the predicted volatilities together
tail_dates <- crypto_prices %>% slice_tail(n=30) %>% select(Date)

volatility_30days_forcecast <- data.frame(Date=tail_dates$Date, vol_bitcoin=volatility_bitcoin_forecast$vol_bitcoin,
                              vol_dogecoin= volatility_dogecoin_forecast$vol_dogecoin,
                              vol_ethereum= volatility_ethereum_forecast$vol_ethereum,
                              vol_litecoin= volatility_litecoin_forecast$vol_litecoin,
                              vol_all_coins= volatility_all_coins_forecast$vol_all_coins)
#view(volatility_30days_forcecast)

#fit the data 
#linear regression
fit <- volatility_test %>% 
  lm(vol_bitcoin ~ vol_litecoin + vol_all_coins + vol_ethereum + vol_dogecoin, data = .)

#logistic regression
fit_glm <- volatility_test %>% 
  glm(vol_bitcoin ~ vol_litecoin + vol_all_coins + vol_ethereum + vol_dogecoin, data = .)

#random forest 
fit_rfi <- randomForest(vol_bitcoin ~ ., data = volatility_test)


#using other variables
fit_rf <- randomForest(vol_bitcoin ~ vol_litecoin + vol_all_coins + vol_ethereum + vol_dogecoin, data = volatility_test)

#predict using forecast data
predict <- volatility_30days_forcecast %>%
  mutate(vol_bitcoin_hat = predict(fit, newdata = .))

predict_glm <- volatility_30days_forcecast %>%
  mutate(vol_bitcoin_hat = predict(fit_glm, newdata = .))

predict_rf <- volatility_30days_forcecast %>%
  mutate(vol_bitcoin_hat = predict(fit_rf, newdata = .))


view(predict)
view(predict_glm)
view(predict_rf)


# graph
volatility_30days_forcecast %>% ggplot(aes(x=Date)) + geom_line(aes(y=vol_bitcoin), color="green") + geom_line(aes(y=vol_dogecoin), color="red") + geom_line(aes(y=vol_ethereum), color="orange") + geom_line(aes(y=vol_litecoin), color="blue") + geom_line(aes(y=vol_all_coins), color="pink")
predict %>% ggplot(aes(x=Date)) + geom_line(aes(y=vol_bitcoin_hat), color="green") + geom_line(aes(y=vol_dogecoin), color="red") + geom_line(aes(y=vol_ethereum), color="orange") + geom_line(aes(y=vol_litecoin), color="blue") + geom_line(aes(y=vol_all_coins), color="pink")
predict_glm %>% ggplot(aes(x=Date)) + geom_line(aes(y=vol_bitcoin_hat), color="green") + geom_line(aes(y=vol_dogecoin), color="red") + geom_line(aes(y=vol_ethereum), color="orange") + geom_line(aes(y=vol_litecoin), color="blue") + geom_line(aes(y=vol_all_coins), color="pink")
predict_rf %>% ggplot(aes(x=Date)) + geom_line(aes(y=vol_bitcoin_hat), color="green") + geom_line(aes(y=vol_dogecoin), color="red") + geom_line(aes(y=vol_ethereum), color="orange") + geom_line(aes(y=vol_litecoin), color="blue") + geom_line(aes(y=vol_all_coins), color="pink")


#validation
garchfit_bitcoin <- ugarchfit(data=bitcoin_xts,spec=garchspec)
garchfit_dogecoin <- ugarchfit(data=dogecoin_xts,spec=garchspec)
garchfit_ethereum <- ugarchfit(data=ethereum_xts,spec=garchspec)
garchfit_litecoin <- ugarchfit(data=litecoin_xts,spec=garchspec)
garchfit_all_coins <- ugarchfit(data=all_coins_xts,spec=garchspec)

#data volatility validation
garchvol_bitcoin <- sigma(garchfit_bitcoin)
garchvol_dogecoin <- sigma(garchfit_dogecoin)
garchvol_ethereum <- sigma(garchfit_ethereum)
garchvol_litecoin <- sigma(garchfit_litecoin)
garchvol_all_coins <- sigma(garchfit_all_coins)

#volatility in one table validation
volatility <- data.frame(Date=index(garchvol_bitcoin), vol_bitcoin=coredata(garchvol_bitcoin),
                         vol_dogecoin= coredata(garchvol_dogecoin),
                         vol_ethereum= coredata(garchvol_ethereum),
                         vol_litecoin= coredata(garchvol_litecoin),
                         vol_all_coins= coredata(garchvol_all_coins))
view(volatility)
validation_volatility <- volatility %>% slice_tail(n=30)
validation_volatility %>% ggplot(aes(x=Date)) + geom_line(aes(y=vol_bitcoin), color="green") + geom_line(aes(y=vol_dogecoin), color="red") + geom_line(aes(y=vol_ethereum), color="orange") + geom_line(aes(y=vol_litecoin), color="blue") + geom_line(aes(y=vol_all_coins), color="pink")


#true volatility against prediction
true_volatility_vs_prediction <- data.frame(Date=validation_volatility$Date, vol_bitcoin=validation_volatility$vol_bitcoin,
                         vol_bitcoin_garch_prediction= volatility_30days_forcecast$vol_bitcoin,
                         vol_bitcoin_lm= predict$vol_bitcoin_hat,
                         vol_bitcoin_glm= predict_glm$vol_bitcoin_hat,
                         vol_bitcoin_rf= predict_rf$vol_bitcoin_hat)
view(true_volatility_vs_prediction)


true_volatility_vs_prediction %>% ggplot(aes(x=Date)) + geom_line(aes(y=vol_bitcoin), color="green") + geom_line(aes(y=vol_bitcoin_garch_prediction), color="red") + geom_line(aes(y=vol_bitcoin_lm), color="orange") + geom_line(aes(y=vol_bitcoin_glm), color="blue") + geom_line(aes(y=vol_bitcoin_rf), color="pink")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

RMSE(true_volatility_vs_prediction$vol_bitcoin,true_volatility_vs_prediction$vol_bitcoin_garch_prediction)
RMSE(true_volatility_vs_prediction$vol_bitcoin,true_volatility_vs_prediction$vol_bitcoin_lm)
RMSE(true_volatility_vs_prediction$vol_bitcoin,true_volatility_vs_prediction$vol_bitcoin_glm)
RMSE(true_volatility_vs_prediction$vol_bitcoin,true_volatility_vs_prediction$vol_bitcoin_rf)
