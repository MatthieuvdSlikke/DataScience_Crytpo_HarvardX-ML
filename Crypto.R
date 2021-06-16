##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos =
                                           "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("dplyr", repos =
                                           "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",
                                     repos = "http://cran.r-project.org")
if(!require(data.table)) install.packages("data.table", repos =
                                            "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos =
                                           "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos =
                                           "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos =
                                           "http://cran.us.r-project.org")
if(!require(textdata)) install.packages("textdata", repos =
                                           "http://cran.us.r-project.org")
if(!require(rtweet)) install.packages("rtweet", repos =
                                          "http://cran.us.r-project.org")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos =
                                        "http://cran.us.r-project.org")
if(!require(xts)) install.packages("xts", repos ="http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(caret)
library(data.table)
library(lubridate)
library(gridExtra)
library(textdata)
library(tidytext) 
library(textdata)
library(rtweet)
library(PerformanceAnalytics)
library(xts)


# Options_KR dataset:
# https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/TweetsElonMusk.csv

dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/TweetsElonMusk.csv", dl, method = "curl")
tweets <- read.csv(file = dl, header = TRUE,stringsAsFactors = FALSE)
tweets <- tweets %>% select(date, username, tweet, replies_count, likes_count, retweets_count, link) %>% mutate(tweet=as.character(tweet))

#calculating sentiment of tweets
tweet_sentiment <- tweets %>%
  unnest_tokens(word, tweet, token = "tweets") %>%
  inner_join(get_sentiments("afinn"))
sent <- tweet_sentiment %>% group_by(link) %>% summarise(Avg_Score = mean(value))
view(sent)

#join sentiment score and tweets
all_together <- full_join(tweets, sent, by="link")
#make other score 0 if no sentiment 
all_together[is.na(all_together)] <-  0
view(all_together)

all_together <- all_together %>% mutate(date=as.Date(date)) %>% filter(date >= '2015-01-01')
all_together %>% ggplot(aes(x=date)) + geom_point(aes(y = Avg_Score))

min(all_together$date)

#there is more lines using the afinn library then using the bing library, so we are going to use the afinn library.

dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/BTC-USD.csv", dl, method = "curl")
bitcoin <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)

bitcoin <- bitcoin %>% mutate(Date=as.Date(Date)) %>% filter(Date >= '2015-01-01') %>% filter(Date <= '2021-04-17') %>% mutate(Open= as.numeric(Open))
bitcoin <- bitcoin %>% mutate(date=Date) 
btc_tweet <- full_join(bitcoin, all_together, by="date")
btc_tweet <- btc_tweet %>% select(date,Open,Avg_Score,replies_count,likes_count,retweets_count)
btc_tweet <- btc_tweet %>% filter(Open!='null')
view(btc_tweet)


redone <- btc_tweet %>% mutate(Open= as.numeric(Open)) 
nodate <- redone  %>% select(-date) %>% mutate(replies_count=as.numeric(replies_count),
                            likes_count=as.numeric(likes_count),
                            retweets_count=as.numeric(retweets_count))
nodate[is.na(nodate)] <-  0
view(nodate)

cor(x=nodate$Open,y=nodate$Avg_Score)

view(redone)

btc_tweet %>%  mutate(Open=as.numeric(Open)) %>% ggplot(aes(x=date, y = Open)) + geom_line()
redone %>% ggplot(aes(x=date, y = Open)) + geom_line()

btc_to_xts<- bitcoin  %>% select(Date,Open)
btc_to_xts<- as.xts(btc_to_xts[, -1], order.by = btc_to_xts$Date, dateFormat="POSIXct")
view(btc_to_xts)


#calculating returns
Returns <- CalculateReturns(btc_to_xts)
Returns[is.na(Returns)] <-  0
sd(Returns)*sqrt(252)
view(Returns)
plot(Returns)
ret_try <- data.frame(date=index(Returns), returns=coredata(Returns))
ret_try <- ret_try %>% mutate(direction= ifelse(returns>=0,1,-1))
ret_try[is.na(ret_try)] <-  0
view(ret_try)


avg_score_day <- redone  %>% select(date, Avg_Score)
avg_score_day <- avg_score_day %>% group_by(date) %>% summarise(Avg_Score = mean(Avg_Score))
avg_score_day[is.na(avg_score_day)] <-  0

total <- full_join(ret_try , avg_score_day, by="date")
total <- total %>% mutate(returns= as.numeric(returns), Avg_Score=as.numeric(Avg_Score),direction=as.numeric(direction))
total[is.na(total)] <-0
total <- total %>% mutate(sent=ifelse(Avg_Score==0,0,ifelse(Avg_Score>0,1,-1)))
cor(x=total$returns,y=total$Avg_Score)
cor(x=total$direction,y=total$Avg_Score)
cor(x=total$direction,y=total$sent)
 

#adding weights

tweet_sentiment_new <- tweets %>%
  unnest_tokens(word, tweet, token = "tweets") %>% 
  filter(str_to_lower(word) %in% c("bitcoin", "bitcoins","btc",
                                   "dogecoin","coin","coins", 
                                   "dogecoins","crypto" ,"cryptocurrency",
                                   "market", "blockchain","ethereum","eth", "nft",
                                   "stocks", "doge", "finance", "currency", 
                                   "mining","invest","invests","investor", "token") ) 
tweet_sentiment_new  <- tweet_sentiment_new %>% mutate(weight=1)
new_al <- inner_join(tweets, tweet_sentiment_new, by="link") %>% select(link,weight) %>% group_by(link) %>% summarise(sum_weight=2)
view(new_al)

new_together <- inner_join(all_together,new_al,by="link")
new_together <- new_together %>% filter(!is.na(date))
new_together <- new_together %>% mutate(sum_weight=ifelse(is.na(sum_weight),1,2))
view(new_together)

new_togethero <- new_together %>% group_by(date) %>% summarise(sum_weight=2)
view(new_togethero)

totali <- full_join(total,new_togethero, by="date")
totali[is.na(totali)] <- 1
totali <- totali %>% mutate(avg= sum_weight*sent)
view(totali)
cor(x=totali$direction,y=totali$avg)

#too weak of correlation

#getting other finance datat
dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/AAPL.csv", dl, method = "curl")
apple <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
apple <- apple %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17') %>% mutate(Open_appl= as.numeric(Open)) %>% select(date,Open_appl)

dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/FB.csv", dl, method = "curl")
facebook <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
facebook <- facebook %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17') %>% mutate(Open_fb= as.numeric(Open)) %>% select(date,Open_fb)

dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/GC%3DF.csv", dl, method = "curl")
gold <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
gold <- gold %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17') %>% mutate(Open_gld= as.numeric(Open)) %>% select(date,Open_gld)
  
dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/GOOGL.csv", dl, method = "curl")
google <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
google <- google %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17') %>% mutate(Open_ggl= as.numeric(Open)) %>% select(date,Open_ggl)
  
dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/TSLA.csv", dl, method = "curl")
tesla <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
tesla <- tesla %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17') %>% mutate(Open_tsla= as.numeric(Open)) %>% select(date,Open_tsla)

dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/AMZN.csv", dl, method = "curl")
amazon <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
amazon <- amazon %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17') %>% mutate(Open_amz= as.numeric(Open)) %>% select(date,Open_amz)

view(tesla)

all_prices <- left_join(apple, facebook, by='date') %>% left_join(., gold, by='date') %>% left_join(., google, by='date') %>% left_join(., tesla, by='date') %>% left_join(., amazon, by='date')
view(all_prices)

#we will do a right join and not left join because we want to remove the weekends 
all_prices_total <- right_join(total, all_prices, by='date') 
view(all_prices_total)
cor(x=all_prices_total$returns,y=all_prices_total$Open_appl)
cor(x=all_prices_total$returns,y=all_prices_total$Open_fb)
cor(x=all_prices_total$returns,y=all_prices_total$Open_gld)
cor(x=all_prices_total$returns,y=all_prices_total$Open_tsla)
cor(x=all_prices_total$returns,y=all_prices_total$Open_amz)

#calculate returns for all
all_pricesc_to_xts<- as.xts(all_prices[, -1], order.by = all_prices$date, dateFormat="POSIXct")
view(all_pricesc_to_xts)
Returns_all <- CalculateReturns(all_pricesc_to_xts)
Returns_all[is.na(Returns_all)] <-  0
#to remove first row Returns_all <- Returns_all[(-1),] 
view(Returns_all)


#volatility way bigger for bitcoin
sd(Returns)*sqrt(252)
sd(Returns_all)*sqrt(252)

ret <- data.frame(date=index(Returns_all), coredata(Returns_all)) %>% mutate(date=as.Date(date))

view(ret)

all_prices_total_ret <- right_join(total, ret, by='date') 
all_prices_total_ret <- all_prices_total_ret %>% mutate(dir_appl= ifelse(Open_appl>=0,1,-1),
                                                        dir_fb= ifelse(Open_fb>=0,1,-1),
                                                        dir_gld= ifelse(Open_gld>=0,1,-1),
                                                        dir_tsla= ifelse(Open_tsla>=0,1,-1),
                                                        dir_amz= ifelse(Open_amz>=0,1,-1))

view(all_prices_total_ret)
#cor not strong enough between 
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$Open_appl)
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$Open_fb)
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$Open_gld)
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$Open_tsla)
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$Open_amz)

cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$dir_appl)
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$dir_fb)
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$dir_gld)
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$dir_tsla)
cor(x=all_prices_total_ret$returns,y=all_prices_total_ret$dir_amz)

#however tech returns are moderately correlated
cor(x=all_prices_total_ret$Open_fb,y=all_prices_total_ret$Open_appl)
cor(x=all_prices_total_ret$Open_appl,y=all_prices_total_ret$Open_tsla)
cor(x=all_prices_total_ret$Open_gld,y=all_prices_total_ret$Open_amz)
cor(x=all_prices_total_ret$Open_appl,y=all_prices_total_ret$Open_amz)

#let's check with price 
all_prices_only_total <- bitcoin %>% select(date,Open) 
all_prices_only_total <- right_join(all_prices_only_total, all_prices, by='date') %>% mutate(Open=as.numeric(Open))
view(all_prices_only_total)

log(all_prices_total$Open)
typeof(all_prices_only_total$Open)

all_prices_only_total %>% ggplot(aes(x=date)) + geom_line(aes(y=log(Open)), colour="red") + geom_line(aes(y=(log(Open_tsla)+log(Open_appl)+log(Open_gld)+log(Open_fb)+log(Open_amz))/5), colour="green")

#crypto currencies

dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/DOGE-USD.csv", dl, method = "curl")
doge <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
doge <- doge %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17') %>% filter(Open!="null") %>% mutate(Open_doge= as.numeric(Open)) %>% select(date,Open_doge)


dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/ETH-USD.csv", dl, method = "curl")
ethereum <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
ethereum  <- ethereum %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17') %>% filter(Open!="null") %>% mutate(Open_eth= as.numeric(Open)) %>% select(date,Open_eth)
view(ethereum)

dl <- tempfile()
download.file("https://raw.githubusercontent.com/MatthieuvdSlikke/DataScience_Crytpo_HarvardX-ML/main/DataSets/LTC-USD.csv", dl, method = "curl")
litecoin <- read.csv(file = dl, header = TRUE, stringsAsFactors = FALSE)
litecoin <- litecoin %>% mutate(date=as.Date(Date)) %>% filter(date >= '2015-01-01') %>%
  filter(date <= '2021-04-17')  %>% filter(Open!="null") %>% mutate(Open_ltc= as.numeric(Open)) %>% select(date,Open_ltc)

crypto <- right_join(doge, ethereum, by='date') %>% left_join(.,litecoin, by='date')
crypto <- right_join(bitcoin%>%select(date,Open), crypto, by='date')
view(crypto)
cor(x=log(crypto$Open),y=log(crypto$Open_doge))
cor(x=log(crypto$Open),y=log(crypto$Open_eth))
cor(x=log(crypto$Open),y=log(crypto$Open_ltc))

#they almost have the exact same line 
crypto %>% ggplot(aes(x=date)) + geom_line(aes(y=log(Open)), colour="red") + geom_line(aes(y=(log(Open_doge)+log(Open_eth)+log(Open_ltc))/3), colour="green")

