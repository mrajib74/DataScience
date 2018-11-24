library(twitteR)
library(tm)
setwd("D:\\Rajib\\XLRI\\Textmining\\Project")
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")


consumer_key="xxxxxxxxxxxx"
consumer_secret="vvvvvvvvvvvvvvvvvvv"
access_token="791122032-bbbbbbbbbbbbbbbb"
access_secret="dddddddddddddddddddddddddd"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

result = searchTwitter("#IPL2018",since='2018-05-01',  n=10000, resultType = 'recent')
r1= twListToDF(result)
write.csv(r1,file="IPL2018.csv")

	