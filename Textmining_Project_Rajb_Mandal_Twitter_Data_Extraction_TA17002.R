library(twitteR)
library(tm)
setwd("D:\\Rajib\\XLRI\\Textmining\\Project")
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")


consumer_key="QvbEsXCrehMDQALjHGIQj19f3"
consumer_secret="dmcm4Op8q6sbjf4RfEdZKa3OEAIMfbyZCghAn2Gj3JBZZfkXA0"
access_token="791122032-WpL9pWaNqMaOmCOEDLdPCLvYYWjJcdAI8ig9H56V"
access_secret="kkyYmPchDRamsvOTZFBASJpYQ5TMZTZsu34Bj1V251kbN"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

result = searchTwitter("#IPL2018",since='2018-05-01',  n=10000, resultType = 'recent')
r1= twListToDF(result)
write.csv(r1,file="IPL2018.csv")

	