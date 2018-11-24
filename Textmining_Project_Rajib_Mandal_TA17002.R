options(java.parameters = "- Xmx1024m")
library(RSentiment)
library(NLP)
library(e1071)
library(caTools)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(stringi)
library(tidytext)
library(dplyr)
library(reshape2)
library(ggplot2)
library(caret)
setwd("D:/Rajib/XLRI/Textmining/Project/05222018/TEXT_MINING_PROJECT_TA17002")

input<- read.csv(file="IPL-2018-11.csv", header=TRUE)
set.seed(1234)

####################################################
## Remove specialcharcters & newline from the revieW
####################################################
input$text<-gsub("[\r\n]", "", input$text)

input<-unique(input$text)
#input
###################################################
##Cleaning Twiter data
###################################################


# Remove Non-English Tweets 
cleanTweet <- sapply(input,function(row) iconv(row, "latin1", "ASCII", sub=""))

cleanTweet<-gsub("&amp", "", cleanTweet)
#remove retweet
cleanTweet<-gsub("(RT|via|rt)((?:\\b\\W*@\\w+)+)", "", cleanTweet)
cleanTweet<-gsub("@\\w+", "", cleanTweet)
cleanTweet<-gsub("[[:punct:]]", "", cleanTweet)
cleanTweet<-gsub("[[:digit:]]", "", cleanTweet)
# Remove Hashtags
cleanTweet<-gsub("#[a-z,A-Z]*", "", cleanTweet)
# remove url
cleanTweet<-gsub("http\\w+", "", cleanTweet)
cleanTweet<-gsub("[ \t]{2,}", "", cleanTweet)
cleanTweet<-gsub("^\\s+|\\s+$", "", cleanTweet) 
cleanTweet<-gsub("<ed>", "", cleanTweet) 
# remove user& target
cleanTweet<-gsub("@[a-z A-Z 0-9]*", "", cleanTweet) 
cleanTweet<-trimws(gsub("^\\s*<U\\+\\w+>|-", " ",cleanTweet))
# remove repeated characters
cleanTweet<-gsub('([[:alpha:]])\\1+', '\\1',cleanTweet)

# remove emojis or special characters
cleanTweet <- gsub('<.*>', '', enc2native(cleanTweet))



ipltweetCorpus<-VCorpus(VectorSource(cleanTweet))
ipltweetCorpus<-tm_map(ipltweetCorpus, tolower)
ipltweetCorpus<-tm_map(ipltweetCorpus,removePunctuation)
# remove numbers
ipltweetCorpus<-tm_map(ipltweetCorpus,removeNumbers)
ipltweetCorpus<-tm_map(ipltweetCorpus, PlainTextDocument)
# Remove Stopwords
ipltweetCorpus<-tm_map(ipltweetCorpus, removeWords, stopwords("en"))

cleaned.df<-data.frame(text=unlist(sapply(ipltweetCorpus, `[`, "content")),stringsAsFactors=F)
write.csv(cleaned.df, file = "ipl2018.txt")


ipltweet<-readLines("ipl2018.txt", n=-1)
totalentimentpresence<-calculate_total_presence_sentiment(ipltweet)
head(totalentimentpresence)
#############################
#Generate  Pie Chart
#############################
dfplot<- as.data.frame(t(totalentimentpresence))
colnames(dfplot) <- c("class", "freq")
dfplot <-dfplot[dfplot$freq!=0,]
dfplot
dfplot <- dfplot %>% 
  group_by(class) %>% 
  ungroup() %>% 
  mutate(per=as.numeric(as.character(freq))/sum(as.numeric(as.character(freq)))) %>% 
  arrange(desc(class))
dfplot$label <- scales::percent(dfplot$per)

ggplot(data=dfplot)+
  geom_bar(aes(x="", y=per, fill=class), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

#############################################################
sentmentscore<-calculate_score(ipltweet)
totalsentiment<-calculate_sentiment(ipltweet)
dfsentiment<-as.data.frame(cbind(ipltweet,totalsentiment))
dfsentimentpredmodel<-dfsentiment

ipltweetdata <- unnest_tokens(dfsentiment, input = text,
                              output = line, token = "sentences", to_lower = T)

ipltweetdata$lineNo <- seq_along(ipltweetdata$line)
ipltweetdatatextToWord <- ipltweetdata %>% 
  unnest_tokens(output = word, input = line, token = "words")




########################################################
##Plot for unique word in tweet
####################################################

###Customst0p word
custom_stop_words <- bind_rows(data_frame(word = c("miss","wil","mi","xi","kl","hai","stoinis","dvscsk"
                                                   ,"cricket","season","wicket","fel","de","wining","india"
                                                   ,"daredevils","runs","stoins","kr","mum","indian"
                                                   ,"kxip","csk","mumbaiindians","yuvraj","delhi",
                                                   "al","tye","ipl","vivoipl","patel","chenai","rcb"
                                                   ,"dvcsk","mivkxip","team","time","indiansov"
                                                   ,"ben","bal","tos","wel","mivskxip","game"
                                                   ,"Kings","indians","super","kings","yuvi"
                                                   ,"mumbai","god","match","player",
                                                   "yadav","csk","sharma","rahul","polard"
                                                   ,"bumrah","punjab","pandya","andrew","se"
                                                   ,"captain","rohit","pant","ashwin"
                                                   ,"maxwel","singh","whistlepodu","ab"
                                                   ,"rayudu","inings","bowling","playofs"
                                                   ,"iplt","kxipvmi","lungi","gayle","rcbvsrh","srh"), 
                                          lexicon = c("custom")), 
                               stop_words)
# plot the top 20 words 
ipltweetdatatextToWord %>%
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency",
       x = "words",
       title = "Top 25  words found in tweets"
       )

################################################################
##Comparison Cloud



str(ipltweetdatatextToWord)
ipltweetdatatextToWord %>%
  left_join(get_sentiments("bing")) %>%
   count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80","red","green","blue"),
                   max.words = 100)

##############################################################

##############################################################
# Most common positive and negative words
#############################################################

bing_word_counts <- ipltweetdatatextToWord %>%
  anti_join(custom_stop_words) %>%
  left_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_word_counts %>%
  
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#############################################################
##Very Negtive

##############################################################
#####Tokenize into lines.
documentsLines <- unnest_tokens(dfsentiment[dfsentiment$sentiment== "Very Negative", ], input = text, output = line, token = "sentences", to_lower = F)
documentsLines$lineNo <- seq_along(documentsLines$line)
#head(documentsLines)

####Tokenize into words (unigrams)

dfsentimenttextToWord <- documentsLines %>% 
  unnest_tokens(output = word, input = line, token = "words")

head(dfsentimenttextToWord)

##Removing stopwords: the stop_words dataset and the anti_join function
dfsentimenttextToWordTemp <- dfsentimenttextToWord %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

###Summarizing word frequencies: the count function
dfsentimenttextToWordTemp %>%
  count(word, sort = TRUE)


bingwordcounts <- dfsentimenttextToWord %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bingwordcounts


##############################################
# Word cloud for very negative word

############################################

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Very Negative")

dfsentimenttextToWord %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 random_color=TRUE,
                 colors = brewer.pal(4, "Dark2"),
                 random.order = FALSE, main="Title"))


#############################################################
## Negtive

##############################################################
#####Tokenize into lines.
documentsLines <- unnest_tokens(dfsentiment[dfsentiment$sentiment== "Negative", ], input = text, output = line, token = "sentences", to_lower = F)
documentsLines$lineNo <- seq_along(documentsLines$line)
#head(documentsLines)

####Tokenize into words (unigrams)

dfsentimenttextToWord <- documentsLines %>% 
  unnest_tokens(output = word, input = line, token = "words")

head(dfsentimenttextToWord)

##Removing stopwords: the stop_words dataset and the anti_join function
dfsentimenttextToWordTemp <- dfsentimenttextToWord %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

###Summarizing word frequencies: the count function
dfsentimenttextToWordTemp %>%
  count(word, sort = TRUE)


bingwordcounts <- dfsentimenttextToWord %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bingwordcounts


##############################################
# Word cloud for negative word

############################################
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Negative")
dfsentimenttextToWord %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 random_color=TRUE,
                 colors = brewer.pal(4, "Dark2"),
                 random.order = FALSE, main="Title"))



#############################################################
## Neutral

##############################################################
#####Tokenize into lines.
documentsLines <- unnest_tokens(dfsentiment[dfsentiment$sentiment== "Neutral", ], input = text, output = line, token = "sentences", to_lower = F)
documentsLines$lineNo <- seq_along(documentsLines$line)
#head(documentsLines)

####Tokenize into words (unigrams)

dfsentimenttextToWord <- documentsLines %>% 
  unnest_tokens(output = word, input = line, token = "words")

head(dfsentimenttextToWord)

##Removing stopwords: the stop_words dataset and the anti_join function
dfsentimenttextToWordTemp <- dfsentimenttextToWord %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

###Summarizing word frequencies: the count function
dfsentimenttextToWordTemp %>%
  count(word, sort = TRUE)


bingwordcounts <- dfsentimenttextToWord %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bingwordcounts


##############################################
# Word cloud for Neutral word

############################################
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Neutral")
dfsentimenttextToWord %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 random_color=TRUE,
                 colors = brewer.pal(4, "Dark2"),
                 random.order = FALSE, main="Title"))


#############################################################
## Very Positive

##############################################################
#####Tokenize into lines.
documentsLines <- unnest_tokens(dfsentiment[dfsentiment$sentiment== "Very Positive", ], input = text, output = line, token = "sentences", to_lower = F)
documentsLines$lineNo <- seq_along(documentsLines$line)
#head(documentsLines)

####Tokenize into words (unigrams)

dfsentimenttextToWord <- documentsLines %>% 
  unnest_tokens(output = word, input = line, token = "words")

head(dfsentimenttextToWord)

##Removing stopwords: the stop_words dataset and the anti_join function
dfsentimenttextToWordTemp <- dfsentimenttextToWord %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

###Summarizing word frequencies: the count function
dfsentimenttextToWordTemp %>%
  count(word, sort = TRUE)

bingwordcounts <- dfsentimenttextToWord %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bingwordcounts


##############################################
# Word cloud for Very Positive word

############################################
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Very Positive")
dfsentimenttextToWord %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 random_color=TRUE,
                 colors = brewer.pal(4, "Dark2"),
                 random.order = FALSE, main="Title"))



#############################################################
##  Positive

##############################################################
#####Tokenize into lines.
documentsLines <- unnest_tokens(dfsentiment[dfsentiment$sentiment== "Positive", ], input = text, output = line, token = "sentences", to_lower = F)
documentsLines$lineNo <- seq_along(documentsLines$line)
#head(documentsLines)

####Tokenize into words (unigrams)

dfsentimenttextToWord <- documentsLines %>% 
  unnest_tokens(output = word, input = line, token = "words")

head(dfsentimenttextToWord)

##Removing stopwords: the stop_words dataset and the anti_join function
dfsentimenttextToWordTemp <- dfsentimenttextToWord %>%
  anti_join(custom_stop_words, by = c("word" = "word"))

###Summarizing word frequencies: the count function
dfsentimenttextToWordTemp %>%
  count(word, sort = TRUE)

bingwordcounts <- dfsentimenttextToWord %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bingwordcounts


##############################################
# Word cloud for  Positive word

############################################
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Positive")
dfsentimenttextToWord %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 random_color=TRUE,
                 colors = brewer.pal(4, "Dark2"),
                 random.order = FALSE, main="Title"))



#dfsentimentpredmodel$text
dfsentimentpredmodel$text
set.seed(1234)
corpoussentiment<-Corpus(VectorSource(dfsentimentpredmodel$text))

dfsentimentpredmodel$sentiment<-as.factor(dfsentimentpredmodel$sentiment)
trnsformcorpoussentiment<-tm_map(corpoussentiment,tolower)
trnsformcorpoussentiment<-tm_map(trnsformcorpoussentiment,removeNumbers)
trnsformcorpoussentiment<-tm_map(trnsformcorpoussentiment,removePunctuation)
trnsformcorpoussentiment<-tm_map(trnsformcorpoussentiment,removeWords,stopwords("english"))
trnsformcorpoussentiment<-tm_map(trnsformcorpoussentiment,stripWhitespace)
trnsformcorpoussentiment<-tm_map(trnsformcorpoussentiment,stemDocument)
dtmtrnsformcorpoussentiment<-DocumentTermMatrix(trnsformcorpoussentiment)
inspect(dtmtrnsformcorpoussentiment)
sparsedtm<-removeSparseTerms(dtmtrnsformcorpoussentiment,.99)

dfsparsedtm<-as.data.frame(as.matrix(sparsedtm))

ag<-cbind(dfsentimentpredmodel,dfsparsedtm)
dfsentimentpredmodel$text<-NULL

apl<-sample.split(dfsentimentpredmodel$sentiment,.7)
agtr<-subset(ag,apl==TRUE)
#summary(agtr)
agvl<-subset(ag,apl==FALSE)

nvclassifier<-naiveBayes(sentiment~.,data=agtr,laplace=2,na.action = na.pass)
#nvclassifier
pred<-predict(nvclassifier,newdata=agvl[,-2])
#pred
tb<-table(pred,agvl$sentiment)
tb
accuracy<-sum(diag(tb))/sum(tb)
accuracy
confusionMatrix(data = pred,agvl$sentiment)

