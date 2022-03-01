library(tidytext)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tm)
library(tidyr)
library(RWeka)
library(topicmodels)


chat_transcript<- read.csv("C:/Users/Chetan Yewale/Downloads/chat_transcript1.csv")
colnames(chat_transcript)<- c("V1")
str(chat_transcript)

###### EXTRACTING VISITOR DATA ######

visitor<-data.frame(grep(chat_transcript$V1,pattern = "Visitor (.*)",value = T))%>% set_colnames(c("chat"))
df13<- visitor %>% separate(chat, c("A", "B"),": ",fill = "left")

chat_transcript<- data.frame(df13[,2])
str(chat_transcript)

chat_transcript$df13...2.<- as.character(chat_transcript$df13...2.)
str(chat_transcript)
colnames(chat_transcript)<- c("V1")

chatscripts<- Corpus(VectorSource(chat_transcript$V1))
inspect(chatscripts[1:3])


#Function containing regex pattern to remove email id
RemoveEmail <- function(x) {
  require(stringr)
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
} 

library(tm)
chatscripts <- tm_map(chatscripts,content_transformer(RemoveEmail))


#data cleansing
chatscripts_1<- tm_map(chatscripts,tolower)
chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
chatscripts_1<- tm_map(chatscripts_1,removeWords,c(stopwords("english"),"mail","email","ananya","give","send"))
chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)

### DTM
dtmr1<- DocumentTermMatrix(chatscripts_1,control = list(wordLengths = c(3,15)))
freqr1<- colSums(as.matrix(dtmr1))
length(freqr1)

#################

### Bigram
minfreq_bigram<-2

token_delim<- " \\t\\r\\n.!?,;\"()"
bitoken<- NGramTokenizer(chatscripts_1,weka_control(min=2,max=2, delimiters = token_delim))
two_word<- data.frame(table(bitoken))
sort_two<- two_word[order(two_word$Freq,decreasing = TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order = FALSE,scale = c(2,0.35),min.freq = minfreq_bigram)


##more than 250 
word_subset1<- subset(freqr1,freqr1 >= 250)

## SORTING
sort<- order(word_subset1,decreasing = TRUE)

barplot(word_subset1[sort],las= 3,col = rainbow(20))

df14<- data.frame(words = names(word_subset1), freq = word_subset1)
sort1<- df14[order(df14$freq,decreasing = T),]
wordcloud2(data = sort1,size = 0.7,shape = 'star')


#### ALL WORDS ####
df15<- data.frame(word = names(freqr1),freq = freqr1)
wordcloud2(data = df15,size = 0.7,shape = 'star')
##wordcloud(word = names(freqr1),freq = freqr1)

########

rowTotals<-apply(dtmr1,1,sum)
dtm.new<-dtmr1[rowTotals>0,]

################ LDA ##################
lda<-LDA(dtm.new,5)
term<-terms(lda,10)
term

# 5 Topics 
td_beta<- tidy(lda)
td_beta %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term,beta ,fill = topic)) +
  geom_col(show.legend = F) +
  facet_wrap(~topic,scales = "free")+
  coord_flip()

###### LDA - VEM Method #######
vem.lda <- LDA(dtm.new, 5, method="VEM")
lda.topics <- as.matrix(topics(article.lda))
lda.topics
lda.terms <- terms(vem.lda,10)
lda.terms

td_beta1<- tidy(vem.lda)
td_beta1 %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(term = reorder_within(term,beta,topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic) )) +
  geom_col(show.legend = F) +
  facet_wrap(~topic,scales = "free")+
  coord_flip()+
  scale_x_reordered()

####################### CORRELATED TOPIC MODELLING ###################################
ctm<- CTM(dtm.new,5)
term_ctm<- terms(ctm,10)
term_ctm

td_beta_ctm<- tidy(ctm)
td_beta_ctm %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(term = reorder_within(term,beta,topic)) %>%
  ggplot(aes(term,beta ,fill = topic)) +
  geom_col(show.legend = F) +
  facet_wrap(~topic,scales = "free")+
  coord_flip()+
  scale_x_reordered()

##########  EXTRACTING BOT DATA ##########

chat_transcript_1<- read.csv("C:/Users/Chetan Yewale/Downloads/chat_transcript_upp.csv")
colnames(chat_transcript_1)<- c("V1")
str(chat_transcript_1)


unstru_data<- data.frame(chat_transcript_1[!apply(chat_transcript_1 == "", 1, all),])
colnames(unstru_data)<- c("V1")

################ BOT CONVERSATION ###################################
bot_convo<- data.frame(unstru_data[!grepl("*Visitor",unstru_data$V1),]) %>% set_colnames(c("V1"))
bot_convo <- bot_convo %>% separate(V1, c("A", "B"),": ",fill = "left")


###### EXTRACTING VISITOR DATA ######

bot<- data.frame(chat_transcript_1[!grepl("*Visitor",chat_transcript_1$V1),])
colnames(bot)<- c("V1")
df16<- bot %>% separate(V1, c("A", "B"),": ",fill = "left")
str(df16)

df17<- as.character(df16[,2])
str(df17)

df17<- data.frame(df17)

###########
df18 <- data.frame(df17[!grepl("======*", df17$df17),])
colnames(df18) <- c("V1")

df18 <-  data.frame(df18[!grepl("Thank you for connecting with ExcelR!*", df18$V1),])
colnames(df18) <- c("V1")

df18 <-  data.frame(df18[!grepl("Hello, how may I be of assistance to you?", df18$V1),])
colnames(df18) <- c("V1")
str(df18$V1)

#########
chat_transcript_2<- as.character(df18$V1)

##### CREATING CORPUS #####
bot_corpus<- Corpus(VectorSource(chat_transcript_2))
inspect(bot_corpus[1:3])


####### PRE PROCESSING #######
chatscripts_bot<- tm_map(bot_corpus,tolower)
chatscripts_bot<- tm_map(chatscripts_bot,removePunctuation)
chatscripts_bot<- tm_map(chatscripts_bot,removeWords,c(stopwords("SMART"),"back","months","month","back"))
chatscripts_bot<- tm_map(chatscripts_bot,removeNumbers)
chatscripts_bot<- tm_map(chatscripts_bot,stripWhitespace)
inspect(chatscripts_bot[1:3])

#### DOCUMENT TERM MATRIX ####
dtm_bot<- DocumentTermMatrix(chatscripts_bot,control = list(wordLengths = c(3,9)))
freq_bot<- colSums(as.matrix(dtm_bot))
length(freq_bot)

#### More than words with frequency of 50
word_subset2<- subset(freq_bot ,freq_bot >= 50)
sort_2<- order(word_subset2,decreasing = TRUE)

word_subset2[sort_2]

df19<- data.frame(word = names(word_subset2[sort_2]),frequency = word_subset2)
### WORDCLOUD2 FOR BOT ###
wordcloud2(df19,size = 0.7, shape = "circle")

####### LDA ######

rowTotals_bot<-apply(dtm_bot,1,sum)
dtm.new_bot<-dtm_bot[rowTotals_2>0,]

lda_bot<-LDA(dtm.new_bot,5)
term_bot<-terms(lda_bot,10)
term_bot

### 5 Topics 
td_beta_bot<- tidy(lda_bot)
td_beta_bot %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term,beta ,fill = topic)) +
  geom_col(show.legend = F) +
  facet_wrap(~topic,scales = "free")+
  coord_flip()

#####################################  BIGRAMM #####################################################
bigrams <- data.frame(text = sapply(corpus_clean, as.character), stringsAsFactors = FALSE) %>%  unnest_tokens(bigram, text, token = "ngrams", n = 2)
frequency1<- rowSums(bigrams)
length(frequency1)
