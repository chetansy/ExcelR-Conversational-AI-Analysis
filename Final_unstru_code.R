library(tidyr)
library(lubridate)
library(SnowballC)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)
library(tidytext)
library(wordcloud2)
library(ggplot2)
library(magrittr)
library(topicmodels)


## Set path for your working location
setwd("E:/Data Science/R Programs/Projct chatbot/Chat")

## unzipped it the file
a<- unzip("E:/Data Science/R Programs/Projct chatbot/Chat1.zip")

folder <- "E:/Data Science/R Programs/Projct chatbot/Chat"
file_names <-list.files()
file_paths <- file.path(folder, file_names)
files <- lapply(file_paths, readLines)
full_data <- (ldply(files, cbind))

################################ UNSTRUCTURED DATA #####################################

a<- data.frame(full_data)
colnames(a)<- c("V1")

df1 <- data.frame(a[!grepl("Timestamp", a$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Unread", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Visitor ID", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Visitor Name", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Visitor Email", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Visitor Notes", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("IP", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Country Code", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Country Name", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Region", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("City", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("User Agent", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Platform", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("Browser", df1$V1),]) %>% set_colnames(c("V1"))
df1 <- data.frame(df1[!grepl("======*", df1$V1),]) %>% set_colnames(c("V1"))


unstru_data<- data.frame(df1)
unstru_data<- data.frame(unstru_data[!apply(unstru_data == "", 1, all),])
colnames(unstru_data)<- c("V1")

################ BOT CONVERSATION ###################################
bot_convo<- data.frame(unstru_data[!grepl("*Visitor",unstru_data$V1),]) %>% set_colnames(c("V1"))
bot_convo <- bot_convo %>% separate(V1, c("A", "B"),": ",fill = "left")
#bot_convo <- data.frame(bot_convo$B) %>% set_colnames(c("V1"))

############### VISITOR CONVERSATION ################################
visitor_convo<-data.frame(grep(unstru_data$V1,pattern = "Visitor (.*)",value = T))%>% set_colnames(c("chat"))
visitor_convo<- visitor_convo %>% separate(chat, c("A", "B"),": ",fill = "left")

visitor_convo<- data.frame(visitor_convo[,2]) %>% set_colnames(c("chat"))
str(visitor_convo)

visitor_convo$chat<- as.character(visitor_convo$chat)
str(visitor_convo)

#################################  STRUCTURED DATA ######################################

struct_data<- anti_join(a,unstru_data,by = "V1")
struct_data<- data.frame(struct_data[!apply(struct_data == "", 1, all),])
colnames(struct_data)<- c("V1")

struct_data <- data.frame(struct_data[!grepl("======*", struct_data$V1),]) %>% set_colnames(c("V1"))

sep_data <- struct_data %>% separate(V1, c("A", "B"),": ")
sep_data<- sep_data %>% separate(A, c("A","c"),":")

sep_data[is.na(sep_data)] <- ""

sep_data$B<- paste(sep_data$c,sep_data$B)
sep_data<- data.frame(sep_data[,-2])

##################################################################
############ WORKING ON UNSTRUCTURED DATA ########################

bot_convo<- data.frame(unstru_data[!grepl("*Visitor",unstru_data$V1),]) %>% set_colnames(c("V1"))
bot_convo <- bot_convo %>% separate(V1, c("A", "B"),": ",fill = "left")
bot_convo<- data.frame(bot_convo[,2]) %>% set_colnames(c("chat"))

###########
bot_convo <-  data.frame(bot_convo[!grepl("======*", bot_convo$chat),]) %>% set_colnames(c("chat"))
bot_convo <-  data.frame(bot_convo[!grepl("Thank you for connecting with ExcelR!*", bot_convo$chat),]) %>% set_colnames(c("chat"))
bot_convo <-  data.frame(bot_convo[!grepl("Hello, how may I be of assistance to you?", bot_convo$chat),]) %>% set_colnames(c("chat"))

#########
chat_transcript_2<- as.character(bot_convo$chat)

##### CREATING CORPUS #####
bot_corpus<- Corpus(VectorSource(chat_transcript_2))

####### PRE PROCESSING #######
chatscripts_bot<- tm_map(bot_corpus,tolower)
chatscripts_bot<- tm_map(chatscripts_bot,removePunctuation)
chatscripts_bot<- tm_map(chatscripts_bot,removeWords,c(stopwords("SMART"),"back","months","month","back"))
chatscripts_bot<- tm_map(chatscripts_bot,removeNumbers)
chatscripts_bot<- tm_map(chatscripts_bot,stripWhitespace)

#### DOCUMENT TERM MATRIX ####
dtm_bot<- DocumentTermMatrix(chatscripts_bot,control = list(wordLengths = c(3,9)))
freq_bot<- colSums(as.matrix(dtm_bot))
length(freq_bot)

#### More than words with frequency of 50
word_subset2<- subset(freq_bot ,freq_bot >= 50)
sort_2<- order(word_subset2,decreasing = TRUE)
df2<- data.frame(word = names(word_subset2[sort_2]),frequency = word_subset2)
### WORDCLOUD2 FOR BOT ###
wordcloud2(df2,size = 0.7, shape = "circle")
barplot(word_subset2[1:20],las= 3,col = rainbow(20))


####################  WITH TF-IDF  ###################
tfidf_bot4<- DocumentTermMatrix(chatscripts_bot,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))
freq_bot4<- colSums(as.matrix(tfidf_bot4))

#### More than words with frequency of 50
word_subset4<- subset(freq_bot4,freq_bot4 >= 20)
## SORTING
sort4<- order(word_subset4,decreasing = TRUE)
df3<- data.frame(words = names(word_subset4), freq = word_subset4)
sort_df3<- df3[order(df3$freq,decreasing = T),]
wordcloud2(data = sort_df3,size = 0.2,shape = 'star')
barplot(word_subset4[1:20],las= 3,col = rainbow(20))


#########################################################################################
####### VISITOR MINING #########

chatscripts<- Corpus(VectorSource(visitor_convo$chat))
#data cleansing
chatscripts_1<- tm_map(chatscripts,tolower)
chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
chatscripts_1<- tm_map(chatscripts_1,removeWords,c(stopwords("english"),"mail","visitor","ananya","give","send","mounica","patel"))
chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)

##### DTM ########
dtmr1<- DocumentTermMatrix(chatscripts_1,control = list(wordLengths = c(3,15)))
freqr1<- colSums(as.matrix(dtmr1))
word_subset1<- subset(freqr1,freqr1 >= 20)
## SORTING
sort<- order(word_subset1,decreasing = TRUE)
df4<- data.frame(words = names(word_subset1), freq = word_subset1)
sort4<- df4[order(df4$freq,decreasing = T),]
wordcloud2(data = sort4,size =0.4,shape = 'star')
barplot(word_subset1[1:20],las= 3,col = rainbow(20))

########### TOPIC MINING ##############
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
  mutate(term = reorder_within(term,beta,topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme(panel.background = element_blank())+
  coord_flip() +
  scale_x_reordered()


#####  CTM  ######
ctm<- CTM(dtm.new,5)
term_ctm<- terms(ctm,10)
term_ctm

td_beta_ctm<- tidy(ctm)
td_beta_ctm %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(term = reorder_within(term,beta,topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme(panel.background = element_blank())+
  coord_flip() +
  scale_x_reordered()

######### WORKING WITH TF-IDF ############

tfidf_visitor<- DocumentTermMatrix(chatscripts_1,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))
freqr3<- colSums(as.matrix(tfidf_visitor))
word_subset3<- subset(freqr3,freqr3 >= 20)
## SORTING
sort<- order(word_subset3,decreasing = TRUE)
df5<- data.frame(words = names(word_subset3), freq = word_subset3)
sort5<- df5[order(df5$freq,decreasing = T),]
wordcloud2(data = sort5,size = 0.3,shape = 'star')
barplot(word_subset3[1:20],las= 3,col = rainbow(20))

##########################
#########################


df113 <- data.frame()
for (i in 1:(nrow(sep_data)/14)){
  for (j in 1:14){
    df112[j,i] <- sep_data$B[0+j]
  }
  k=k+14
}


a<- nrow(struct_data)/14

