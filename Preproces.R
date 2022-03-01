library(tidyr)
library(readtext)
library(lubridate)
library(SnowballC)
library(parsedate)
library(dplyr)
library(plyr)
library(tm)
library(hunspell)
library(wordcloud)

setwd("C:/Users/Chetan Yewale/Downloads/chatd")
folder <- "C:/Users/Chetan Yewale/Downloads/chatd"
file_names <-list.files()
file_paths <- file.path(folder, file_names)
files <- lapply(file_paths, read.delim,head=F)
full_data <- (ldply(files, cbind))


df1 <- data.frame(full_data[1:14,1])
colnames(df1) <- c("V1")

for (j in 1:100009){
                    if (full_data$V1[j] == full_data$V1[21]) {
                                                        for (i in 1:14){ 
                                                                df1[j+i,1] <- full_data[j+i,1]
                                                                          }
                                                                }
                    }

###############
df2 <- drop_na(df1)
df3 <- df2 %>% separate(V1, c("A", "B","C","D"),":")
df3[is.na(df3)] <- " "


l <- 1
while(l < 3766){
  df3$B[l] <- paste(df3$B[l],df3$C[l],df3$D[l],sep = ":")
  l=l+14
}

df3 <- df3[,1:2]

df4 <- data.frame()

k <- 0
for (i in 1:(nrow(df2)/14)){
                              for (j in 1:14){
                                                df4[j,i] <- df3$B[k+j]
                                              }
                              k=k+14
                            }

structured_data <- data.frame(t(df4))
colnames(structured_data) <- c( "Timestamp","Unread","Visitor ID","Visitor Name","Visitor Email","Visitor Notes","IP","Country Code","Country Name", "Region","City","User Agent","Platform","Browser"  )  

df5 <- data.frame(full_data[1:5000,1])
colnames(df5) <- c("V1")

df6 <- anti_join(df5,df1,by= "V1")


chat_transcript <- data.frame(df6)
str(chat_transcript)

#########
chat_transcript<- read.csv("C:/Users/Chetan Yewale/Downloads/chat_transcript1.csv")
chat_transcript<- data.frame(chat_transcript[1:300000,])
colnames(chat_transcript) <- c("V1")

df12<- data.frame(chat_transcript[!grepl("*Visitor",chat_transcript$V1),])

df7 <- chat_transcript %>% separate(V1, c("A", "B"),": ",fill = "left")
df7 <- data.frame(df7$B)
colnames(df7) <- c("V1")

##################

df8 <- data.frame(df7[!grepl("======*", df7$V1),])
colnames(df8) <- c("V1")

df8 <-  data.frame(df8[!grepl("Thank you for connecting with ExcelR!*", df8$V1),])
colnames(df8) <- c("V1")

df8 <-  data.frame(df8[!grepl("Hello, how may I be of assistance to you?", df8$V1),])
colnames(df8) <- c("V1")
str(df8$V1)

###############
chat_transcript<- as.character(df8$V1)
str(chat_transcript)

chatscripts<- Corpus(VectorSource(chat_transcript))
inspect(chatscripts[1:3])

## Data Cleansing
chatscripts_1<- tm_map(chatscripts,tolower)
chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
#chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
stopwords('SMART')
inspect(chatscripts_1[1:3])
chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)
#chatscripts_1<- tm_map(chatscripts_1,stemDocument)
inspect(chatscripts_1[1:3])
chatscripts_1<- tm_map(chatscripts_1,content_transformer(gsub),pattern="clarif",replacement="clarify")
chatscripts_1<- tm_map(chatscripts_1,content_transformer(gsub),pattern ="exclus",replacement ="exclusive")
chatscripts_1<- tm_map(chatscripts_1,content_transformer(gsub),pattern="posit",replacement = "positive")
chatscripts_1<- tm_map(chatscripts_1,content_transformer(gsub),pattern="certifi",replacement="certificate")
chatscripts_1<- tm_map(chatscripts_1,content_transformer(gsub),pattern="institut",replacement="institute")


inspect(chatscripts_1[1:3])

## DocumentTermMatrix
dtm<- DocumentTermMatrix(chatscripts_1)
inspect(dtm)

freq<- colSums(as.matrix(dtm))
length(freq)

barplot(freq,las= 3)

##remov Words
dtmr<- DocumentTermMatrix(chatscripts_1,control = list(wordLengths = c(4,7)))
freqr<- colSums(as.matrix(dtmr))
length(freqr)
str(freqr)

word_subset<- subset(freqr,freqr >= 20)

##plot for majority occurance words
barplot(word_subset,las= 3,col = rainbow(20))

sort<- order(word_subset,decreasing = TRUE)
word_subset[head(sort)]
word_subset[tail(sort)]
word_subset[sort]

df<- data.frame(words = names(word_subset), freq = word_subset)

library(wordcloud2)
wordcloud2(data = df,size = 0.4,shape = 'circle')

findFreqTerms(dtmr,lowfreq = 50)
findAssocs(dtmr,"excelr",0.4)
