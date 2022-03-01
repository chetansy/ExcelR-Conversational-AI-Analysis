####################################################### Loading Required Libraries #########################################################

library(plyr)
library(tidyr)
library(doParallel)
library(tidyr)
library(tm)
library(wordcloud)
library(RWeka)

####################################################### Setting up Working Directory and LOading Given Data Sets ##########################

setwd("D:/Data Science Project/Conversation Mining/Chat Transcripts for Project")
folder <- "D:/Data Science Project/Conversation Mining/Chat Transcripts for Project"
file_names <-list.files()
file_paths <- file.path(folder, file_names)
files <- lapply(file_paths, readLines)
full_data <- data.frame(ldply(files, cbind))
colnames(full_data) <- c("V1")
sum(is.na(full_data))
full_data[full_data==""]<-NA # replacing empty cells with NA
sum(is.na(full_data))
#library(tidyr)
full_data <- drop_na(full_data)
full_data[1:20,1] #Checking first 14 observations

####################################################### Separating Structured Data of 14 Variables from fill_data #########################

#library(doParallel)
registerDoParallel(cores=4)

#Running Loop to Separating First 14 Observations From Each Record

df1 <- data.frame(full_data[1:14,1])
colnames(df1) <- c("V1")
condition <- full_data$V1 == full_data$V1[21]
for (j in 1:2301770){
  if (condition[j]) {
    for (i in 1:14){ 
      df1[j+i,1] <- full_data[j+i,1]
    }
  }
}

# Separating Data Into Two Columns as Variable (A) and Observation (B)
#library(tidyr)
df2 <- drop_na(df1)
df3 <- df2 %>% separate(V1, c("A", "B","C","D"),":")
df3[is.na(df3)] <- " "

l <- 1
while(l < 1789536){
  df3$B[l] <- paste(df3$B[l],df3$C[l],df3$D[l],sep = ":")
  l=l+14
}
df3 <- df3[,1:2]

# Separating Each Record Into Each Column
df4 <- data.frame()
k <- 0
for (i in 1:(nrow(df2)/14)){
  for (j in 1:14){
    df4[j,i] <- df3$B[k+j]
  }
  k=k+14
}

# taking Transpose of the Data to Get Structured Data Frame

structured_data <- data.frame(t(df4))

# Changing Column Namess
colnames(structured_data) <- c( "Timestamp","Unread","Visitor_ID","Visitor_Name","Visitor_Email","Visitor_Notes","IP",
                                "Country_Code","Country_Name", "Region","City","User_Agent","Platform","Browser"  ) 

# Convering First Column Into DataTime
structured_data$Timestamp<-gsub("[a-zA-Z]","",structured_data$Timestamp)
structured_data$Timestamp <- as.POSIXct(structured_data$Timestamp,format = "%Y-%m-%d%H:%M:%S")
structured_data <- data.frame(lapply(structured_data, trimws), stringsAsFactors = FALSE)
write.csv(structured_data,"D:/Data Science Project/Conversation Mining/structured_data.csv", row.names = FALSE)

####################################################### Separating Unstructured Data of 14 Variables from fill_data #########################

df5 <- data.frame(full_data[1:2301771,1])

num_na <- dim(df5)[1]-dim(df1)[1]
df6 <- cbind.data.frame(X=df5[1],Y=c(df1$V1,rep(NA,num_na)))

colnames(df6) <- c("X","Y")
df7 <- anti_join(df6,df6,by=c("X"="Y")) 
chat_transcript <- data.frame(df7[,-2])
write.csv(chat_transcript,"D:/Data Science Project/Conversation Mining/chat_transcript.csv", row.names = FALSE)

