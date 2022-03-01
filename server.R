library(shinyjs)
library(wordcloud2)
library(reshape2)

shinyServer<- function(input,output,session){
  options(shiny.maxRequestSize=110*1024^2)
  
  data1 <- reactive ({ 
    req(input$file1)
    inFile1 <- input$file1
    file.rename(inFile1$datapath,paste(inFile1$datapath, ".csv", sep=""))
    full_data1 <- read.csv(paste(inFile1$datapath, ".csv", sep=""))
    return(full_data1)
                      })
  
  data2 <- reactive ({ 
    req(input$file2) 
    inFile2 <- input$file2 
    file.rename(inFile2$datapath,paste(inFile2$datapath, ".csv", sep=""))
    full_data2 <- read.csv(paste(inFile2$datapath, ".csv", sep=""))
    return(full_data2)
  })
  
  
  ################################
  
  output$contents1<- DT:: renderDataTable({
    data1()
  })
  
    
  output$contents2<- DT:: renderDataTable({
    
    full_data2 <- data2()
    unstru_data<- data.frame(full_data2)
    unstru_data <- data.frame(unstru_data[!grepl("======*", unstru_data$V1),]) %>% set_colnames(c("V1"))
    
    ################ BOT CONVERSATION ###################################
    bot_convo<- data.frame(unstru_data[!grepl("*Visitor",unstru_data$V1),]) %>% set_colnames(c("V1"))
    bot_convo <- bot_convo %>% separate(V1, c("A", "B"),": ",fill = "left")
    return(bot_convo)
  })
  
  output$contents3<- DT:: renderDataTable({
    full_data2 <- data2()
    unstru_data<- data.frame(full_data2)
    
    ################ VISITOR CONVERSATION ###################################
    visitor_convo<-data.frame(grep(unstru_data$V1,pattern = "Visitor (.*)",value = T))%>% set_colnames(c("chat"))
    visitor_convo<- visitor_convo %>% separate(chat, c("A", "B"),": ",fill = "left")
    return(visitor_convo)
  })

  ##############################
  
  
  
  ###################################### PLOT WORDCLOUD ################################################
  
  output$plot1<- renderWordcloud2({
    input$plot1buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    ################ VISITOR CONVERSATION ###################################
    visitor_convo<- data.frame(grep(unstru_data$V1,pattern = "Visitor (.*)",value = T)) %>% set_colnames(c("chat"))
    visitor_convo<- visitor_convo %>% separate(chat, c("A", "B"),": ",fill = "left")
    visitor_convo<- data.frame(visitor_convo[,2]) %>% set_colnames(c("chat"))
    
    visitor_convo$chat<- as.character(visitor_convo$chat)
    chatscripts<- Corpus(VectorSource(visitor_convo$chat))
    #data cleansing
    chatscripts_1<- tm_map(chatscripts,tolower)
    chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
    chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
    chatscripts_1<- tm_map(chatscripts_1,removeWords,c(stopwords("english"),"mail","visitor","ananya","give","send","mounica","patel"))
    chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
    chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)
    ### DTM
    dtmr1<- DocumentTermMatrix(chatscripts_1,control = list(wordLengths = c(3,15)))
    freqr1<- colSums(as.matrix(dtmr1))
    word_subset1<- subset(freqr1,freqr1 >= 20)
    ## SORTING
    sort<- order(word_subset1,decreasing = TRUE)
    df14<- data.frame(words = names(word_subset1), freq = word_subset1)
    sort1<- df14[order(df14$freq,decreasing = T),]
    wordcloud2(data = sort1,size =0.4,shape = 'star')

  })
  
  output$plot1.1<- renderPlot({
    input$plot1buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    ################ VISITOR CONVERSATION ###################################
    visitor_convo<- data.frame(grep(unstru_data$V1,pattern = "Visitor (.*)",value = T)) %>% set_colnames(c("chat"))
    visitor_convo<- visitor_convo %>% separate(chat, c("A", "B"),": ",fill = "left")
    visitor_convo<- data.frame(visitor_convo[,2]) %>% set_colnames(c("chat"))
    
    visitor_convo$chat<- as.character(visitor_convo$chat)
    chatscripts<- Corpus(VectorSource(visitor_convo$chat))
    #data cleansing
    chatscripts_1<- tm_map(chatscripts,tolower)
    chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
    chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
    chatscripts_1<- tm_map(chatscripts_1,removeWords,c(stopwords("english"),"mail","visitor","ananya","give","send","mounica","patel"))
    chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
    chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)
    ### DTM
    dtmr1<- DocumentTermMatrix(chatscripts_1,control = list(wordLengths = c(3,15)))
    freqr1<- colSums(as.matrix(dtmr1))
    word_subset1<- subset(freqr1,freqr1 >= 20)
    barplot(word_subset1[1:20],las= 3,col = rainbow(20))
  })
  
  ###### BOT WORDCLOUD ########
  
  output$plot2<- renderWordcloud2({
    input$plot2buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    bot<- data.frame(unstru_data[!grepl("*Visitor",unstru_data$V1),])
    colnames(bot)<- c("chat")
    bot<- bot %>% separate(chat, c("A", "B"),": ",fill = "left")
    bot<- data.frame(as.character(bot[,2]))
    colnames(bot)<- c("chat")
    
    ###########
    bot <- data.frame(bot[!grepl("======*", bot$chat),]) %>% set_colnames(c("chat"))
    bot <-  data.frame(bot[!grepl("Thank you for connecting with ExcelR!*", bot$chat),]) %>% set_colnames(c("chat"))
    bot <-  data.frame(bot[!grepl("Hello, how may I be of assistance to you?", bot$chat),]) %>% set_colnames(c("chat"))
    
    #########
    bot$chat <- as.character(bot$chat)
    
    ##### CREATING CORPUS #####
    bot_corpus<- Corpus(VectorSource(bot$chat))
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
    word_subset2<- subset(freq_bot,freq_bot >= 10)
    ## SORTING
    sort2<- order(word_subset2,decreasing = TRUE)
    df15<- data.frame(words = names(word_subset2), freq = word_subset2)
    sort_df<- df15[order(df15$freq,decreasing = T),]
    wordcloud2(data = sort_df,size = 0.4,shape = 'star')
            })

  ##########################
  
  output$plot2.1<- renderPlot({
    input$plot2buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    bot<- data.frame(unstru_data[!grepl("*Visitor",unstru_data$V1),])
    colnames(bot)<- c("chat")
    bot<- bot %>% separate(chat, c("A", "B"),": ",fill = "left")
    bot<- data.frame(as.character(bot[,2]))
    colnames(bot)<- c("chat")
    
    ###########
    bot <- data.frame(bot[!grepl("======*", bot$chat),]) %>% set_colnames(c("chat"))
    bot <-  data.frame(bot[!grepl("Thank you for connecting with ExcelR!*", bot$chat),]) %>% set_colnames(c("chat"))
    bot <-  data.frame(bot[!grepl("Hello, how may I be of assistance to you?", bot$chat),]) %>% set_colnames(c("chat"))
    
    #########
    bot$chat <- as.character(bot$chat)
    
    ##### CREATING CORPUS #####
    bot_corpus<- Corpus(VectorSource(bot$chat))
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
    word_subset2<- subset(freq_bot,freq_bot >= 10)
    ## SORTING
    sort2<- order(word_subset2,decreasing = TRUE)
    df15<- data.frame(words = names(word_subset2), freq = word_subset2)
    sort_df<- df15[order(df15$freq,decreasing = T),]
    barplot(word_subset2[1:20],las= 3,col = rainbow(20))
  })
  
  
  ###############################################################################
  output$plot3<- renderWordcloud2({
    input$plot3buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    ################ VISITOR CONVERSATION ###################################
    visitor_convo<- data.frame(grep(unstru_data$V1,pattern = "Visitor (.*)",value = T)) %>% set_colnames(c("chat"))
    visitor_convo<- visitor_convo %>% separate(chat, c("A", "B"),": ",fill = "left")
    visitor_convo<- data.frame(visitor_convo[,2]) %>% set_colnames(c("chat"))
    
    visitor_convo$chat<- as.character(visitor_convo$chat)
    chatscripts<- Corpus(VectorSource(visitor_convo$chat))
    #data cleansing
    chatscripts_1<- tm_map(chatscripts,tolower)
    chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
    chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
    chatscripts_1<- tm_map(chatscripts_1,removeWords,c(stopwords("english"),"mail","visitor","ananya","give","send","mounica","patel"))
    chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
    chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)
    ### DTM
    dtmr3<- DocumentTermMatrix(chatscripts_1,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))
    freqr3<- colSums(as.matrix(dtmr3))
    word_subset3<- subset(freqr3,freqr3 >= 20)
    ## SORTING
    sort<- order(word_subset3,decreasing = TRUE)
    df16<- data.frame(words = names(word_subset3), freq = word_subset3)
    sort13<- df16[order(df16$freq,decreasing = T),]
    wordcloud2(data = sort13,size = 0.3,shape = 'star')
    
  })
  ###########################################################################################
  
  
  output$plot3.1<- renderPlot({
    input$plot3buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    ################ VISITOR CONVERSATION ###################################
    visitor_convo<- data.frame(grep(unstru_data$V1,pattern = "Visitor (.*)",value = T)) %>% set_colnames(c("chat"))
    visitor_convo<- visitor_convo %>% separate(chat, c("A", "B"),": ",fill = "left")
    visitor_convo<- data.frame(visitor_convo[,2]) %>% set_colnames(c("chat"))
    
    visitor_convo$chat<- as.character(visitor_convo$chat)
    chatscripts<- Corpus(VectorSource(visitor_convo$chat))
    #data cleansing
    chatscripts_1<- tm_map(chatscripts,tolower)
    chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
    chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
    chatscripts_1<- tm_map(chatscripts_1,removeWords,c(stopwords("english"),"mail","visitor","ananya","give","send","mounica","patel"))
    chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
    chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)
    ### DTM
    dtmr3<- DocumentTermMatrix(chatscripts_1,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))
    freqr3<- colSums(as.matrix(dtmr3))
    word_subset3<- subset(freqr3,freqr3 >= 20)
    ## SORTING
    sort<- order(word_subset3,decreasing = TRUE)
    df16<- data.frame(words = names(word_subset3), freq = word_subset3)
    sort13<- df16[order(df16$freq,decreasing = T),]
    barplot(word_subset3[1:20],las= 3,col = rainbow(20))
    
  })
  
  
  ##########################################################################################
  output$plot4<- renderWordcloud2({
    input$plot4buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    bot<- data.frame(unstru_data[!grepl("*Visitor",unstru_data$V1),])
    colnames(bot)<- c("chat")
    bot<- bot %>% separate(chat, c("A", "B"),": ",fill = "left")
    bot<- data.frame(as.character(bot[,2]))
    colnames(bot)<- c("chat")
    
    ###########
    bot <- data.frame(bot[!grepl("======*", bot$chat),]) %>% set_colnames(c("chat"))
    bot <-  data.frame(bot[!grepl("Thank you for connecting with ExcelR!*", bot$chat),]) %>% set_colnames(c("chat"))
    bot <-  data.frame(bot[!grepl("Hello, how may I be of assistance to you?", bot$chat),]) %>% set_colnames(c("chat"))
    
    #########
    bot$chat <- as.character(bot$chat)
    
    ##### CREATING CORPUS #####
    bot_corpus<- Corpus(VectorSource(bot$chat))
    ####### PRE PROCESSING #######
    chatscripts_bot<- tm_map(bot_corpus,tolower)
    chatscripts_bot<- tm_map(chatscripts_bot,removePunctuation)
    chatscripts_bot<- tm_map(chatscripts_bot,removeWords,c(stopwords("SMART"),"back","months","month","back"))
    chatscripts_bot<- tm_map(chatscripts_bot,removeNumbers)
    chatscripts_bot<- tm_map(chatscripts_bot,stripWhitespace)
    
    #### DOCUMENT TERM MATRIX ####
    dtm_bot4<- DocumentTermMatrix(chatscripts_bot,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))
    freq_bot4<- colSums(as.matrix(dtm_bot4))
    
    #### More than words with frequency of 50
    word_subset4<- subset(freq_bot4,freq_bot4 >= 20)
    ## SORTING
    sort4<- order(word_subset4,decreasing = TRUE)
    df17<- data.frame(words = names(word_subset4), freq = word_subset4)
    sort_df4<- df17[order(df17$freq,decreasing = T),]
    wordcloud2(data = sort_df4,size = 0.2,shape = 'star')
  })
  
  #############################################################################
  
  output$plot4.1<- renderPlot({
    input$plot4buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    bot<- data.frame(unstru_data[!grepl("*Visitor",unstru_data$V1),])
    colnames(bot)<- c("chat")
    bot<- bot %>% separate(chat, c("A", "B"),": ",fill = "left")
    bot<- data.frame(as.character(bot[,2]))
    colnames(bot)<- c("chat")
    
    ###########
    bot <- data.frame(bot[!grepl("======*", bot$chat),]) %>% set_colnames(c("chat"))
    bot <-  data.frame(bot[!grepl("Thank you for connecting with ExcelR!*", bot$chat),]) %>% set_colnames(c("chat"))
    bot <-  data.frame(bot[!grepl("Hello, how may I be of assistance to you?", bot$chat),]) %>% set_colnames(c("chat"))
    
    #########
    bot$chat <- as.character(bot$chat)
    
    ##### CREATING CORPUS #####
    bot_corpus<- Corpus(VectorSource(bot$chat))
    ####### PRE PROCESSING #######
    chatscripts_bot<- tm_map(bot_corpus,tolower)
    chatscripts_bot<- tm_map(chatscripts_bot,removePunctuation)
    chatscripts_bot<- tm_map(chatscripts_bot,removeWords,c(stopwords("SMART"),"back","months","month","back"))
    chatscripts_bot<- tm_map(chatscripts_bot,removeNumbers)
    chatscripts_bot<- tm_map(chatscripts_bot,stripWhitespace)
    
    #### DOCUMENT TERM MATRIX ####
    dtm_bot4<- DocumentTermMatrix(chatscripts_bot,control = list(weighting = function(p) weightTfIdf(p,normalize = F),stopwords=T))
    freq_bot4<- colSums(as.matrix(dtm_bot4))
    
    #### More than words with frequency of 50
    word_subset4<- subset(freq_bot4,freq_bot4 >= 20)
    ## SORTING
    sort4<- order(word_subset4,decreasing = TRUE)
    df17<- data.frame(words = names(word_subset4), freq = word_subset4)
    sort_df4<- df17[order(df17$freq,decreasing = T),]
    barplot(word_subset4[1:20],las= 3,col = rainbow(20))
  })
  
  #########################################################################################################
  
  output$plot5<- renderPlot({
    input$plot5buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    ################ VISITOR CONVERSATION ###################################
    visitor_convo<- data.frame(grep(unstru_data$V1,pattern = "Visitor (.*)",value = T)) %>% set_colnames(c("chat"))
    visitor_convo<- visitor_convo %>% separate(chat, c("A", "B"),": ",fill = "left")
    visitor_convo<- data.frame(visitor_convo[,2]) %>% set_colnames(c("chat"))
    
    visitor_convo$chat<- as.character(visitor_convo$chat)
    chatscripts<- Corpus(VectorSource(visitor_convo$chat))
    #data cleansing
    chatscripts_1<- tm_map(chatscripts,tolower)
    chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
    chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
    chatscripts_1<- tm_map(chatscripts_1,removeWords,c(stopwords("english"),"mail","visitor","ananya","give","send","mounica","patel"))
    chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
    chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)
    ### DTM
    dtmr1<- DocumentTermMatrix(chatscripts_1,control = list(wordLengths = c(3,15)))
    
    rowTotals<-apply(dtmr1,1,sum)
    dtm.new<-dtmr1[rowTotals>0,]
    
    ################ LDA ##################
    lda<-LDA(dtm.new,input$nogroups)
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
    
  })
  #########################################################################################################
  
  output$plot6<- renderPlot({
    input$plot6buton
    full_data2<- data2()
    unstru_data<- data.frame(full_data2)
    
    ################ VISITOR CONVERSATION ###################################
    visitor_convo<- data.frame(grep(unstru_data$V1,pattern = "Visitor (.*)",value = T)) %>% set_colnames(c("chat"))
    visitor_convo<- visitor_convo %>% separate(chat, c("A", "B"),": ",fill = "left")
    visitor_convo<- data.frame(visitor_convo[,2]) %>% set_colnames(c("chat"))
    
    visitor_convo$chat<- as.character(visitor_convo$chat)
    chatscripts<- Corpus(VectorSource(visitor_convo$chat))
    #data cleansing
    chatscripts_1<- tm_map(chatscripts,tolower)
    chatscripts_1<- tm_map(chatscripts_1,removePunctuation)
    chatscripts_1<- tm_map(chatscripts_1,removeWords,stopwords("SMART"))
    chatscripts_1<- tm_map(chatscripts_1,removeWords,c(stopwords("english"),"mail","visitor","ananya","give","send","mounica","patel"))
    chatscripts_1<- tm_map(chatscripts_1,removeNumbers)
    chatscripts_1<- tm_map(chatscripts_1,stripWhitespace)
    ### DTM
    dtmr4<- DocumentTermMatrix(chatscripts_1,control = list(wordLengths = c(3,15)))
    
    rowTotals<-apply(dtmr4,1,sum)
    dtm.new<-dtmr4[rowTotals>0,]
    
    ################ CTM ##################
    ctm<- CTM(dtm.new,input$no1groups)
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
  })
  }

