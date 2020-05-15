#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # output$data_ready<-reactive({FALSE})
    output$nText <- renderText({""})

    # output$nText <- renderText({input$create_ngram})
    observeEvent(input$create_ngram,{
        
        if (input$create_ngram==1)  {
            
            #  #wd<-"D:/Coursera/Data Science/10 Data Science Capstone/R_directory/Final_project/"
            #  #setwd(wd)          
             sample_ratio <- input$train_set_proportion 
             withProgress(message = "Please be patient. This might take a few minutes.", min = 0, max = 1, value=0,
                       {
            incProgress(.1, message = "Loading the data")
                           
           if (input$upload_a_Corpus==TRUE & !is.null(input$file1$datapath)) {    
               sample_data <- readLines(input$file1$datapath,encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
               set.seed(1000)
               train_data <- sample(sample_data, round(length(sample_data)*sample_ratio,0))
           } else{

          # # 
          # files_exist<- (file.exists("en_US.blogs.txt") & file.exists("en_US.news.txt") &
          #                    file.exists("en_US.twitter.txt"))
          # # data source
          # # https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
          # if (!files_exist){
          #     incProgress(.1, message = "Downloading the data")
          #     temp <- tempfile()
          #     url="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
          #     download.file(url,temp)
          #     file_names<-c("final/en_US/en_US.blogs.txt","final/en_US/en_US.news.txt","final/en_US/en_US.twitter.txt")
          #     unzip(temp,file_names,overwrite = TRUE,exdir = ".", junkpaths=TRUE)
          # 
          #     #file.remove("data.zip")
          #     unlink(temp)
          #     #unlink("./data/data")
          # 
          # 
          # }
          # 
          # data_address_blogs<- "en_US.blogs.txt"
          # data_address_news<- "en_US.news.txt"
          # data_address_twitter<- "en_US.twitter.txt"
          # 
          # raw_blog_file2<-readLines(data_address_blogs, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
          # raw_news_file2<-readLines(data_address_news, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
          # raw_twitter_file2<-readLines(data_address_twitter, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
          # 
          # 
          # incProgress(.1, message = "Loading the data")
          # 
          # 
          # sample_ratio <- input$train_set_proportion
          # 
          # set.seed(1000)
          # sample_blogs <- sample(raw_blog_file2, round(length(raw_blog_file2)*sample_ratio,0))
          # sample_news <- sample(raw_news_file2, round(length(raw_news_file2)*sample_ratio,0))
          # sample_twitter <- sample(raw_twitter_file2, round(length(raw_twitter_file2)*sample_ratio,0))
          # train_data <- c(sample_blogs, sample_news, sample_twitter)
          # # write_lines(sample_data,"sample_data.txt")
          # rm(raw_blog_file2,raw_news_file2,raw_twitter_file2,sample_blogs,sample_news,sample_twitter)                
          # 
          # The sample is small to be able to run on the shiny server 
           sample_data <- readLines("sample_data.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
           set.seed(1000)
           train_data <- sample(sample_data, round(length(sample_data)*sample_ratio,0))
          }
           
                           
          #incProgress(.1,message = "Please be patient. This might take a few minutes.")
           incProgress(.1,message = "Tokenization and creating the n-grams")
          #corpus_data<-corpus(sample_data)
          token_data<-tokens(train_data, remove_punct = TRUE, remove_symbols = TRUE
                             ,remove_numbers = TRUE,remove_url = TRUE, split_hyphens = TRUE,what = "word")

          
          
          #unigram
          gram_1<-tokens_ngrams(token_data,1)
          gram_1_dfm<-dfm(gram_1,tolower = TRUE, case_insensitive = FALSE)
          gram_1_table<-data.frame(freq=colSums(gram_1_dfm))
          gram_1_table$word<-row.names(gram_1_table)
          row.names(gram_1_table)<-NULL
          gram_1_table<-gram_1_table[c(2,1)]


          incProgress(.2,message = "Creating the Bigram")


          #bigram
          gram_2<-tokens_ngrams(token_data,2)
          gram_2_dfm<-dfm(gram_2,tolower = TRUE, case_insensitive = FALSE)
          gram_2_table<-data.frame(freq=colSums(gram_2_dfm))
          gram_2_table$word<-row.names(gram_2_table)
          row.names(gram_2_table)<-NULL
          gram_2_table<-gram_2_table[c(2,1)]


          incProgress(.2,message = "Creating the Trigram")


          #trigram
          gram_3<-tokens_ngrams(token_data,3)
          gram_3_dfm<-dfm(gram_3,tolower = TRUE, case_insensitive = FALSE)
          gram_3_table<-data.frame(freq=colSums(gram_3_dfm))
          gram_3_table$word<-row.names(gram_3_table)
          row.names(gram_3_table)<-NULL
          gram_3_table<-gram_3_table[c(2,1)]


          incProgress(.2,message = "Creating the 4gram")


          #4_gram
          gram_4<-tokens_ngrams(token_data,4)
          gram_4_dfm<-dfm(gram_4,tolower = TRUE, case_insensitive = FALSE)
          gram_4_table<-data.frame(freq=colSums(gram_4_dfm))
          gram_4_table$word<-row.names(gram_4_table)
          row.names(gram_4_table)<-NULL
          gram_4_table<-gram_4_table[c(2,1)]


          ngram_table<<-list(gram_1_table,gram_2_table,gram_3_table,gram_4_table)
          rm(gram_1,gram_1_dfm,gram_1_table,gram_2,gram_2_dfm,gram_2_table
             ,gram_3,gram_3_dfm,gram_3_table,gram_4,gram_4_dfm,gram_4_table)

          
          
          #output$nText <- renderText({"Loading the data ...  Please be patient. This might take a few minutes."})
          output$nText <- renderUI({"Choose a model"})
          
          
          
          
          
          # output$data_ready<-reactive({TRUE})
          
          
            }) #withProgress
        }  # action button if (input$create_ngram==1)
        else {output$nText <- renderUI({"Refresh the page please!"})}
        
        
}) #observeEvent
    
    last_words<-function (input_data_frame){
        output_words=NULL
        output_words=sub("^.+_", "",input_data_frame[,1])
        return(output_words)
    }
    
    
    obs_ngram<-function(x_input,ngram_no){
        x_input=tolower(x_input)
        x_input=unlist(strsplit(x_input," ")) 
        #  x_input=gsub("[^A-Za-z0-9 ]","",x_input)
        obs_ngram_output=NULL
        #obs_ngram_output=data.frame(word=character(),freq=numeric())
        n=ngram_no  
        
        if (length(x_input)>=n & n>1) {
            ngram_table_input=ngram_table[[n]]
            x_input_ngram=paste(x_input[(length(x_input)-(n-2)):length(x_input)],collapse="_")
            pattern1=paste("^",x_input_ngram,"_",sep = "")
            obs_ngram_output=ngram_table_input[str_detect(ngram_table_input$word,pattern1),]
        } else {
            obs_ngram_output_ngram="text length is shorter than the n-gram"
        }
        if (n==1) {
            ngram_table_input=ngram_table[[n]]
            x_input_ngram=x_input[length(x_input)]
            obs_ngram_output=ngram_table_input[ngram_table_input$word==x_input_ngram,]
        }
        if (is.null(obs_ngram_output)) {obs_ngram_output=NULL} else{
            if (nrow(obs_ngram_output)==0)   {obs_ngram_output=NULL}}
        return(obs_ngram_output) 
    }
    
    unobs_ngram<-function(x_input,ngram_no,obs_ngram_words){
        x_input=tolower(x_input)
        x_input=unlist(strsplit(x_input," ")) 
        #  x_input=gsub("[^A-Za-z0-9 ]","",x_input)
        unobs_words=NULL
        y_output=data.frame(word=character(),freq=numeric())
        n=ngram_no
        
        if (length(x_input)>=n & n>1) {
            ngram_table_input=ngram_table[[n]]
            x_input_ngram=paste(x_input[(length(x_input)-(n-2)):length(x_input)],collapse="_")
            pattern1=paste("^",x_input_ngram,"_",sep = "")
            y_output=ngram_table_input[str_detect(ngram_table_input$word,pattern1),]
        } 
        if (n==1) {
            ngram_table_input=ngram_table[[n]]
            y_output=ngram_table_input
        }
        if (nrow(y_output)!=0) {
            unobs_words=last_words(y_output) [!(last_words(y_output) %in% obs_ngram_words)]
        }
        return(unobs_words) 
    }
    
    match_ngram<-function(x_input,ngram_no){
        x_input=tolower(x_input)
        x_input=unlist(strsplit(x_input," ")) 
        #  x_input=gsub("[^A-Za-z0-9 ]","",x_input)
        match_ngram_output=NULL
        n=ngram_no  
        if (length(x_input)>=n) {
            ngram_table_input=ngram_table[[n]]
            pattern1=paste(x_input[(length(x_input)-(n-1)):length(x_input)],collapse="_")
            match_ngram_output=ngram_table_input[ngram_table_input$word==pattern1,]
        } else {
            match_ngram_output_ngram="text length is shorter than the n-gram"
        }
        if (is.null(match_ngram_output)) {match_ngram_output=NULL} else{
            if (nrow(match_ngram_output)==0)   {match_ngram_output=NULL}}
        return(match_ngram_output) 
    }
    
    
    obs_bo_ngram<-function(x_input,ngram_no,unobs_ngram_words){
        x_input=tolower(x_input)
        x_input=unlist(strsplit(x_input," ")) 
        #  x_input=gsub("[^A-Za-z0-9 ]","",x_input)
        y_output=data.frame(word=character(),freq=numeric())
        n=ngram_no #n=2
        if (length(x_input)>=n & n>1) {
            ngram_table_input=ngram_table[[n]]
            pattern1=paste(x_input[(length(x_input)-(n-2)):length(x_input)],collapse="_")
            bo_ngram=paste(pattern1,unobs_ngram_words,sep="_")
        } else {bo_ngram_output=NULL}
        
        # if (length(x_input)>=n & n==1) {
        #     ngram_table_input=ngram_table[[n]]
        #     bo_ngram=unobs_ngram_words
        # } else {bo_ngram_output=NULL}
        
        bo_ngram_output=ngram_table_input[ngram_table_input$word %in% bo_ngram,]
        return(bo_ngram_output)
    }
    
    unobs_bo_ngram<-function(x_input,ngram_no,unobs_ngram_words,obs_bo_ngram){
        x_input=tolower(x_input)
        x_input=unlist(strsplit(x_input," ")) 
        #  x_input=gsub("[^A-Za-z0-9 ]","",x_input)
        y_output=data.frame(word=character(),freq=numeric())
        n=ngram_no #n=2
        if (length(x_input)>=n & n>1) {
            # ngram_table_input=ngram_table[[n]]
            pattern1=paste(x_input[(length(x_input)-(n-2)):length(x_input)],collapse="_")
            bo_ngram=paste(pattern1,unobs_ngram_words,sep="_")
        } else {bo_ngram_output=NULL}
        
        # if (length(x_input)>=n & n==1) {
        #     # ngram_table_input=ngram_table[[n+1]]
        #     pattern1=x_input[length(x_input)]
        #     bo_ngram=paste(pattern1,unobs_ngram_words,sep="_")
        # } else {bo_ngram_output=NULL}
        
        bo_ngram_output=bo_ngram[!(bo_ngram %in% obs_bo_ngram$word)]
        return(bo_ngram_output)
    }
    
    
    # Discounting: Katz Back-off model
    Katz_Back_Off_2gram<-function(x,gamma){
        obs_2gram<-obs_ngram(x,ngram_no=2)
        obs_1gram<-obs_ngram(x,ngram_no=1)
        
        match_1gram<-match_ngram(x,ngram_no=1)
        
        obs_2gram_words<-NULL
        if (!is.null(obs_2gram)) {
            obs_2gram_words<-last_words(obs_2gram)
            unobs_2gram_words<-unobs_ngram(x,ngram_no=1, obs_2gram_words)
        } else {return(NULL)}
        
        alpha_uni<-1-sum(obs_2gram$freq-0.5)/obs_1gram$freq
        
        obs_bo_2gram<-obs_2gram
        unobs_bo_2gram<-unobs_2gram_words
        # unobs_bo_2gram<-str_split_fixed(unobs_bo_2gram, "_", 2)[, 2]
        
        obs_bo_2gram$prob<-(obs_bo_2gram$freq-0.5)/match_1gram$freq
        
        unigram<-ngram_table[[1]]
        unobs_bo_2gram<-unigram[unigram$word %in% unobs_bo_2gram,]
        unobs_bo_2gram$prob<-unobs_bo_2gram$freq*alpha_uni/sum(unobs_bo_2gram$freq)
        bo_2gram<-rbind(obs_bo_2gram,unobs_bo_2gram)
        #  alpha_3gram<-1 - sum((obs_3gram$freq - gamma[3]) / match_2gram$freq[1])
        bo_2gram<-bo_2gram[order(bo_2gram$prob, decreasing = TRUE),]
        bo_2gram$pred<-unlist(str_split_fixed(bo_2gram$word, "_",2))[,2]
        return(head(bo_2gram[c("pred","prob")],5))
    }
    Katz_Back_Off_3gram<-function(x,gamma){
        withProgress(message = "Predicting the next word using Katz Back-off Trigram model", min = 0, max = 1, value=0,{
            obs_3gram<-obs_ngram(x,ngram_no=3)
            obs_2gram<-obs_ngram(x,ngram_no=2)
            
            incProgress(.2,message = "Processing ...")
            
            match_2gram<-match_ngram(x,ngram_no=2)
            match_1gram<-match_ngram(x,ngram_no=1)
            
            incProgress(.2,message = "Processing ...")
            
            obs_3gram_words<-NULL
            obs_2gram_words<-NULL
            if (!is.null(obs_3gram)) {
                obs_3gram_words<-last_words(obs_3gram)
                unobs_3gram_words<-unobs_ngram(x,ngram_no=1, obs_3gram_words)
            }else {return(NULL)}
            # if (!is.null(obs_2gram)) {obs_2gram_words<-last_words(obs_2gram)}
            # unobs_2gram_words<-unobs_ngram(x,ngram_no=1, obs_2gram_words)
            
            obs_bo_3gram<-obs_3gram
            obs_bo_3gram$prob<-(obs_bo_3gram$freq-gamma[3])/match_2gram$freq[1]
            
            incProgress(.2,message = "Processing ...")
            
            alphaBi<-1-sum(obs_2gram$freq-gamma[2])/match_1gram$freq
            
            obs_bo_2gram<-obs_bo_ngram(x,ngram_no=2,unobs_3gram_words)
            unobs_bo_2gram<-unobs_bo_ngram(x,ngram_no=2,unobs_3gram_words,obs_bo_2gram)
            unobs_bo_2gram<-str_split_fixed(unobs_bo_2gram, "_", 2)[, 2]
            
            obs_bo_2gram$prob<-(obs_bo_2gram$freq-gamma[2])/match_1gram$freq
            
            unigram<-ngram_table[[1]]
            unobs_bo_2gram<-unigram[unigram$word %in% unobs_bo_2gram,]
            unobs_bo_2gram$prob<-unobs_bo_2gram$freq*alphaBi/sum(unobs_bo_2gram$freq)
            bo_2gram<-rbind(obs_bo_2gram,unobs_bo_2gram)
            alpha_3gram<-1 - sum((obs_3gram$freq - gamma[3]) / match_2gram$freq[1])
            bo_2gram<-bo_2gram[order(bo_2gram$prob, decreasing = TRUE),]
            
            incProgress(.2,message = "Processing ...")
            
            unobs_bo_3gram<- bo_2gram
            unobs_bo_3gram$word<-paste(str_split(match_2gram$word[1], "_")[[1]][1], bo_2gram$word, sep="_")
            unobs_bo_3gram$prob<-alpha_3gram * bo_2gram$prob / sum(bo_2gram$prob)
            
            bo_3gram<-rbind(obs_bo_3gram,unobs_bo_3gram)
            bo_3gram<-bo_3gram[order(bo_3gram$prob, decreasing = TRUE),]
            bo_3gram$pred<-unlist(str_split_fixed(bo_3gram$word, "_",3))[,3]
            return(head(bo_3gram[c("pred","prob")],5))
        })
    }
    
    Katz_Back_Off_4gram<-function(x,gamma){
        withProgress(message = "Predicting the next word using Katz Back-off 4-gram model", min = 0, max = 1, value=0,{
            obs_4gram<-obs_ngram(x,ngram_no=4)
            obs_3gram<-obs_ngram(x,ngram_no=3)
            obs_2gram<-obs_ngram(x,ngram_no=2)
            
            match_3gram<-match_ngram(x,ngram_no=3)  
            match_2gram<-match_ngram(x,ngram_no=2)
            match_1gram<-match_ngram(x,ngram_no=1)
            
            incProgress(.2,message = "Processing ...")
            
            
            obs_4gram_words<-NULL  
            obs_3gram_words<-NULL
            obs_2gram_words<-NULL
            if (!is.null(obs_3gram)) {
                obs_3gram_words<-last_words(obs_3gram)
                unobs_3gram_words<-unobs_ngram(x,ngram_no=1, obs_3gram_words)
            }else {return(NULL)}
            
            if (!is.null(obs_4gram)) {
                obs_4gram_words<-last_words(obs_4gram)
                unobs_4gram_words<-unobs_ngram(x,ngram_no=1, obs_4gram_words)
            }else {return(NULL)}
            # if (!is.null(obs_2gram)) {obs_2gram_words<-last_words(obs_2gram)}
            # unobs_2gram_words<-unobs_ngram(x,ngram_no=1, obs_2gram_words)
            
            obs_bo_4gram<-obs_4gram
            obs_bo_4gram$prob<-(obs_bo_4gram$freq-gamma[4])/match_3gram$freq[1]
            
            incProgress(.2,message = "Processing ...")
            
            alphaBi <- 1-sum(obs_2gram$freq-gamma[2])/match_1gram$freq
            alphatri<- 1-sum(obs_3gram$freq-gamma[3])/match_2gram$freq
            
            
            # 
            # obs_bo_2gram<-obs_bo_ngram(x,ngram_no=2,unobs_3gram_words)
            # unobs_bo_2gram<-unobs_bo_ngram(x,ngram_no=2,unobs_3gram_words,obs_bo_2gram)
            # unobs_bo_2gram<-str_split_fixed(unobs_bo_2gram, "_", 2)[, 2]
            
            incProgress(.2,message = "Processing ...")            
            
            
            obs_bo_2gram<-obs_bo_ngram(x,ngram_no=2,unobs_3gram_words)
            unobs_bo_2gram<-unobs_bo_ngram(x,ngram_no=2,unobs_3gram_words,obs_bo_2gram)
            unobs_bo_2gram<-str_split_fixed(unobs_bo_2gram, "_", 2)[, 2]
            
            obs_bo_3gram<-obs_bo_ngram(x,ngram_no=3,unobs_4gram_words)
            unobs_bo_3gram<-unobs_bo_ngram(x,ngram_no=3,unobs_4gram_words,obs_bo_3gram)
            unobs_bo_3gram<-str_split_fixed(unobs_bo_3gram, "_", 2)[, 2]
            
            obs_bo_2gram$prob<-(obs_bo_2gram$freq-gamma[2])/match_1gram$freq
            obs_bo_3gram$prob<-(obs_bo_3gram$freq-gamma[3])/match_2gram$freq
            
            
            incProgress(.2,message = "Processing ...")
            
            
            unigram<-ngram_table[[1]]
            unobs_bo_2gram<-unigram[unigram$word %in% unobs_bo_2gram,]
            unobs_bo_2gram$prob<-unobs_bo_2gram$freq*alphaBi/sum(unobs_bo_2gram$freq)
            bo_2gram<-rbind(obs_bo_2gram,unobs_bo_2gram)
            
            alpha_3gram<-1 - sum((obs_3gram$freq - gamma[3]) / match_2gram$freq[1])
            bo_2gram<-bo_2gram[order(bo_2gram$prob, decreasing = TRUE),]
            
            unobs_bo_3gram<- bo_2gram
            unobs_bo_3gram$word<-paste(str_split(match_2gram$word[1], "_")[[1]][1], bo_2gram$word, sep="_")
            unobs_bo_3gram$prob<-alpha_3gram * bo_2gram$prob / sum(bo_2gram$prob)
            
            incProgress(.1,message = "Processing ...")
            
            
            # bigram<-ngram_table[[2]]
            # unobs_bo_3gram<-bigram[bigram$word %in% unobs_bo_3gram,]
            # unobs_bo_3gram$prob<-unobs_bo_3gram$freq*alphatri/sum(unobs_bo_3gram$freq)
            bo_3gram<-rbind(obs_bo_3gram,unobs_bo_3gram)
            
            alpha_4gram<-1 - sum((obs_4gram$freq - gamma[4]) / match_3gram$freq[1])
            bo_3gram<-bo_3gram[order(bo_3gram$prob, decreasing = TRUE),]  
            
            unobs_bo_4gram<- bo_3gram
            unobs_bo_4gram$word<-paste(str_split(match_3gram$word[1], "_")[[1]][1], bo_3gram$word, sep="_")
            unobs_bo_4gram$prob<-alpha_4gram * bo_3gram$prob / sum(bo_3gram$prob)
            
            # bo_3gram<-rbind(obs_bo_3gram,unobs_bo_3gram)
            
            incProgress(.1,message = "Processing ...")
            
            bo_4gram<-rbind(obs_bo_4gram,unobs_bo_4gram)
            bo_4gram<-bo_4gram[order(bo_4gram$prob, decreasing = TRUE),]
            bo_4gram$pred<-unlist(str_split_fixed(bo_4gram$word, "_",4))[,4]
            return(head(bo_4gram[c("pred","prob")],5))
        })
    }
    
    ### Linear Interpolation
    
    linear_Interpolation_2gram<-function(x, lambda){
        
        obs_2gram<-obs_ngram(x,ngram_no=2)
        match_1gram<-match_ngram(x,ngram_no=1)
        q_obs_2gram<-obs_2gram
        q_obs_2gram$w0<-unlist(str_split_fixed(q_obs_2gram$word, "_",2))[,2]
        q_obs_2gram$prob<-q_obs_2gram$freq/match_1gram$freq[1]
        
        q_obs_1gram<-ngram_table[[1]]
        q_obs_1gram$w0<-q_obs_1gram$word
        q_obs_1gram$prob<-q_obs_1gram$freq/sum(q_obs_1gram$freq)
        
        q_matrix<-merge(q_obs_2gram,q_obs_1gram, by="w0", all = TRUE)
        names(q_matrix)[names(q_matrix)=="word.x"]<-"word_2gram"
        names(q_matrix)[names(q_matrix)=="freq.x"]<-"freq_2gram"
        names(q_matrix)[names(q_matrix)=="prob.x"]<-"prob_2gram"
        names(q_matrix)[names(q_matrix)=="word.y"]<-"word_1gram"
        names(q_matrix)[names(q_matrix)=="freq.y"]<-"freq_1gram"
        names(q_matrix)[names(q_matrix)=="prob.y"]<-"prob_1gram"
        
        q_matrix$prob_2gram[is.na(q_matrix$prob_2gram)]<-0
        q_matrix$freq_2gram[is.na(q_matrix$freq_2gram)]<-0
        q_matrix$lambda_2gram<-lambda[2]
        q_matrix$lambda_1gram<-lambda[1]
        
        q_matrix$prob<-q_matrix$prob_2gram*q_matrix$lambda_2gram +
            q_matrix$prob_1gram*q_matrix$lambda_1gram
        q_matrix<-q_matrix[order(q_matrix$prob, decreasing = TRUE),]
        names(q_matrix)[names(q_matrix)=="w0"]="pred"
        return(head(q_matrix[c("pred","prob")],5))
    }
    
    
    linear_Interpolation_3gram<-function(x, lambda){
        
        withProgress(message = "Predicting the next word using Linear Interpolation Trigram model", min = 0, max = 1, value=0,{
        
            obs_3gram<-obs_ngram(x,ngram_no=3)
            if (is.null(obs_3gram)) {return(NULL)}
            match_2gram<-match_ngram(x,ngram_no=2)
            q_obs_3gram<-obs_3gram
            q_obs_3gram$w0<-unlist(str_split_fixed(q_obs_3gram$word, "_",3))[,3]
            q_obs_3gram$prob<-q_obs_3gram$freq/match_2gram$freq[1]
            
            incProgress(.2,message = "Processing ...")
            
            obs_2gram<-obs_ngram(x,ngram_no=2)
            match_1gram<-match_ngram(x,ngram_no=1)
            q_obs_2gram<-obs_2gram
            q_obs_2gram$w0<-unlist(str_split_fixed(q_obs_2gram$word, "_",2))[,2]
            q_obs_2gram$prob<-q_obs_2gram$freq/match_1gram$freq[1]
            
            q_obs_1gram<-ngram_table[[1]]
            q_obs_1gram$w0<-q_obs_1gram$word
            q_obs_1gram$prob<-q_obs_1gram$freq/sum(q_obs_1gram$freq)
            q_obs_1gram$lambda_1gram<-lambda[1]
            
            incProgress(.2,message = "Processing ...")
            
            q_matrix<-merge(q_obs_3gram,q_obs_2gram, by="w0", all = TRUE)
            q_matrix<-merge(q_matrix,q_obs_1gram, by="w0", all = TRUE)
            names(q_matrix)[names(q_matrix)=="word.x"]<-"word_3gram"
            names(q_matrix)[names(q_matrix)=="freq.x"]<-"freq_3gram"
            names(q_matrix)[names(q_matrix)=="prob.x"]<-"prob_3gram"
            names(q_matrix)[names(q_matrix)=="word.y"]<-"word_2gram"
            names(q_matrix)[names(q_matrix)=="freq.y"]<-"freq_2gram"
            names(q_matrix)[names(q_matrix)=="prob.y"]<-"prob_2gram"
            names(q_matrix)[names(q_matrix)=="word"]<-"word_1gram"
            names(q_matrix)[names(q_matrix)=="freq"]<-"freq_1gram"
            names(q_matrix)[names(q_matrix)=="prob"]<-"prob_1gram"
            
            incProgress(.2,message = "Processing ...")
            
            q_matrix$prob_3gram[is.na(q_matrix$prob_3gram)]<-0
            q_matrix$freq_3gram[is.na(q_matrix$freq_3gram)]<-0
            q_matrix$prob_2gram[is.na(q_matrix$prob_2gram)]<-0
            q_matrix$freq_2gram[is.na(q_matrix$freq_2gram)]<-0
            q_matrix$lambda_3gram<-lambda[3]
            q_matrix$lambda_2gram<-lambda[2]
            q_matrix$lambda_1gram<-lambda[1]
            
            incProgress(.2,message = "Processing ...")
            
            q_matrix$prob<-q_matrix$prob_3gram*q_matrix$lambda_3gram +
            q_matrix$prob_2gram*q_matrix$lambda_2gram +
            q_matrix$prob_1gram*q_matrix$lambda_1gram
            q_matrix<-q_matrix[order(q_matrix$prob, decreasing = TRUE),]
            names(q_matrix)[names(q_matrix)=="w0"]="pred"
            return(head(q_matrix[c("pred","prob")],5))
        })
        
    }
    
    linear_Interpolation_4gram<-function(x, lambda){
        
        withProgress(message = "Predicting the next word using Linear Interpolation 4-gram model", min = 0, max = 1, value=0,{
        
            obs_4gram<-obs_ngram(x,ngram_no=4)
            
            if (is.null(obs_4gram)) {return(NULL)}
            match_3gram<-match_ngram(x,ngram_no=3)
            q_obs_4gram<-obs_4gram
            if (is.null(q_obs_4gram)) {q_obs_4gram<-data.frame(word="",freq=0,prob=0)}
            q_obs_4gram$w0<-unlist(str_split_fixed(q_obs_4gram$word, "_",4))[,4]
            q_obs_4gram$prob<-q_obs_4gram$freq/match_3gram$freq[1]
            names(q_obs_4gram)[names(q_obs_4gram)=="word"]<-"word_4gram"
            names(q_obs_4gram)[names(q_obs_4gram)=="freq"]<-"freq_4gram"
            names(q_obs_4gram)[names(q_obs_4gram)=="prob"]<-"prob_4gram"
            
            incProgress(.2,message = "Processing ...")
            
            obs_3gram<-obs_ngram(x,ngram_no=3)
            match_2gram<-match_ngram(x,ngram_no=2)
            q_obs_3gram<-obs_3gram
            if (is.null(q_obs_3gram)) {q_obs_3gram<-data.frame(word="",freq=0,prob=0)}
            q_obs_3gram$w0<-unlist(str_split_fixed(q_obs_3gram$word, "_",3))[,3]
            q_obs_3gram$prob<-q_obs_3gram$freq/match_2gram$freq[1]
            names(q_obs_3gram)[names(q_obs_3gram)=="word"]<-"word_3gram"
            names(q_obs_3gram)[names(q_obs_3gram)=="freq"]<-"freq_3gram"
            names(q_obs_3gram)[names(q_obs_3gram)=="prob"]<-"prob_3gram"
            
            incProgress(.2,message = "Processing ...")
            
            obs_2gram<-obs_ngram(x,ngram_no=2)
            match_1gram<-match_ngram(x,ngram_no=1)
            q_obs_2gram<-obs_2gram
            if (is.null(q_obs_2gram)) {q_obs_2gram<-data.frame(word="",freq=0,prob=0)}
            q_obs_2gram$w0<-unlist(str_split_fixed(q_obs_2gram$word, "_",2))[,2]
            q_obs_2gram$prob<-q_obs_2gram$freq/match_1gram$freq[1]
            names(q_obs_2gram)[names(q_obs_2gram)=="word"]<-"word_2gram"
            names(q_obs_2gram)[names(q_obs_2gram)=="freq"]<-"freq_2gram"
            names(q_obs_2gram)[names(q_obs_2gram)=="prob"]<-"prob_2gram"
            
            incProgress(.2,message = "Processing ...")
            
            q_obs_1gram<-ngram_table[[1]]
            q_obs_1gram$w0<-q_obs_1gram$word
            q_obs_1gram$prob<-q_obs_1gram$freq/sum(q_obs_1gram$freq)
            q_obs_1gram$lambda_1gram<-lambda[1]
            names(q_obs_1gram)[names(q_obs_1gram)=="word"]<-"word_1gram"
            names(q_obs_1gram)[names(q_obs_1gram)=="freq"]<-"freq_1gram"
            names(q_obs_1gram)[names(q_obs_1gram)=="prob"]<-"prob_1gram"
            
            incProgress(.2,message = "Processing ...")
            
            q_matrix<-merge(q_obs_4gram,q_obs_3gram, by="w0", all = TRUE)
            q_matrix<-merge(q_matrix,q_obs_2gram, by="w0", all = TRUE)
            q_matrix<-merge(q_matrix,q_obs_1gram, by="w0", all = TRUE)
            
            q_matrix$prob_4gram[is.na(q_matrix$prob_4gram)]<-0
            q_matrix$freq_4gram[is.na(q_matrix$freq_4gram)]<-0
            q_matrix$prob_3gram[is.na(q_matrix$prob_3gram)]<-0
            q_matrix$freq_3gram[is.na(q_matrix$freq_3gram)]<-0
            q_matrix$prob_2gram[is.na(q_matrix$prob_2gram)]<-0
            q_matrix$freq_2gram[is.na(q_matrix$freq_2gram)]<-0
            q_matrix$lambda_4gram<-lambda[4]
            q_matrix$lambda_3gram<-lambda[3]
            q_matrix$lambda_2gram<-lambda[2]
            q_matrix$lambda_1gram<-lambda[1]
            
            incProgress(.2,message = "Processing ...")
            
            q_matrix$prob<-q_matrix$prob_4gram*q_matrix$lambda_4gram +
            q_matrix$prob_3gram*q_matrix$lambda_3gram +
            q_matrix$prob_2gram*q_matrix$lambda_2gram +
            q_matrix$prob_1gram*q_matrix$lambda_1gram
            q_matrix<-q_matrix[order(q_matrix$prob, decreasing = TRUE),]
            
            names(q_matrix)[names(q_matrix)=="w0"]="pred"
            return(head(q_matrix[c("pred","prob")],5))
        })
    }
    
    observeEvent(input$predict_word,{
        if (input$Input_text!=""){
        
            x<-input$Input_text
        
            if (input$NLP_models_radio == "BO_2gram"){
                #gamma<-c(0,0.5,0,0,0,0)
                gamma<-c(0,input$weight_bigram,0,0,0,0)
                predicted_words<-Katz_Back_Off_2gram(x,gamma)
                chart_title<-"Predicted words using Katz Back-off Bigrams model"                
            }
            
            if (input$NLP_models_radio == "BO_3gram"){
                #gamma<-c(0,0.5,0.5,0,0,0)
                gamma<-c(0,input$weight_bigram,input$weight_trigram,0,0,0)
                predicted_words<-Katz_Back_Off_3gram(x,gamma)
                chart_title<-"Predicted words using Katz Back-off Trigrams model"                
            }
            
            if (input$NLP_models_radio == "BO_4gram"){
                # gamma<-c(0,0.2,0.4,0.4,0,0)
                gamma<-c(0,input$weight_bigram,input$weight_trigram,input$weight_trigram,0,0)
                predicted_words<-Katz_Back_Off_4gram(x,gamma)
                chart_title<-"Predicted words using Katz Back-off 4grams model"
            }
        
            if (input$NLP_models_radio == "LI_2gram"){
                #lambda<-c(labmda2=0.6,labmda1=0.4)
                lambda<-c(input$weight_unigram,input$weight_bigram,input$weight_trigram)
                predicted_words<-linear_Interpolation_2gram(x,lambda)
                chart_title<-"Predicted words using Linear Interpolation Bigram model"
            }
            
            if (input$NLP_models_radio == "LI_3gram"){
                #lambda<-c(labmda3=0.2,labmda2=0.4,labmda1=0.4)
                lambda<-c(input$weight_unigram,input$weight_bigram,input$weight_trigram)
                predicted_words<-linear_Interpolation_3gram(x,lambda)
                chart_title<-"Predicted words using Linear Interpolation Trigram model"
            }
            
            if (input$NLP_models_radio == "LI_4gram"){
                #lambda<-c(labmda4=0.1,labmda3=0.2,labmda2=0.3,labmda1=0.4)
                lambda<-c(input$weight_unigram,input$weight_bigram,input$weight_trigram,input$weight_4gram)
                predicted_words<-linear_Interpolation_4gram(x,lambda)
                chart_title<-"Predicted words using Linear Interpolation 4-gram model"
            }
        if (!is.null(predicted_words)){
            output$word_plot <- renderPlot({
                ggplot(predicted_words, aes(x=prob, y=reorder(pred,prob))) + 
                    geom_bar(stat = "identity",color="blue", fill="steelblue", width=0.7) +
                    theme(axis.text.x = element_text(size = 15)) +
                    theme(axis.text.y = element_text(size = 15)) +
                    theme(axis.title.x = element_text(size = 20)) +
                    theme(axis.title.y = element_text(size = 20)) +
                    ggtitle(chart_title) +
                    theme(plot.title = element_text(size = 20, hjust=0.5)) +
                    xlab("Probability") + ylab("Word")
                
            }) 
        }
        }
    })
    gc()
})





