library(quanteda)
library(tidyverse)
library(ggplot2)

wd<-"D:/Coursera/Data Science/10 Data Science Capstone/R_directory/Final_project/"
setwd(wd)
data_address_blogs<- paste (wd, "data","en_US.blogs.txt",sep="/")
data_address_news<- paste (wd, "data","en_US.news.txt",sep="/")
data_address_twitter<- paste (wd, "data","en_US.twitter.txt",sep="/")

########### the new has Control-Z but not end of the file

#raw_text_file<-readtext(data_address_news)
raw_blog_file2<-readLines(data_address_blogs, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
raw_news_file2<-readLines(data_address_news, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
raw_twitter_file2<-readLines(data_address_twitter, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)




## blog only for now

sample_ratio <- 0.25

set.seed(1000)
sample_blogs <- sample(raw_blog_file2, round(length(raw_blog_file2)*sample_ratio,0))
sample_news <- sample(raw_news_file2, round(length(raw_news_file2)*sample_ratio,0))
sample_twitter <- sample(raw_twitter_file2, round(length(raw_twitter_file2)*sample_ratio,0))
sample_data <- c(sample_blogs, sample_news, sample_twitter)

rm(raw_blog_file2,raw_news_file2,raw_twitter_file2
   ,sample_blogs,sample_news,sample_twitter)

#corpus_data<-corpus(sample_data)
token_data<-tokens(sample_data, remove_punct = TRUE, remove_symbols = TRUE
               ,remove_numbers = TRUE,remove_url = TRUE, split_hyphens = TRUE,what = "word")
               #,remove_separators = TRUE ,what = "sentence")

#dfmat<-dfm(token_data,stem = TRUE,tolower = TRUE, case_insensitive = FALSE)
#dfmat_prop <- dfm_weight(dfmat, scheme  = "prop")
#print(dfmat_prop)

## no need to generate n_gram, we can generate realtime based on the word user types
#a <- kwic(token_data, pattern = (c('aaron*')))

#unigram
gram_1<-tokens_ngrams(token_data,1)
gram_1_dfm<-dfm(gram_1,tolower = TRUE, case_insensitive = FALSE)
gram_1_table<-data.frame(freq=colSums(gram_1_dfm))
gram_1_table$word<-row.names(gram_1_table)
row.names(gram_1_table)<-NULL
gram_1_table<-gram_1_table[c(2,1)]

#bigram
gram_2<-tokens_ngrams(token_data,2)
gram_2_dfm<-dfm(gram_2,tolower = TRUE, case_insensitive = FALSE)
gram_2_table<-data.frame(freq=colSums(gram_2_dfm))
gram_2_table$word<-row.names(gram_2_table)
row.names(gram_2_table)<-NULL
gram_2_table<-gram_2_table[c(2,1)]

#trigram
gram_3<-tokens_ngrams(token_data,3)
gram_3_dfm<-dfm(gram_3,tolower = TRUE, case_insensitive = FALSE)
gram_3_table<-data.frame(freq=colSums(gram_3_dfm))
gram_3_table$word<-row.names(gram_3_table)
row.names(gram_3_table)<-NULL
gram_3_table<-gram_3_table[c(2,1)]

#4_gram
gram_4<-tokens_ngrams(token_data,4)
gram_4_dfm<-dfm(gram_4,tolower = TRUE, case_insensitive = FALSE)
gram_4_table<-data.frame(freq=colSums(gram_4_dfm))
gram_4_table$word<-row.names(gram_4_table)
row.names(gram_4_table)<-NULL
gram_4_table<-gram_4_table[c(2,1)]

# #5_gram
# gram_5<-tokens_ngrams(token_data,5)
# gram_5_dfm<-dfm(gram_5,tolower = TRUE, case_insensitive = FALSE)
# gram_5_table<-data.frame(freq=colSums(gram_5_dfm))
# gram_5_table$word<-row.names(gram_5_table)
# row.names(gram_5_table)<-NULL
# gram_5_table<-gram_5_table[c(2,1)]
# 
# #6_gram
# gram_6<-tokens_ngrams(token_data,6)
# gram_6_dfm<-dfm(gram_6,tolower = TRUE, case_insensitive = FALSE)
# gram_6_table<-data.frame(freq=colSums(gram_6_dfm))
# gram_6_table$word<-row.names(gram_6_table)
# row.names(gram_6_table)<-NULL
# gram_6_table<-gram_6_table[c(2,1)]

# 
# gram_6_2<-tokens_ngrams(token_data,2:6)
# gram_6_2_dfm<-dfm(gram_6_2,stem = TRUE,tolower = TRUE, case_insensitive = FALSE)
# gram_6_2_table <- data.frame(freq=colSums(gram_6_2_dfm))
# gram_6_2_table$word<-row.names(gram_6_2_table)
# row.names(gram_6_2_table)<-NULL
# gram_6_table<-gram_6_2_table[c(2,1)]

ngram_table<-list(gram_1_table,gram_2_table,gram_3_table,gram_4_table)
                  #,gram_5_table,gram_6_table)
rm(gram_1,gram_1_dfm,gram_1_table
   ,gram_2,gram_2_dfm,gram_2_table
   ,gram_3,gram_3_dfm,gram_3_table
   ,gram_4,gram_4_dfm,gram_4_table)
   # ,gram_5,gram_5_dfm,gram_5_table
   # ,gram_6,gram_6_dfm,gram_6_table)

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
  #   ngram_table_input=ngram_table[[n]]
  #   bo_ngram=unobs_ngram_words
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
  #   # ngram_table_input=ngram_table[[n+1]]
  #   pattern1=x_input[length(x_input)]
  #   bo_ngram=paste(pattern1,unobs_ngram_words,sep="_")
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
  obs_3gram<-obs_ngram(x,ngram_no=3)
  obs_2gram<-obs_ngram(x,ngram_no=2)
  
  match_2gram<-match_ngram(x,ngram_no=2)
  match_1gram<-match_ngram(x,ngram_no=1)
  
  obs_3gram_words<-NULL
  obs_2gram_words<-NULL
  if (!is.null(obs_3gram)) {
    obs_3gram_words<-last_words(obs_3gram)
    unobs_3gram_words<-unobs_ngram(x,ngram_no=1, obs_3gram_words)
  } else {return(NULL)}
  # if (!is.null(obs_2gram)) {obs_2gram_words<-last_words(obs_2gram)}
  # unobs_2gram_words<-unobs_ngram(x,ngram_no=1, obs_2gram_words)
  
  obs_bo_3gram<-obs_3gram
  obs_bo_3gram$prob<-(obs_bo_3gram$freq-gamma[3])/match_2gram$freq[1]
  
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
  
  unobs_bo_3gram<- bo_2gram
  unobs_bo_3gram$word<-paste(str_split(match_2gram$word[1], "_")[[1]][1], bo_2gram$word, sep="_")
  unobs_bo_3gram$prob<-alpha_3gram * bo_2gram$prob / sum(bo_2gram$prob)
  
  bo_3gram<-rbind(obs_bo_3gram,unobs_bo_3gram)
  bo_3gram<-bo_3gram[order(bo_3gram$prob, decreasing = TRUE),]
  bo_3gram$pred<-unlist(str_split_fixed(bo_3gram$word, "_",3))[,3]
  return(head(bo_3gram[c("pred","prob")],5))
}

Katz_Back_Off_4gram<-function(x,gamma){
  obs_4gram<-obs_ngram(x,ngram_no=4)
  obs_3gram<-obs_ngram(x,ngram_no=3)
  obs_2gram<-obs_ngram(x,ngram_no=2)

  match_3gram<-match_ngram(x,ngram_no=3)  
  match_2gram<-match_ngram(x,ngram_no=2)
  match_1gram<-match_ngram(x,ngram_no=1)
  
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
  
  alphaBi <- 1-sum(obs_2gram$freq-gamma[2])/match_1gram$freq
  alphatri<- 1-sum(obs_3gram$freq-gamma[3])/match_2gram$freq
  

  # 
  # obs_bo_2gram<-obs_bo_ngram(x,ngram_no=2,unobs_3gram_words)
  # unobs_bo_2gram<-unobs_bo_ngram(x,ngram_no=2,unobs_3gram_words,obs_bo_2gram)
  # unobs_bo_2gram<-str_split_fixed(unobs_bo_2gram, "_", 2)[, 2]
  
  
  
    
  obs_bo_2gram<-obs_bo_ngram(x,ngram_no=2,unobs_3gram_words)
  unobs_bo_2gram<-unobs_bo_ngram(x,ngram_no=2,unobs_3gram_words,obs_bo_2gram)
  unobs_bo_2gram<-str_split_fixed(unobs_bo_2gram, "_", 2)[, 2]
  
  obs_bo_3gram<-obs_bo_ngram(x,ngram_no=3,unobs_4gram_words)
  unobs_bo_3gram<-unobs_bo_ngram(x,ngram_no=3,unobs_4gram_words,obs_bo_3gram)
  unobs_bo_3gram<-str_split_fixed(unobs_bo_3gram, "_", 2)[, 2]
  
  obs_bo_2gram$prob<-(obs_bo_2gram$freq-gamma[2])/match_1gram$freq
  obs_bo_3gram$prob<-(obs_bo_3gram$freq-gamma[3])/match_2gram$freq
  
  
  unigram<-ngram_table[[1]]
  unobs_bo_2gram<-unigram[unigram$word %in% unobs_bo_2gram,]
  unobs_bo_2gram$prob<-unobs_bo_2gram$freq*alphaBi/sum(unobs_bo_2gram$freq)
  bo_2gram<-rbind(obs_bo_2gram,unobs_bo_2gram)
  
  alpha_3gram<-1 - sum((obs_3gram$freq - gamma[3]) / match_2gram$freq[1])
  bo_2gram<-bo_2gram[order(bo_2gram$prob, decreasing = TRUE),]
  
  unobs_bo_3gram<- bo_2gram
  unobs_bo_3gram$word<-paste(str_split(match_2gram$word[1], "_")[[1]][1], bo_2gram$word, sep="_")
  unobs_bo_3gram$prob<-alpha_3gram * bo_2gram$prob / sum(bo_2gram$prob)
  
  
  
  
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
  
  
  bo_4gram<-rbind(obs_bo_4gram,unobs_bo_4gram)
  bo_4gram<-bo_4gram[order(bo_4gram$prob, decreasing = TRUE),]
  bo_4gram$pred<-unlist(str_split_fixed(bo_4gram$word, "_",4))[,4]
  return(head(bo_4gram[c("pred","prob")],5))
}

### Linear Interpolaiton

linear_interpolaiton_2gram<-function(x, lambda){

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
  return(head(q_matrix[c("pred","prob")],10))
}


linear_interpolaiton_3gram<-function(x, lambda){
  obs_3gram<-obs_ngram(x,ngram_no=3)
  if (is.null(obs_3gram)) {return(NULL)}
  match_2gram<-match_ngram(x,ngram_no=2)
  q_obs_3gram<-obs_3gram
  q_obs_3gram$w0<-unlist(str_split_fixed(q_obs_3gram$word, "_",3))[,3]
  q_obs_3gram$prob<-q_obs_3gram$freq/match_2gram$freq[1]
  
  obs_2gram<-obs_ngram(x,ngram_no=2)
  match_1gram<-match_ngram(x,ngram_no=1)
  q_obs_2gram<-obs_2gram
  q_obs_2gram$w0<-unlist(str_split_fixed(q_obs_2gram$word, "_",2))[,2]
  q_obs_2gram$prob<-q_obs_2gram$freq/match_1gram$freq[1]
  
  q_obs_1gram<-ngram_table[[1]]
  q_obs_1gram$w0<-q_obs_1gram$word
  q_obs_1gram$prob<-q_obs_1gram$freq/sum(q_obs_1gram$freq)
  q_obs_1gram$lambda_1gram<-lambda[1]
  
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
  
  q_matrix$prob_3gram[is.na(q_matrix$prob_3gram)]<-0
  q_matrix$freq_3gram[is.na(q_matrix$freq_3gram)]<-0
  q_matrix$prob_2gram[is.na(q_matrix$prob_2gram)]<-0
  q_matrix$freq_2gram[is.na(q_matrix$freq_2gram)]<-0
  q_matrix$lambda_3gram<-lambda[3]
  q_matrix$lambda_2gram<-lambda[2]
  q_matrix$lambda_1gram<-lambda[1]
  
  q_matrix$prob<-q_matrix$prob_3gram*q_matrix$lambda_3gram +
  q_matrix$prob_2gram*q_matrix$lambda_2gram +
  q_matrix$prob_1gram*q_matrix$lambda_1gram
  q_matrix<-q_matrix[order(q_matrix$prob, decreasing = TRUE),]
  names(q_matrix)[names(q_matrix)=="w0"]="pred"
  return(head(q_matrix[c("pred","prob")],10))
}

linear_interpolaiton_4gram<-function(x, lambda){
  
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
  
  obs_3gram<-obs_ngram(x,ngram_no=3)
  match_2gram<-match_ngram(x,ngram_no=2)
  q_obs_3gram<-obs_3gram
  if (is.null(q_obs_3gram)) {q_obs_3gram<-data.frame(word="",freq=0,prob=0)}
  q_obs_3gram$w0<-unlist(str_split_fixed(q_obs_3gram$word, "_",3))[,3]
  q_obs_3gram$prob<-q_obs_3gram$freq/match_2gram$freq[1]
  names(q_obs_3gram)[names(q_obs_3gram)=="word"]<-"word_3gram"
  names(q_obs_3gram)[names(q_obs_3gram)=="freq"]<-"freq_3gram"
  names(q_obs_3gram)[names(q_obs_3gram)=="prob"]<-"prob_3gram"
  
  obs_2gram<-obs_ngram(x,ngram_no=2)
  match_1gram<-match_ngram(x,ngram_no=1)
  q_obs_2gram<-obs_2gram
  if (is.null(q_obs_2gram)) {q_obs_2gram<-data.frame(word="",freq=0,prob=0)}
  q_obs_2gram$w0<-unlist(str_split_fixed(q_obs_2gram$word, "_",2))[,2]
  q_obs_2gram$prob<-q_obs_2gram$freq/match_1gram$freq[1]
  names(q_obs_2gram)[names(q_obs_2gram)=="word"]<-"word_2gram"
  names(q_obs_2gram)[names(q_obs_2gram)=="freq"]<-"freq_2gram"
  names(q_obs_2gram)[names(q_obs_2gram)=="prob"]<-"prob_2gram"
  
  q_obs_1gram<-ngram_table[[1]]
  q_obs_1gram$w0<-q_obs_1gram$word
  q_obs_1gram$prob<-q_obs_1gram$freq/sum(q_obs_1gram$freq)
  q_obs_1gram$lambda_1gram<-lambda[1]
  names(q_obs_1gram)[names(q_obs_1gram)=="word"]<-"word_1gram"
  names(q_obs_1gram)[names(q_obs_1gram)=="freq"]<-"freq_1gram"
  names(q_obs_1gram)[names(q_obs_1gram)=="prob"]<-"prob_1gram"
  
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
  
  q_matrix$prob<-q_matrix$prob_4gram*q_matrix$lambda_4gram +
  q_matrix$prob_3gram*q_matrix$lambda_3gram +
  q_matrix$prob_2gram*q_matrix$lambda_2gram +
  q_matrix$prob_1gram*q_matrix$lambda_1gram
  q_matrix<-q_matrix[order(q_matrix$prob, decreasing = TRUE),]
  
  names(q_matrix)[names(q_matrix)=="w0"]="pred"
  return(head(q_matrix[c("pred","prob")],10))
}

x<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of" 


gamma<-c(0,0.5,0.5,0,0,0)
predicted_words<-Katz_Back_Off_2gram(x,gamma)
chart_title<-"Predicted words using Katz Back-off Bigrams model" 


gamma<-c(0,0.5,0.5,0,0,0)
predicted_words<-Katz_Back_Off_3gram(x,gamma)
chart_title<-"Predicted words using Katz Back-off Trigrams model" 

gamma<-c(0,0.2,0.4,0.4,0,0)
predicted_words<-Katz_Back_Off_4gram(x,gamma)
chart_title<-"Predicted words using Katz Back-off 4-grams model" 


lambda<-c(labmda2=0.6,labmda1=0.4)
predicted_words<-linear_interpolaiton_2gram(x,lambda)
chart_title<-"Predicted words using Linear Interpolaiton Bigram model"

lambda<-c(labmda3=0.2,labmda2=0.4,labmda1=0.4)
predicted_words<-linear_interpolaiton_3gram(x,lambda)
chart_title<-"Predicted words using Linear Interpolaiton Trigram model"

lambda<-c(labmda4=0.1,labmda3=0.2,labmda2=0.3,labmda1=0.4)
predicted_words<-linear_interpolaiton_4gram(x,lambda)
chart_title<-"Predicted words using Linear Interpolaiton 4-gram model"



ggplot(predicted_words, aes(x=prob, y=reorder(pred,prob))) + 
  geom_bar(stat = "identity",color="blue", fill="steelblue", width=0.7) +
  theme(axis.text.x = element_text(size = 15)) +
  theme(axis.text.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  ggtitle(chart_title) +
  theme(plot.title = element_text(size = 20, hjust=0.5)) +
  xlab("Probability") + ylab("Word")
