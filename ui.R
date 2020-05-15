library(quanteda)
library(tidyverse)
library(shinyjs)
library(ggplot2)

#data_ready<-FALSE

library(shiny)

options(shiny.maxRequestSize = 50*1024^2)

# Define UI for application that draws a histogram
shinyUI(
    
    fluidPage(

    # Application title
    titlePanel("Natural Language Processing"),
    h3("Predicting the Next Word"),
    helpText(a("Farid Tayari", href="mailto:farid.tayari@gmail.com")),
    br(),
    
    checkboxInput("upload_a_Corpus", "You can upload a new corpus"),
    conditionalPanel(condition = "input.upload_a_Corpus==true",
                     fileInput("file1", "",
                               multiple = FALSE,
                               accept = c("text",
                                          "text/plain",
                                          ".txt")),),
    br(),
    sliderInput("train_set_proportion",
                "Train set proportion",
                min = 0.01,
                max = 1,
                value = 0.05),
    
    
    br(),
    actionButton("create_ngram", "Load the data and set up the N-grams"),
    h3(""),
    br(),
    uiOutput("nText"),
    
    br(),
    # verbatimTextOutput("data_ready"),
    sidebarLayout(
        conditionalPanel(condition = "output.nText!=''",
        sidebarPanel(    

                     radioButtons("NLP_models_radio", "NLP models:",
                                  c("Katz Back-off Bigrams" = "BO_2gram",
                                    "Katz Back-off Trigrams" = "BO_3gram",
                                    "Katz Back-off 4-grams" = "BO_4gram",
                                    "Linear Interpolation Bigrams" = "LI_2gram",
                                    "Linear Interpolation Trigrams" = "LI_3gram",
                                    "Linear Interpolation 4-grams" = "LI_4gram"), 
                                     selected = "BO_2gram"),
                     
                     
                     textInput("Input_text", "Enter the text"),
                     
                     
                     conditionalPanel(condition = "input.NLP_models_radio=='BO_2gram'",
                                      h4("Gamma (Discount):"),
                                      sliderInput("weight_bigram",
                                                  "Bigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5)
                     ),
                     
                     
                     conditionalPanel(condition = "input.NLP_models_radio=='BO_3gram'",
                                      h4("Gamma (Discount):"),
                                      sliderInput("weight_bigram",
                                                  "Bigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5),
                                      sliderInput("weight_trigram",
                                                  "Trigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5)
                     ),
                         
                     conditionalPanel(condition = "input.NLP_models_radio=='BO_4gram'",
                                      h4("Gamma (Discount):"),
                                      sliderInput("weight_bigram",
                                                  "Bigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.2),
                                      sliderInput("weight_trigram",
                                                  "Trigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.4),
                                      sliderInput("weight_4gram",
                                                  "4gram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.4)
                     ),
                     conditionalPanel(condition = "input.NLP_models_radio=='LI_2gram'",
                                      h4("Weights:"),
                                      sliderInput("weight_unigram",
                                                  "Unigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.4),
                                      sliderInput("weight_bigram",
                                                  "Bigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.6)
                     ),
                     conditionalPanel(condition = "input.NLP_models_radio=='LI_3gram'",
                                      h4("Weights:"),
                                      sliderInput("weight_unigram",
                                                  "Unigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.2),
                                      sliderInput("weight_bigram",
                                                  "Bigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.4),
                                      sliderInput("weight_trigram",
                                                  "Trigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.4)
                     ),
                     conditionalPanel(condition = "input.NLP_models_radio=='LI_4gram'",
                                      h4("Weights:"),
                                      sliderInput("weight_unigram",
                                                  "Unigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.1),
                                      sliderInput("weight_bigram",
                                                  "Bigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.2),
                                      sliderInput("weight_trigram",
                                                  "Trigram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.3),
                                      sliderInput("weight_4gram",
                                                  "4gram",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.4)
                     ),
                     actionButton("predict_word", "Predict the next word"),
                     

        ),),# conditionalPanel

        # Show a plot of the generated distribution
        mainPanel(
            # verbatimTextOutput("nText"),
               plotOutput("word_plot"),
               
               h6("Use at least two words for Bigram, three words for Trigram, and four words for 4-gram models."),
               h6("This application is running on a public server, which doesn't support computational power required for higher train set proportions. Consequently, the accuracy of the predictions might be low.")
        )
    )
))
