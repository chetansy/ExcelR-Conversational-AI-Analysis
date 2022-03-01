library(tidytext)
library(dplyr)
library(magrittr)
library(tm)
library(tidyr)
library(topicmodels)
library(shinyBS)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(wordcloud2)
library(reshape2)

ui<- shinyUI ( navbarPage(
  theme = shinytheme("sandstone"),
  strong (
    tags$a(
        href = 'https://www.excelr.com/',
        img(src = "https://excelrcom.b-cdn.net/assets/media/general/logo_white_204.png", height = "30px"),)),
  
  
  tabPanel(h4(icon("accusoft"),"Project Backgroud"),
           
           mainPanel(
             h3(strong("ExcelR Convsersational Data"),style = "color: blue"),
             tags$br(),
             
             h4("In this era of Ai/DS, things are changing rapidly. Because of its horizontal nature, companies across the
globe are adopting this technology in a quest for efficiency and optimization.",
                tags$br(),
                tags$br(),
                
              h4("One such rapidly changing sector is conversational AI because of its 360 Degree advantage such as:"),
              h4("1.  24*7 Service"),
              h4("2.  Easy Connectivity"),   
              h4("3.  Unbiased Conversation"),  
              h4("4.  Reduced Human Dependency"),   
              h4("5.  Effective Connectivity"),
              tags$br(),
              
            h4("With the above discussed benefits these data points are also full of insights. So as a Data Scientist in this
project we are responsible for extracting all the actionable insights, helpful for business."),
            tags$br(),
            
            h3(strong("Objective :--"),style = "color: blue"),
            h4("Topic Mining & Exploratory Analysis for improving the Resource Allocation, Content Modification & Service Improvement.")
           ))
           
           
           ),
  
  tabPanel(h4(icon("500px"),"Data Summary"),
           sidebarLayout(
             sidebarPanel(
               column(10,fileInput("file1","Choose Structured ",multiple = F,accept = c(".csv"))),
               
               column(10,fileInput("file2","Choose Un-Structured ",multiple = F,accept = c(".csv"))),
              
             ),
             
             mainPanel(
               fluidRow(
                 h4("Structured Data"),
                 DT:: dataTableOutput("contents1"),
                 tags$hr(),
                 tags$hr(),
                 
                 h4("Unstructured Data"),
                 tags$hr(),
                 
                 tabsetPanel(
                   tabPanel(
                     h5("BOT"),
                     tags$br(),
                     DT:: dataTableOutput("contents2"),
                   ),
                   
                   tabPanel(
                     h5("VISITOR"),
                     tags$br(),
                     DT:: dataTableOutput("contents3"),
                     tags$hr(),
                   )),
                 
                 
               )
             )
           )),
  
  tabPanel(h4(icon("eye"),"Wordcloud"),
           tabsetPanel(
                    tabPanel(
                        h5("Visualization Using DTM"),
                        tags$hr(),
                
                              bsButton("plot1buton","Visitor Conversations"),
                                  splitLayout(
                                      addSpinner(wordcloud2Output("plot1"),spin = "circle",color="#0dc565"),
                                      addSpinner(plotOutput("plot1.1"),spin = "circle",color="#0dc565")),
               
                              bsButton("plot2buton","BOT Conversations"),
                
                                  splitLayout(
                                      addSpinner(wordcloud2Output("plot2"),spin = "circle",color="#0dc565"),
                                      addSpinner(plotOutput("plot2.1"),spin = "circle",color="#0dc565")),
                                    ),
             
                  tabPanel(
                        h5("Visualization Using TF-IDF"),
                        tags$hr(),
                          
                          bsButton("plot3buton","Visitor Conversations"),
                          splitLayout(
                            addSpinner(wordcloud2Output("plot3"),spin = "circle",color="#0dc565"),
                            addSpinner(plotOutput("plot3.1"),spin = "circle",color="#0dc565")),
                          
                          bsButton("plot4buton","BOT Conversations"),
                          
                          splitLayout(
                            addSpinner(wordcloud2Output("plot4"),spin = "circle",color="#0dc565"),
                            addSpinner(plotOutput("plot4.1"),spin = "circle",color="#0dc565")),
                        
                        )
                    )
                  ),
  
  tabPanel(h4(icon("autoprefixer"),"Topic Mining"),
           
           tabsetPanel(
             tabPanel(
               h5("Latent Dirichlet Allocation"),
               tags$hr(),
               sliderInput("nogroups","Number Of Topics",min= 1,max = 10,value = 5),
                 h5("LDA Using DTM"),
                 addSpinner(plotOutput("plot5",height = "500px"),spin = "circle",color="#0dc565"),
               ),
             
             tabPanel(
               h5("Co-related Topic Modelling"),
               tags$hr(),
               sliderInput("no1groups","Number Of Topics",min= 1,max = 10,value = 5),
                 h5("CTM Using DTM"),
                 addSpinner(plotOutput("plot6",height = "500px"),spin = "circle",color="#0dc565"),
             )
           )
           
           
           )
))

