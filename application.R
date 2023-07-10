library(dplyr)
library(DT)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(words)
source("C:/My_RStudio/Workspace/R_hackathon/functions.R")

word_list <- words::words %>% 
  filter(word_length == 5) %>% 
  select(word)

header <- dashboardHeader(title = "Wordle")

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  useShinyjs(),
  div(
    class = "word-button",
    textInput(
      inputId = "word1_guess",
      label = "Enter your guess:",
      value = ""
    ) %>%
      tagAppendAttributes(class = "inline-element"),
    bsButton(inputId = "go1", "Lock Guess", class = "lockButton")
  ),
  letter_colour_ui("word1",1),
  br(),
  letter_colour_ui("word2",2),
  br(),
  letter_colour_ui("word3",3),
  br(),
  letter_colour_ui("word4",4),
  br(),
  letter_colour_ui("word5",5),
  br(),
  letter_colour_ui("word5",5)
)

ui <- dashboardPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  skin = "black",
  header = header,
  sidebar = sidebar,
  body = body
)

server <- function(input,output,session){
  
  counter <- reactiveValues(countervalue = 0)
  
  target <- sample(word_list$word,1)
  target_val <- strsplit(target, "")[[1]]
  
  observeEvent(input$go1, {
    if(nchar(input$word1_guess)!=5){
      showNotification("Error: Word must be exactly 5 letters")
    } else if(!(input$word1_guess %in% word_list$word)){
      showNotification("Error: Not a real word")
    } else {
      
    guess_val <- strsplit(input$word1_guess, "")[[1]]
    print(guess_val)
    
    word <- word_checker(session, target_val, guess_val)
    print(word)
    
    if (counter$countervalue==0){
      check_update_col(session, input, word, 1)
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    else if (counter$countervalue==1){
      check_update_col(session, input, word, 2)
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    else if (counter$countervalue==2){
      check_update_col(session, input, word, 3)
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    else if (counter$countervalue==3){
      check_update_col(session, input, word, 4)
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    else if (counter$countervalue==4){
      check_update_col(session, input, word, 5)
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    else if (counter$countervalue==5){
      check_update_col(session, input, word, 6)
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }}
    
    print(target_val)
    print(counter$countervalue)
  })
  
  
}


shinyApp(ui = ui, server = server)
