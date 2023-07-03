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

header <- dashboardHeader(title = "Wordle Solver")

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
    guess_val <- strsplit(input$word1_guess, "")[[1]]
    print(guess_val)
    
    test <- word_checker(session, target_val, guess_val)
    print(test)
    
    if (counter$countervalue==0){
      change_letter_col(session, test[1,1], "l1_w1")
      change_letter_col(session, test[2,1], "l2_w1")
      change_letter_col(session, test[3,1], "l3_w1")
      change_letter_col(session, test[4,1], "l4_w1")
      change_letter_col(session, test[5,1], "l5_w1")
      updateButton(session, "l1_w1", label = toupper(str_sub(input$word1_guess, 1, 1)))
      updateButton(session, "l2_w1", label = toupper(str_sub(input$word1_guess, 2, 2)))
      updateButton(session, "l3_w1", label = toupper(str_sub(input$word1_guess, 3, 3)))
      updateButton(session, "l4_w1", label = toupper(str_sub(input$word1_guess, 4, 4)))
      updateButton(session, "l5_w1", label = toupper(str_sub(input$word1_guess, 5, 5)))
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    else if (counter$countervalue==1){
      change_letter_col(session, test[1,1], "l1_w2")
      change_letter_col(session, test[2,1], "l2_w2")
      change_letter_col(session, test[3,1], "l3_w2")
      change_letter_col(session, test[4,1], "l4_w2")
      change_letter_col(session, test[5,1], "l5_w2")
      updateButton(session, "l1_w2", label = toupper(str_sub(input$word1_guess, 1, 1)))
      updateButton(session, "l2_w2", label = toupper(str_sub(input$word1_guess, 2, 2)))
      updateButton(session, "l3_w2", label = toupper(str_sub(input$word1_guess, 3, 3)))
      updateButton(session, "l4_w2", label = toupper(str_sub(input$word1_guess, 4, 4)))
      updateButton(session, "l5_w2", label = toupper(str_sub(input$word1_guess, 5, 5)))
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    else if (counter$countervalue==2){
      change_letter_col(session, test[1,1], "l1_w3")
      change_letter_col(session, test[2,1], "l2_w3")
      change_letter_col(session, test[3,1], "l3_w3")
      change_letter_col(session, test[4,1], "l4_w3")
      change_letter_col(session, test[5,1], "l5_w3")
      updateButton(session, "l1_w3", label = toupper(str_sub(input$word1_guess, 1, 1)))
      updateButton(session, "l2_w3", label = toupper(str_sub(input$word1_guess, 2, 2)))
      updateButton(session, "l3_w3", label = toupper(str_sub(input$word1_guess, 3, 3)))
      updateButton(session, "l4_w3", label = toupper(str_sub(input$word1_guess, 4, 4)))
      updateButton(session, "l5_w3", label = toupper(str_sub(input$word1_guess, 5, 5)))
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    else if (counter$countervalue==3){
      change_letter_col(session, test[1,1], "l1_w4")
      change_letter_col(session, test[2,1], "l2_w4")
      change_letter_col(session, test[3,1], "l3_w4")
      change_letter_col(session, test[4,1], "l4_w4")
      change_letter_col(session, test[5,1], "l5_w4")
      updateButton(session, "l1_w4", label = toupper(str_sub(input$word1_guess, 1, 1)))
      updateButton(session, "l2_w4", label = toupper(str_sub(input$word1_guess, 2, 2)))
      updateButton(session, "l3_w4", label = toupper(str_sub(input$word1_guess, 3, 3)))
      updateButton(session, "l4_w4", label = toupper(str_sub(input$word1_guess, 4, 4)))
      updateButton(session, "l5_w4", label = toupper(str_sub(input$word1_guess, 5, 5)))
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    else if (counter$countervalue==4){
      change_letter_col(session, test[1,1], "l1_w5")
      change_letter_col(session, test[2,1], "l2_w5")
      change_letter_col(session, test[3,1], "l3_w5")
      change_letter_col(session, test[4,1], "l4_w5")
      change_letter_col(session, test[5,1], "l5_w5")
      updateButton(session, "l1_w5", label = toupper(str_sub(input$word1_guess, 1, 1)))
      updateButton(session, "l2_w5", label = toupper(str_sub(input$word1_guess, 2, 2)))
      updateButton(session, "l3_w5", label = toupper(str_sub(input$word1_guess, 3, 3)))
      updateButton(session, "l4_w5", label = toupper(str_sub(input$word1_guess, 4, 4)))
      updateButton(session, "l5_w5", label = toupper(str_sub(input$word1_guess, 5, 5)))
      
      counter$countervalue <- counter$countervalue+1
      print(counter$countervalue)
    }
    
    print(target_val)
    print(counter$countervalue)
  })
  
  observeEvent(input$word1_guess,{
    if(nchar(input$word1_guess) != 5) {
      disable(id = "go1")
    }
    else {
      enable(id = "go1")
    }
  })
}


shinyApp(ui = ui, server = server)
